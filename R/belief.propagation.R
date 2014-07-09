# proc.order <- function(node, from, adj)
# {
#   # Recursive method to compute order of the message passing in the upward step.
#   #
#   # node : current node
#   # from : (local) root
#   # adj  : adjacency matrix
#   neighbours <- setdiff(which(adj[node,] > 0), from)
#   
#   if (length(neighbours) > 0)
#   {
#     for (n in neighbours) {
#       proc.order(n, node, adj)
#       parents.list <<- c(parents.list, node)
#     }
#   }
#   
#   process.order <<- c(process.order, node)
# }
# 
# 
# compute.message <- function(pot, dp, vfrom, vto, node.sizes)
# {
#   # Compute message from one node to another:
#   # marginalize variables not in separator between two nodes.
#   #
#   # pot   : cpt to be marginalized
#   # dp    : dimensions of the potential (may not contain all of the variables
#   #         that have to be present in the clique, if this is performed in
#   #         the upward step)
#   # vfrom : variables in the sending clique
#   # vto   : variables in the receiving clique
#   # node.sizes : node sizes
#   
#   # separator is made of the shared variables between the two cliques
#   vars.msg <- c(unlist(vfrom))
#   sep      <- intersect(vars.msg, c(unlist(vto)))
#   dp       <- c(unlist(dp))
#   
#   # for all of the variables not in the separator, repeat marginalization
#   # shrinking the prob.table
#   for (var in setdiff(vars.msg, sep))
#   {
#     if (length(intersect(var, dp)))
#     {
#       msg  <- marginalize(pot, dp, var)
#       pot  <- msg$potential
#       dp   <- as.list(msg$vars)
#     }
#   }
#   
#   return(list("potential"=pot, "vars"=dp))
# }
# 
# 
# marginalize <- function(pot, vars, marg.var)
# {
#   # Marginalize a variable in a probability table.
#   #
#   # pot      : probability table
#   # vars     : variables associated to pot
#   # marg.var : variable to be marginalizes
#   
#   marg.dim <- which(unlist(vars) == marg.var)
#   
#   # get dimensions, compute dimensions for the soon-to-be-created prob. table
#   # and number of the values that it will contain
#   dims          <- dim(pot)
#   new.dims      <- c(dims[-marg.dim])
#   new.num.vals  <- prod(new.dims)
#   length.of.run <- c(dims[marg.dim])
#   num.runs      <- new.num.vals
#   
#   # switch dimensions in the array:
#   # dimension corresponding to the variable to be marginalized goes first
#   new.order <- c(marg.dim, (1:length(dims))[-marg.dim])
# 
#   # remove marginalized dimension name
#   new.order.names <- c(vars[-marg.dim])
#   
#   # switch dimensions, make prob.table  a linear array,
#   cpt  <- aperm(pot, new.order)
#   marg <- array(cpt)
#   
#   # the marginalization is now done by summing consecutive values
#   marg <- tapply(marg, rep(1:new.num.vals, each=length.of.run), sum)  
# 
#   # apply new dimensions to resulting list (if needed)
#   if (length(new.order.names) > 0)
#   {
#     marg <- array(marg, new.dims, c(unlist(new.order.names)))
#   }
#   
#   return(list("potential"=marg, "vars"=new.order.names))
# }
# 
# 
# mult <- function(cpt1, vars1, cpt2, vars2, node.sizes)
# {
#   # Multiply a cpt by another cpt.
#   # Returns a list containing the resulting cpt and the associated variables list.
#   #
#   # cpt1  : first cpt
#   # vars1 : variables associated to cpt1
#   # cpt2  : second cpt
#   # vars2 : variables associated to cpt2
#   # node.sizes : sizes of the nodes
#   
#   # clean format
#   vars1 <- c(unlist(vars1))
#   vars2 <- c(unlist(vars2))
#   
#   # If the variables associated to cpt1 are all contained in the list of variables for cpt2, but
#   # no variables of cpt2 is contained also in cpt1, swap the two cpts.
#   # Handles cases such as P(AB) x P(C|AB) ==> P(C|AB) x P(AB)
#   # Not really needed, but easier to understand.
#   if ((length(setdiff(vars1, vars2)) == 0 &&
#        length(setdiff(vars2, vars1)) > 0     )        
#   )
#   {
#     tmp   <- vars1
#     vars1 <- vars2
#     vars2 <- tmp
#     
#     tmp  <- cpt1
#     cpt1 <- cpt2
#     cpt2 <- tmp
#   }
#   
#   # For (my) simplicity, cpts are managed with the variables (and therefore dimensions) in ascending order.
#   # Check this requirement, and take action if it is not met.
#   out   <- sort.dimensions(cpt1, vars1)
#   cpt1  <- out$potential
#   vars1 <- c(unlist(out$vars))
# 
#   out   <- sort.dimensions(cpt2, vars2)
#   cpt2  <- out$potential
#   vars2 <- c(unlist(out$vars))
#   
#   # Proper multiplication starts here.
#   # It works like this:
#   # - look for the common variablesin vars1 and vars2;
#   common.vars <- c(intersect(vars1, vars2))
#   common1 <- match  (vars1, common.vars)
#   common1 <- common1[!is.na(common1)]
#   common2 <- match  (vars2, common.vars)
#   common2 <- common2[!is.na(common2)]
#   
#   # - if the cpts share no common variables, we can multiply them with an outer product;
#   if (length(common.vars) == 0)
#   {
#     cpt1 <- cpt1 %o% cpt2
#   }
#   else
#   # otherwise, we have to manage the shared variables: consider P(C|A) x P(AB); unlisting the cpts we obtain
#   # [ac !ac a!c !a!c], and
#   # [ab !ab a!b !a!b]
#   # (remember we have ordered the dimensions in ascending order). We have to handle the shared A and compute
#   # ac   x ab   = acb
#   # ac   x a!b  = ac!b
#   # !ac  x !ab  = !acb
#   # !ac  x !a!b = !ac!b
#   # a!c  x ab   = a!cb
#   # a!c  x a!b  = a!c!b
#   # !a!c x !ab  = !a!cb
#   # !a!c x !a!b = !a!c!b
#   # (we will then have to reorder dimensions).
#   # In order to do this, we permute dimensions for the two cpts, by putting the shared variables
#   # as first dimensions of cpt1 and last dimensions of cpt2; then, we consecutively repeat every cell
#   # of cpt1, and the entire sequence of cells of cpt2, the proper number of times in order
#   # to reach the final number of elements (the ``proper number'' is the product of the size of
#   # the variables not shared among the two cpt2); then we can finally compute the
#   # element-wise product of cpt1 and cpt2;
#   {
#     if (length(vars1) > 1)
#     {
#       new.order <- c(c(c(1:length(vars1))[common1]),
#                      c(c(1:length(vars1))[-common1]))
#       cpt1      <- aperm(cpt1, new.order)
#       vars1     <- vars1[new.order]
#     }
#     
#     # [a b c] ==> [a a b b c c]
#     cpt1  <- c(sapply(c(cpt1),
#                       function(x){
#                         rep(x, 
#                             prod(node.sizes[vars2[-common2]])
#                             )
#                       }
#                ))
# 
#     if(length(vars2) > 1)
#     {
#       new.order <- c(c(c(1:length(vars2))[-common2]),
#                      c(c(1:length(vars2))[common2]))
#       cpt2      <- aperm(cpt2, new.order)
#       vars2     <- vars2[new.order]
#     }
#     
#     # [a b c] ==> [a b c a b c]
#     cpt2  <- c(rep(c(unlist(cpt2)),
#                    prod(
#                      node.sizes[vars1[-common1]]
#                    )
#              ))
#     
#     # - point-wise product
#     cpt1 <- c(unlist(cpt1)) * c(unlist(cpt2))
#     
#   }
# 
# 
#   # - compute variables for the resulting cpt; if there were no shared variables, then
#   #   it suffices to concatenate 
#   if (length(intersect(unlist(vars1),unlist(vars2))) > 0)
#   {
#     new.where <- which(vars2 == common.vars)
#     vars1     <- as.list(c(c(unlist(vars2)[-new.where]),
#                            c(unlist(vars1))))
#   }
#   else
#   {
#     #vars1 <- as.list(rev(c(unlist(vars2)), (c(unlist(vars1)))))
#     vars1 <- as.list(c(c(
#                rev(c(vars1)),
#                (c(vars2))))
#              )
#   }
# 
#   cpt1 <- array(c(cpt1), c(node.sizes[unlist(vars1)]))
#   
#   out  <- sort.dimensions(cpt1, vars1)
# 
#   return(list("potential"=out$potential, "vars"=out$vars))
# }
# 
# 
# divide <- function(cpt1, vars1, cpt2, vars2, node.sizes)
# {
#   # Divide a cpt by another cpt.
#   # Returns a list containing the resulting cpt and the associated variables list.
#   # cpt1  : dividend cpt
#   # vars1 : variables associated to cpt1
#   # cpt2  : divisor cpt
#   # vars2 : variables associated to cpt2
#   # node.sizes : sizes of the nodes
#   
#   # clean format
#   vars1 <- c(unlist(vars1))
#   vars2 <- c(unlist(vars2))
#   
#   # If the variables associated to cpt1 are all contained in the list of variables for cpt2, but
#   # no variables of cpt2 is contained also in cpt1, swap the two cpts.
#   # Handles cases such as P(AB) / P(C|AB) ==> P(C|AB) / P(AB)
#   if ((length(setdiff(vars1, vars2)) == 0 &&
#          length(setdiff(vars2, vars1)) > 0     )        
#   )
#   {
#     tmp   <- vars1
#     vars1 <- vars2
#     vars2 <- tmp
#     
#     tmp  <- cpt1
#     cpt1 <- cpt2
#     cpt2 <- tmp
#   }
#   
#   # For (my) simplicity, cpts are managed with the variables (and therefore dimensions) in ascending order.
#   # Check this requirement, and take action if it is not met.
#   out   <- sort.dimensions(cpt1, vars1)
#   cpt1  <- out$potential
#   vars1 <- c(unlist(out$vars))
#   
#   out   <- sort.dimensions(cpt2, vars2)
#   cpt2  <- out$potential
#   vars2 <- c(unlist(out$vars))
#   
#   
#   # The proper division starts here.
#   # It works like this:
#   # - domain of the divisor is entirely contained into the one of the dividend;
#   # - look for the common variables (all of the variables in vars2, some of them in vars1);
#   common.vars <- c(intersect(vars1, vars2))
#   common1 <- match(vars1, common.vars)
#   common1 <- common1[!is.na(common1)]
# 
#   # - permute array dimensions for cpt1 putting the common variables in the first dimensions;
#   if (length(vars1) > 1)
#   {
#     cpt1 <- aperm(cpt1, c(c(c(1:length(vars1))[common1]),
#                           c(c(1:length(vars1))[-common1])
#     ))
#     vars1 <- c(vars1[c(c(1:length(vars1))[common1])],
#                vars1[c(c(1:length(vars1))[-common1])])
#   }
#   
#   # - unlist cpt2 and repeat it as many times as needed (product of cardinality
#   #   of non-common variables of cpt1);
#   cpt2      <- c(rep(c(unlist(cpt2)),
#                      prod(
#                        node.sizes[vars1[-common1]]
#                      )
#                 ))
# 
#   # - now, every cell of cpt1 is paired with a cell of cpt2 whose variables
#   #   have the same setting of it;
#   # - perform element-wise division, handling the 0/0 case (conventionally set to 0 too);
#   cpt1 <- sapply(1:length(unlist(cpt2)),
#                 function(x) {
#                   if(cpt2[x] == 0) {
#                     return(0)
#                   } else {
#                     return(unlist(cpt1)[x] / unlist(cpt2)[x])
#                   }
#                 })
# 
#   # - rebuild array with corresponding dimensions, and permute dimensions to reconstruct order
#   cpt1 <- array(c(cpt1), c(node.sizes[unlist(vars1)]))
#   out  <- sort.dimensions(cpt1, vars1)
#   
#   return(list("potential"=out$potential, "vars"=out$vars))
# }
# 
# sort.dimensions <- function(cpt, vars)
# {
#   # Permute array dimensions of the cpt accoring to the dimension names.
#   # Dimensions (each corresponding to a variable) will be sorted in numerical order.
#   # cpt  : conditional probability table
#   # vars : dimension names
#   
#   new.ordering <- c(unlist(vars))
#   if (length(new.ordering) > 1)
#   {
#     # look if there is some variable out of order (preceding a variable with a lower number)
#     is.ordered <- TRUE
#     for (i in 1:(length(new.ordering)-1))
#     {
#       if (new.ordering[i] >= new.ordering[i+1])
#       {
#         is.ordered <- FALSE
#         break
#       }
#     }
#     
#     if (!is.ordered) # permute
#     {
#       dd           <- data.frame(c1 = c(new.ordering),
#                                  c2 = c(1:length(new.ordering)))
#       dd           <- dd[with(dd,order(c1)),]
#       new.ordering <- new.ordering[c(dd[,"c2"])]
#       cpt          <- aperm(cpt, c(dd[,"c2"]))
#       vars         <- as.list(new.ordering)
#     }
#   }
#   return(list("potential"=cpt, "vars"=vars))
# }
# 
# 
# bp.query <- function(potentials, cliques, query.var)
# {
#   clique <- which.min(lapply(1:length(cliques),
#                               function(x){
#                                 length(
#                                   which(unlist(
#                                     is.element(
#                                       query.var,
#                                       unlist(cliques[[x]])
#                                     )
#                                   ) == FALSE) == 0)
#                                 }
#   ))
#   
#   pot  <- potentials[[clique]]
#   vars <-c(unlist(cliques[[clique]]))
#   
#   for (var in setdiff(vars, c(unlist(query.var))))
#   {
#     marg  <- marginalize(pot, vars, var)
#     pot   <- marg$potential
#     vars  <- marg$vars
#   }
#   return(pot)
# }
