belief.propagation <- function(graph, ctree, orig.cliques, cpts, dim.vars, observed.vars, observed.vals, node.sizes)
{
  # Takes a junction tree and the conditional probabilities of the original BN,
  # and compute the belief propagation.
  # If needed, insert the observed values.
  #
  # graph         : adjacency matrix
  # ctree         : clique tree as computed by junction.tree()
  # orig.cliques  : list of cliques
  # cpts          : list of conditional probability tables
  # dim.vars      : list of variables contained in the cliques of the clique tree
  # observed.vars : list of observed variables
  # observed.vals : list of observed values, w.r.t. observed.vars
  # node.sizes    : node sizes
   
  num.nodes  <- nrow(graph)
  num.cliqs  <- length(orig.cliques)
  
  # cliques contains the variables that compose each clique
  cliques    <- orig.cliques
  
  # potentials is a list containing the probability tables of each clique
  potentials <- as.list(rep(as.list(c(1)), num.cliqs))
  
  # dimensions.contained contains the variables that effectively compose the cpt
  # currently contained in each node of the clique tree.
  # After last round it will match corresponding clique.
  dimensions.contained <- NULL
  for (i in 1:num.cliqs)
    dimensions.contained[[i]] <- as.list(c(NULL))

  # choose as root (one among) the clique(s) whose connected edges have the highest overall sum
  root <- which.max(rowSums(ctree))
  
  # Assign factors to a cluster graph
  # Construct initial potentials:
  # initial potentials are conditional or joint probability tables, depending on the initial BN
  # and how we assign each CPT to the cliques.
  #
  # The probabilities are stored as multidimensional arrays, with the convention that
  # the variables in the tables are in alphanumerical order.
  # E.g.: the network A -> C <- D -> B, whose junction tree has two nodes (and tables) ACD and BD.
  #
  # We construct a table for a clique this way:
  # - start with an empty table (NULL)
  # - whenever a {C,J}PT is assigned to that clique, its variables are ordered
  # - then, we control if the variables of the table we're inserting are already present in the clique table:
  #   - if no, we can multiply the two tables, ensuring the variables are ordered after that
  #   - if yes, we multiply the two tables
  # If the clique is currently empty, just add the cpt.
  # We have, however, to maintain the order of the variables in the probability table.
  
  for (cpt in 1:num.nodes)
  {
    # find a suitable clique for the CPT of node `cpt` (a clique that contains node `cpt` and all of its parents)
    target.clique <- which.min(lapply(1:num.cliqs,
                                      function(x){
                                        length(
                                          which(unlist(
                                            is.element(
                                              unlist(dim.vars[[cpt]]),
                                              unlist(cliques[[x]])
                                            )
                                          ) == FALSE) == 0)
                                      }
                                ))

    # get the variables currently contained in the selected clique
    ds <- c(unlist(dimensions.contained[[target.clique]]))
    
    if (length(ds) == 0)
    {
      # if current clique is empty, just insert the cpt
      out <- sort.dimensions(cpts[[cpt]], dim.vars[[cpt]])
      potentials[[target.clique]]           <- out$potential
      dimensions.contained[[target.clique]] <- out$vars
    }
    else
    {
      # multiply current prob. table for the already inserted prob. table
      out <- mult(potentials[[target.clique]],
                  dimensions.contained[[target.clique]],
                  cpts[[cpt]],
                  dim.vars[[cpt]],
                  node.sizes)
      potentials[[target.clique]]           <- out$potential
      dimensions.contained[[target.clique]] <- out$vars
    }
  }

  
  # INCORPORATE EVIDENCE
  # If there are any observed variables, insert the knowledge.
  # Each observation is inserted by setting to zero all of the combinations that
  # do not match the observation. This is done this way:
  # - for each observed variable, permute the dimensions of the cpt in order to
  #   put the observed variable as last dimension.
  #   This way, if the cpt is unrolled, the values of the obs.var. will be contiguous.;
  # - we can now zero entire runs of consecutive values;
  # - finally we restore the original order of the dimensions by repeating
  #   the permutation the correct number of times. 
  #   TODO restore order using sort.dimensions()
  
  if (length(observed.vars) > 0)
  {
    observed.vars <- c(unlist(observed.vars))
    for (var in 1:length(unlist(observed.vars)))
    {
      # look for one clique containing the variable
      target.clique <- which.min(lapply(1:num.cliqs,
                                        function(x) {
                                          which(is.element(
                                            unlist(dimensions.contained[[x]]),
                                            unlist(observed.vars)[var]
                                          ) == TRUE)
                                        }
                                  ))
      # construct new order for aperm()
      num.of.vars <- length(unlist(dimensions.contained[[target.clique]]))
      position    <- which(unlist(dimensions.contained[[target.clique]]) == observed.vars[var])
      new.order   <- c(c(1:num.of.vars)[-position], position)
      how.many.repeats <- length(which(new.order-c(1:num.of.vars) != 0)) - 1 # -1 as the first is the aperm() we're computing now
      potentials[[target.clique]] <- aperm(potentials[[target.clique]], new.order)
  
      # set to zero entries corresponding to non-observed values
      num.vals <- node.sizes[observed.vars[var]]
      step     <- prod(node.sizes[c(unlist(dimensions.contained[[target.clique]]))[1:(length(dimensions.contained[[target.clique]]))-1]])
      
      for (i in 1:num.vals)
      {
        if (i != unlist(observed.vals)[var])
        {
          potentials[[target.clique]][(i-1)*step + (1:step)] <- 0
        }
      }
      
      # restore order (if needed)
      # IIRC there should be some permutation algebra result for this
      if (how.many.repeats > 0)
      {
        for (i in 1:how.many.repeats)
        {
          potentials[[target.clique]] <- aperm(potentials[[target.clique]], new.order)
        }
      }
    }
  }

  
  # compute processing order from leaves to root
  process.order <<- c()
  parents.list  <<- c()
  proc.order(root, c(), ctree)
  #   print(process.order)
  #   print(parents.list)

  
  # MESSAGE PASSING FROM LEAVES TO ROOT
  
  # msg.pots contains the prob.tables for the messages,
  # while msg.vars contains the corresponding variables
  msg.pots <- NULL
  msg.vars <- NULL
  for (i in 1:num.cliqs)
  {
    msg.pots[[i]] <- as.list(c(NULL))
    msg.vars[[i]] <- as.list(c(NULL))
  }

  # For each clique (excluding the root) compute the message by marginalizing
  # the variables not in the separator, then store the message and multiply it
  # for the cpt contained in the neighbour clique, overwriting the corresponding
  # potential and associated variables.
  for (clique in 1:(num.cliqs-1))
  {
    out <- compute.message(potentials[[process.order[clique]]],
                           dimensions.contained[[process.order[clique]]],
                           cliques[[process.order[clique]]],
                           cliques[[parents.list[clique]]],
                           node.sizes)
    msg.pots[[process.order[clique]]] <- out$potential
    msg.vars[[process.order[clique]]] <- out$vars
    
    out <- mult(potentials[[parents.list[clique]]],
                dimensions.contained[[parents.list[clique]]],
                msg.pots[[process.order[clique]]],
                msg.vars[[process.order[clique]]],
                node.sizes)
    potentials[[parents.list[clique]]]           <- out$potential
    dimensions.contained[[parents.list[clique]]] <- out$vars
  }

  # Upward step is thus completed. Now go backward from root to leaves.
  # This step is done by taking the CPT of the root node and dividing it (for each child)
  # by the message received from the corresponding child, then marginalize the variables
  # not in the separator and pass the new nessage to the child. As the messages computed
  # in the upward step are not needed anymore after the division, they can be overwritten.
  # Then multiply the cpt of the child for the message computed, and iterate by treating
  # each (internal) node as root.
  for (clique in (num.cliqs-1):1)
  {
    out <- divide(potentials[[parents.list[clique]]],
                  dimensions.contained[[parents.list[clique]]],
                  msg.pots[[process.order[clique]]],
                  msg.vars[[process.order[clique]]],
                  node.sizes)
    msg.pots[[process.order[clique]]] <- out$potential
    msg.vars[[process.order[clique]]] <- as.list(out$vars)

    out  <- compute.message(msg.pots[[process.order[clique]]],
                            msg.vars[[process.order[clique]]],
                            cliques[[parents.list[clique]]],
                            cliques[[process.order[clique]]],
                            node.sizes
                        )
    msg.pots[[process.order[clique]]] <- out$potential
    msg.vars[[process.order[clique]]] <- out$vars

    out <- mult(potentials[[process.order[clique]]],
                dimensions.contained[[process.order[clique]]],
                msg.pots[[process.order[clique]]],
                msg.vars[[process.order[clique]]],
                node.sizes)
    potentials[[process.order[clique]]] <- out$potential
    dimensions.contained[[process.order[clique]]] <- out$vars
  }

  # Finally return the potentials computed (will be all JPTs).
  return(potentials)
}


proc.order <- function(node, from, adj)
{
  # Recursive method to compute order of the message passing in the upward step.
  #
  # node : current node
  # from : (local) root
  # adj  : adjacency matrix
  neighbours <- setdiff(which(adj[node,] > 0), from)
  
  if (length(neighbours) > 0)
  {
    for (n in neighbours) {
      proc.order(n, node, adj)
      parents.list <<- c(parents.list, node)
    }
  }
  
  process.order <<- c(process.order, node)
}


compute.message <- function(pot, dp, vfrom, vto, node.sizes)
{
  # Compute message from one node to another:
  # marginalize variables not in separator between two nodes.
  #
  # pot   : cpt to be marginalized
  # dp    : dimensions of the potential (may not contain all of the variables
  #         that have to be present in the clique, if this is performed in
  #         the upward step)
  # vfrom : variables in the sending clique
  # vto   : variables in the receiving clique
  # node.sizes : node sizes
  
  # separator is made of the shared variables between the two cliques
  vars.msg <- c(unlist(vfrom))
  sep      <- intersect(vars.msg, c(unlist(vto)))
  dp       <- c(unlist(dp))
  
  # for all of the variables not in the separator, repeat marginalization
  # shrinking the prob.table
  for (var in setdiff(vars.msg, sep))
  {
    if (length(intersect(var, dp)))
    {
      msg  <- marginalize(pot, dp, var)
      pot  <- msg$potential
      dp   <- as.list(msg$vars)
    }
  }
  
  return(list("potential"=pot, "vars"=dp))
}


marginalize <- function(pot, vars, marg.var)
{
  # Marginalize a variable in a probability table.
  #
  # pot      : probability table
  # vars     : variables associated to pot
  # marg.var : variable to be marginalizes
  
  marg.dim <- which(unlist(vars) == marg.var)
  
  # get dimensions, compute dimensions for the soon-to-be-created prob. table
  # and number of the values that it will contain
  dims          <- dim(pot)
  new.dims      <- c(dims[-marg.dim])
  new.num.vals  <- prod(new.dims)
  length.of.run <- c(dims[marg.dim])
  num.runs      <- new.num.vals
  
  # switch dimensions in the array:
  # dimension corresponding to the variable to be marginalized goes first
  new.order <- c(marg.dim, (1:length(dims))[-marg.dim])

  # remove marginalized dimension name
  new.order.names <- c(vars[-marg.dim])
  
  # switch dimensions, make prob.table  a linear array,
  cpt  <- aperm(pot, new.order)
  marg <- array(cpt)
  
  # the marginalization is now done by summing consecutive values
  marg <- tapply(marg, rep(1:new.num.vals, each=length.of.run), sum)  

  # apply new dimensions to resulting list (if needed)
  if (length(new.order.names) > 0)
  {
    marg <- array(marg, new.dims, c(unlist(new.order.names)))
  }
  
  return(list("potential"=marg, "vars"=new.order.names))
}


mult <- function(cpt1, vars1, cpt2, vars2, node.sizes)
{
  # Multiply a cpt by another cpt.
  # Returns a list containing the resulting cpt and the associated variables list.
  #
  # cpt1  : first cpt
  # vars1 : variables associated to cpt1
  # cpt2  : second cpt
  # vars2 : variables associated to cpt2
  # node.sizes : sizes of the nodes
  
  # clean format
  vars1 <- c(unlist(vars1))
  vars2 <- c(unlist(vars2))
  
  # If the variables associated to cpt1 are all contained in the list of variables for cpt2, but
  # no variables of cpt2 is contained also in cpt1, swap the two cpts.
  # Handles cases such as P(AB) x P(C|AB) ==> P(C|AB) x P(AB)
  # Not really needed, but easier to understand.
  if ((length(setdiff(vars1, vars2)) == 0 &&
       length(setdiff(vars2, vars1)) > 0     )        
  )
  {
    tmp   <- vars1
    vars1 <- vars2
    vars2 <- tmp
    
    tmp  <- cpt1
    cpt1 <- cpt2
    cpt2 <- tmp
  }
  
  # For (my) simplicity, cpts are managed with the variables (and therefore dimensions) in ascending order.
  # Check this requirement, and take action if it is not met.
  out   <- sort.dimensions(cpt1, vars1)
  cpt1  <- out$potential
  vars1 <- c(unlist(out$vars))

  out   <- sort.dimensions(cpt2, vars2)
  cpt2  <- out$potential
  vars2 <- c(unlist(out$vars))
  
  # Proper multiplication starts here.
  # It works like this:
  # - look for the common variablesin vars1 and vars2;
  common.vars <- c(intersect(vars1, vars2))
  common1 <- match  (vars1, common.vars)
  common1 <- common1[!is.na(common1)]
  common2 <- match  (vars2, common.vars)
  common2 <- common2[!is.na(common2)]
  
  # - if the cpts share no common variables, we can multiply them with an outer product;
  if (length(common.vars) == 0)
  {
    cpt1 <- cpt1 %o% cpt2
  }
  else
  # otherwise, we have to manage the shared variables: consider P(C|A) x P(AB); unlisting the cpts we obtain
  # [ac !ac a!c !a!c], and
  # [ab !ab a!b !a!b]
  # (remember we have ordered the dimensions in ascending order). We have to handle the shared A and compute
  # ac   x ab   = acb
  # ac   x a!b  = ac!b
  # !ac  x !ab  = !acb
  # !ac  x !a!b = !ac!b
  # a!c  x ab   = a!cb
  # a!c  x a!b  = a!c!b
  # !a!c x !ab  = !a!cb
  # !a!c x !a!b = !a!c!b
  # (we will then have to reorder dimensions).
  # In order to do this, we permute dimensions for the two cpts, by putting the shared variables
  # as first dimensions of cpt1 and last dimensions of cpt2; then, we consecutively repeat every cell
  # of cpt1, and the entire sequence of cells of cpt2, the proper number of times in order
  # to reach the final number of elements (the ``proper number'' is the product of the size of
  # the variables not shared among the two cpt2); then we can finally compute the
  # element-wise product of cpt1 and cpt2;
  {
    if (length(vars1) > 1)
    {
      new.order <- c(c(c(1:length(vars1))[common1]),
                     c(c(1:length(vars1))[-common1]))
      cpt1      <- aperm(cpt1, new.order)
      vars1     <- vars1[new.order]
    }
    
    # [a b c] ==> [a a b b c c]
    cpt1  <- c(sapply(c(cpt1),
                      function(x){
                        rep(x, 
                            prod(node.sizes[vars2[-common2]])
                            )
                      }
               ))

    if(length(vars2) > 1)
    {
      new.order <- c(c(c(1:length(vars2))[-common2]),
                     c(c(1:length(vars2))[common2]))
      cpt2      <- aperm(cpt2, new.order)
      vars2     <- vars2[new.order]
    }
    
    # [a b c] ==> [a b c a b c]
    cpt2  <- c(rep(c(unlist(cpt2)),
                   prod(
                     node.sizes[vars1[-common1]]
                   )
             ))
    
    # - point-wise product
    cpt1 <- c(unlist(cpt1)) * c(unlist(cpt2))
    
  }


  # - compute variables for the resulting cpt; if there were no shared variables, then
  #   it suffices to concatenate 
  if (length(intersect(unlist(vars1),unlist(vars2))) > 0)
  {
    new.where <- which(vars2 == common.vars)
    vars1     <- as.list(c(c(unlist(vars2)[-new.where]),
                           c(unlist(vars1))))
  }
  else
  {
    #vars1 <- as.list(rev(c(unlist(vars2)), (c(unlist(vars1)))))
    vars1 <- as.list(c(c(
               rev(c(vars1)),
               (c(vars2))))
             )
  }

  cpt1 <- array(c(cpt1), c(node.sizes[unlist(vars1)]))
  
  out  <- sort.dimensions(cpt1, vars1)

  return(list("potential"=out$potential, "vars"=out$vars))
}


divide <- function(cpt1, vars1, cpt2, vars2, node.sizes)
{
  # Divide a cpt by another cpt.
  # Returns a list containing the resulting cpt and the associated variables list.
  # cpt1  : dividend cpt
  # vars1 : variables associated to cpt1
  # cpt2  : divisor cpt
  # vars2 : variables associated to cpt2
  # node.sizes : sizes of the nodes
  
  # clean format
  vars1 <- c(unlist(vars1))
  vars2 <- c(unlist(vars2))
  
  # If the variables associated to cpt1 are all contained in the list of variables for cpt2, but
  # no variables of cpt2 is contained also in cpt1, swap the two cpts.
  # Handles cases such as P(AB) / P(C|AB) ==> P(C|AB) / P(AB)
  if ((length(setdiff(vars1, vars2)) == 0 &&
         length(setdiff(vars2, vars1)) > 0     )        
  )
  {
    tmp   <- vars1
    vars1 <- vars2
    vars2 <- tmp
    
    tmp  <- cpt1
    cpt1 <- cpt2
    cpt2 <- tmp
  }
  
  # For (my) simplicity, cpts are managed with the variables (and therefore dimensions) in ascending order.
  # Check this requirement, and take action if it is not met.
  out   <- sort.dimensions(cpt1, vars1)
  cpt1  <- out$potential
  vars1 <- c(unlist(out$vars))
  
  out   <- sort.dimensions(cpt2, vars2)
  cpt2  <- out$potential
  vars2 <- c(unlist(out$vars))
  
  
  # The proper division starts here.
  # It works like this:
  # - domain of the divisor is entirely contained into the one of the dividend;
  # - look for the common variables (all of the variables in vars2, some of them in vars1);
  common.vars <- c(intersect(vars1, vars2))
  common1 <- match(vars1, common.vars)
  common1 <- common1[!is.na(common1)]

  # - permute array dimensions for cpt1 putting the common variables in the first dimensions;
  if (length(vars1) > 1)
  {
    cpt1 <- aperm(cpt1, c(c(c(1:length(vars1))[common1]),
                          c(c(1:length(vars1))[-common1])
    ))
    vars1 <- c(vars1[c(c(1:length(vars1))[common1])],
               vars1[c(c(1:length(vars1))[-common1])])
  }
  
  # - unlist cpt2 and repeat it as many times as needed (product of cardinality
  #   of non-common variables of cpt1);
  cpt2      <- c(rep(c(unlist(cpt2)),
                     prod(
                       node.sizes[vars1[-common1]]
                     )
                ))

  # - now, every cell of cpt1 is paired with a cell of cpt2 whose variables
  #   have the same setting of it;
  # - perform element-wise division, handling the 0/0 case (conventionally set to 0 too);
  cpt1 <- sapply(1:length(unlist(cpt2)),
                function(x) {
                  if(cpt2[x] == 0) {
                    return(0)
                  } else {
                    return(unlist(cpt1)[x] / unlist(cpt2)[x])
                  }
                })

  # - rebuild array with corresponding dimensions, and permute dimensions to reconstruct order
  cpt1 <- array(c(cpt1), c(node.sizes[unlist(vars1)]))
  out  <- sort.dimensions(cpt1, vars1)
  
  return(list("potential"=out$potential, "vars"=out$vars))
}

sort.dimensions <- function(cpt, vars)
{
  # Permute array dimensions of the cpt accoring to the dimension names.
  # Dimensions (each corresponding to a variable) will be sorted in numerical order.
  # cpt  : conditional probability table
  # vars : dimension names
  
  new.ordering <- c(unlist(vars))
  if (length(new.ordering) > 1)
  {
    # look if there is some variable out of order (preceding a variable with a lower number)
    is.ordered <- TRUE
    for (i in 1:(length(new.ordering)-1))
    {
      if (new.ordering[i] >= new.ordering[i+1])
      {
        is.ordered <- FALSE
        break
      }
    }
    
    if (!is.ordered) # permute
    {
      dd           <- data.frame(c1 = c(new.ordering),
                                 c2 = c(1:length(new.ordering)))
      dd           <- dd[with(dd,order(c1)),]
      new.ordering <- new.ordering[c(dd[,"c2"])]
      cpt          <- aperm(cpt, c(dd[,"c2"]))
      vars         <- as.list(new.ordering)
    }
  }
  return(list("potential"=cpt, "vars"=vars))
}


bp.query <- function(potentials, cliques, query.var)
{
  clique <- which.min(lapply(1:length(cliques),
                              function(x){
                                length(
                                  which(unlist(
                                    is.element(
                                      query.var,
                                      unlist(cliques[[x]])
                                    )
                                  ) == FALSE) == 0)
                                }
  ))
  
  pot  <- potentials[[clique]]
  vars <-c(unlist(cliques[[clique]]))
  
  for (var in setdiff(vars, c(unlist(query.var))))
  {
    marg  <- marginalize(pot, vars, var)
    pot   <- marg$potential
    vars  <- marg$vars
  }
  return(pot)
}
