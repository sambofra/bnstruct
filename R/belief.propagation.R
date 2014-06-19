belief.propagation <- function(graph, ctree, orig.cliques, cpts, dim.vars, observed.vars, observed.vals, node.sizes)
{
  # graph         : adjacency matrix
  # ctree         : clique tree as computed by junction.tree()
  # orig.cliques  : list of cliques
  # cpts          : list of conditional probability tables
  # dim.vars      : list of variables contained in the cliques of the clique tree
  # observed.vars : list of observed variables
  # observed.vals : list of observed values, w.r.t. observed.vars
  # node.sizes    : node sizes
  
  cliques    <- orig.cliques
  num.nodes  <- nrow(graph)
  num.cliqs  <- length(cliques)
  messages   <- rep(as.list(c(1)), num.cliqs)
  potentials <- as.list(rep(as.list(c(1)), num.cliqs))
  
  root <- which.max(rowSums(ctree))
  print(root)
  
  dimensions.contained <- NULL
  for (i in 1:num.cliqs)
    dimensions.contained[[i]] <- NULL

  #print("------------------")
  #print(ctree)
  #print(cliques)
  #print(length(cliques))
  
  # Assign factors to a cluster graph
  # Construct initial potentials:
  # initial potentials are conditional or joint probability tables, depending on the initial BN
  # and how we assign each CPT to the cliques.
  # The probabilities are stored as multidimensional arrays, with the convention that
  # the variables in the tables are in alphanumerical order.
  # E.g.: the network A -> C <- D -> B, whose junction tree has two nodes (and tables) ACD and BD.
  # We construct a table for a clique this way:
  # - start with an empty table (NULL)
  # - whenever a {C,J}PT is assigned to that clique, its variables are ordered
  # - then, we control if the variables of the table we're inserting are already present in the clique table:
  #   - if no, we can multiply the two tables, ensuring the variables are ordered after that
  #   - if yes, this means that we are constructing a JPT from a CPT; say that we have to compute P(B|A) x P(A):
  #     - if the clique already contains P(A), then we're inserting P(B|A), and we have to transpose the table
  #       in order to have A in the first dimension and B in the second dimension, then to multiply P(A) for
  #       that transposed table;
  #     - if, instead, P(B|A) is already in the clique table, then we can just multiply it for P(A).
  
  print("##############################")
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
    #cat(cpt, " ", unlist(dim.vars[[cpt]]), " ", target.clique, " " ,cliques[[target.clique]] , "\n")
    
    ds <- c(dimensions.contained[[target.clique]])
    
    # Now we have to multiply the cpts in order to form the initial potential of the clique.
    # First, we check if the current initial potential and the cpt we're multiplying share some variable:
    # - if no, just multiply them;
    # - if yes, then we are multiplying a CPT for an unconditioned probability.
    # If the clique is currently empty, just add the cpt.
    # We have, however, to maintain the order of the variables in the probability table.

#     print("ds")
#     print(c(ds))
#     print("dim.vars")
#     print(c(dim.vars[[cpt]]))
#     print("length intersection")
#     print(length(intersect(c(unlist(ds)), c(unlist(dim.vars[[cpt]])))))
    
    print("@@@@@@@@@@@@@@")
    if (length(ds) == 0)
    {
      print("''''''")
      new.ordering <- c(unlist(dim.vars[[cpt]]))
      potentials[[target.clique]] <- cpts[[cpt]]
      if(length(dim.vars[[cpt]]) > 1)
      {
        dd <- data.frame(c1 = c(new.ordering), c2=c(1:length(new.ordering)))
        dd <- dd[with(dd,order(c1)),]
        new.ordering <- new.ordering[c(dd[,"c2"])]
        potentials[[target.clique]] <- aperm(potentials[[target.clique]], c(dd[,"c2"]))
      }
      dimensions.contained[[target.clique]] <- as.list(new.ordering)
    }
    else
#    {
#       if (length(intersect(c(unlist(ds)), c(unlist(dim.vars[[cpt]])))) == 0)
#       {
#         # no variables in common: ensure ordering, tnen multiply
#         new.ordering <- unlist(c(ds, c(dim.vars[[cpt]])))
#         
#         print("**************")
#         print(new.ordering)
#         if (length(new.ordering) > 1)
#         {
#           is.ordered <- TRUE
#           for (i in 1:(length(new.ordering)-1))
#           {
#             if (new.ordering[i] >= new.ordering[i+1])
#             {
#               is.ordered <- FALSE
#               break
#             }
#           }
#           if (!is.ordered) # permute
#           {
#             potentials[[target.clique]] <- potentials[[target.clique]] %o% cpts[[cpt]]
#             dd <- data.frame(c1 = c(new.ordering), c2=c(1:length(new.ordering)))
#             dd <- dd[with(dd,order(c1)),]
#             new.ordering <- new.ordering[c(dd[,"c2"])]
#             potentials[[target.clique]] <- aperm(potentials[[target.clique]], c(dd[,"c2"]))
#           }
#           else
#           {
#             potentials[[target.clique]] <- cpts[[cpt]] %o% potentials[[target.clique]]
#           }
#         }
#         print("new.ordering")
#         print(target.clique)
#         print(new.ordering)
#         dimensions.contained[[target.clique]] <- as.list(new.ordering)
#       }
#       else
    {
      print(dimensions.contained[[target.clique]])
      out <- mult(potentials[[target.clique]],
                  dimensions.contained[[target.clique]],
                  cpts[[cpt]], dim.vars[[cpt]], node.sizes)
      potentials[[target.clique]] <- out$cpt
      dimensions.contained[[target.clique]] <- as.list(out$vars)
      print(dimensions.contained[[target.clique]])
    }
  
  }
  print("---------------------------------------------------------")
  print(potentials)
  print(dimensions.contained)
  print(cliques)
  # fill in missing variables in the cpts
  # needed if separator variables among two cliques are given
  # by edges introduced during the triangulation step
  for (i in 1:num.cliqs)
  {
    sd <- setdiff(unlist(cliques[[i]]),unlist(dimensions.contained[[i]]))
    if (length(sd) > 0)
    {
      #print(sd)
      for (j in sd)
      {
        out <- mult(potentials[[i]], dimensions.contained[[i]],
                    array(c(rep(1.0,node.sizes[j]))), as.list(c(j)), node.sizes) #,c(1,node.sizes[j])
        #print(j)
        #print(out)
        #readLines(file("stdin"),1)
        potentials[[i]] <- out$cpt
        dimensions.contained[[i]] <- as.list(out$vars)
      }
    }
  }
  print(dimensions.contained)
  #break

  # INCORPORATE EVIDENCE
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
    if (how.many.repeats > 0)
    {
      for (i in 1:how.many.repeats)
      {
        potentials[[target.clique]] <- aperm(potentials[[target.clique]], new.order)
      }
    }
  }

  #print(potentials)
  
  # go from leaves to root
  # compute processing order
  process.order <<- c()
  parents.list  <<- c()
  proc.order(root, c(), ctree)
  leaves <- setdiff(process.order, parents.list)
  messages.dimensions <- cliques
  print(process.order)
  print(parents.list)
  print(leaves)
    
  print("######################")
  
  #break
  cliques <- dimensions.contained
  msg.pots <- potentials
  for (clique in 1:(num.cliqs-1))
  {
    out <- pass.message(process.order[clique], parents.list[clique], cliques, msg.pots, messages, leaves, node.sizes)
    msg.pots[[process.order[clique]]] <- out$potential
    #cliques <- out$cliques
    #dimensions.contained[[process.order]]
  }
  print("POTENTIALS")
  print(potentials)
  print("MESSAGES")
  print(msg.pots)
  
  # go from root to leaves
}

pass.message <- function(from, to, cliques, potentials, messages, leaves, node.sizes)
{
  print("PASS MESSAGE")
  sep <- intersect(unlist(cliques[[from]]), unlist(cliques[[to]]))
  print(from)
  print(to)
  print(cliques[[from]])
  print(cliques[[to]])
  print(sep)
#   print("--------------------------------")
#   print(unlist(cliques[c]))
#   print(unlist(cliques[d]))
#   print(sep)
  if (length(intersect(leaves, from)) == 0)
  {
    for (s in sep)
    {
#       pos  <- which(cliques[sep] == s)
#       posm <- which()
#       len  <- length(curr.cl)
#       dims <- dim(pot)
#       per  <- c(c(1:len)[-pos],pos)
#       pot  <- aperm(pot, per)
#       pot  <- pot %o% aperm()
#       pot  <- aperm(pot, per)
#       curr.cl <- curr.cl[-pos]
    }
  }
  print("°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°")
  print(cliques[[from]])
  print(unlist(cliques[[from]]))
  print(sep)
  print(setdiff(unlist(cliques[[from]]),sep))
  pot <- potentials[[from]]
  vars.msg <- cliques[[from]]
  for (var in setdiff(unlist(cliques[[from]]),sep))
  {
    msg <- marginalize(from, pot, vars.msg, var)
    print(msg)
    pot <- msg$msg
    vars.msg <- as.list(msg$clique)
  }
  print("SSEEPP")
  print(sep)
  print(pot)
  print(vars.msg)
  out <- mult(potentials[[to]], cliques[[to]], pot, sep, node.sizes)
  pot <- out$cpt
  #cliques[[to]] <- out$vars # though should be unchanged
  return(list("potential"=pot, "vars"=vars.msg))
  #print(potentials[[c]] %o% potentials[[d]])
}

proc.order <- function(node, from, adj)
{
  neighbours <- setdiff(which(adj[node,] > 0), from)
  print("==========")
  print(node)
  print(from)
  print(neighbours)
  if (length(neighbours) > 0)
  {
    for (n in neighbours) {
      proc.order(n, node, adj)
      parents.list <<- c(parents.list, node)
    }
  }
  process.order <<- c(process.order, node)
}

marginalize <- function(clique, pot, vars, sep)
{
  marg.dim <- which(unlist(vars) == sep)
  print(paste("marg.dim", marg.dim))
  
  # get dimensions, compute dimensions for the soon-to-be-created prob. table
  # and number of the values that it will contain
  dims <- dim(pot)
  print(paste("dims", dims))
  new.dims <- c(dims[-marg.dim])
  print(paste("new.dims", new.dims))
  new.num.vals <- prod(new.dims)
  length.of.run <- c(dims[marg.dim])
  print(new.num.vals * length.of.run == length(c(pot)))
  num.runs <- new.num.vals
  
  # switch dimensions in the array:
  # dimension corresponding to the variable to be marginalized goes first
  new.order <- c(marg.dim, (1:length(dims))[-marg.dim])
  print(paste("new.order", new.order))
  # remove marginalized dimension name
  new.order.names <- c(vars[-marg.dim])
  print(paste("no.names", new.order.names))
  # switch dimensions, make jpt a linear array
  cpt <- aperm(pot, new.order)
  marg <- array(cpt)
  print("marg")
  print(marg)
  marg <- tapply(marg, rep(1:new.num.vals, each=length.of.run), sum)  
  print("marg")
  print(marg)
  # apply new dimensions to resulting list (if needed)
  marg <- array(marg, new.dims, c(unlist(new.order.names)))
  print(marg)
  return(list("msg"=marg, "clique"=new.order.names))
}

mult <- function(cpt1, vars1, cpt2, vars2, node.sizes)
{
  print("$$$$$$$$$$$$$")
  print(vars1)
  print(vars2)
  # how to multiply (or, at least, how I try):
  # permute matrices in order to switch the common variable (there is at least one)
  # as last dimension. Then consider the c/jpts as monodimensional arrays,
  # expand them properly (jst discover how...), multiply them element-wise,
  # reconstruct dimensions for the resulting c/jpt and permute dimensions again.
  common.vars <- c(intersect(c(unlist(vars1)), c(unlist(vars2))))
  print("common vars")
  print(common.vars)
  where.in.table1 <- which(c(vars1) == common.vars)
  where.in.table2 <- which(c(vars2) == common.vars)
  print(where.in.table1)
  print(where.in.table2)

  if (length(vars1) > 1)
  {
    cpt1 <- aperm(cpt1, 
                   c(c(1:length(vars1))[-where.in.table1],
                     c(c(1:length(vars1))[where.in.table1])
                   ))
  }
  #if (length(dim(cpt2)) > 1)
  {
    second.table <- aperm(cpt2,
                          c(c(1:length(vars2))[-where.in.table2],
                            c(c(1:length(vars2))[where.in.table2])
                          ))
  }
  print("£££££££££££")
  print(cpt1)
  print(rep(c(unlist(cpt1)),
            prod(
              node.sizes[which(c(vars2) == where.in.table2)] ##### != ?????
            )
  ))
  print(rep(c(unlist(second.table)),prod(node.sizes[c(unlist(vars1))[-where.in.table1]])))
  print(unlist(vars1))
  cpt1 <- c(rep(c(unlist(cpt1)),
             prod(
               node.sizes[c(unlist(vars2))[where.in.table2]]
             )
  )) * 
    c(rep(c(unlist(second.table)),
          prod(node.sizes[c(unlist(vars1))[-where.in.table1]])))
  
  print("èèèèèèèèèèèèè")
  print(vars1)
  print("--")
  print(vars2)
  print("--")
  print(c(unlist(vars2)[-where.in.table2]))
  print("--")
  if (length(intersect(unlist(vars1),unlist(vars2))) > 0)
  {
    vars1 <- as.list(c(unlist(vars1), c(unlist(vars2)[-where.in.table2])))
  }
  else
  {
    vars1 <- as.list(c(unlist(vars1), c(unlist(vars2))))
  }
  print(vars1)
  cpt1 <- array(c(cpt1), c(node.sizes[unlist(vars1)]))
  readLines(file("stdin"),1)
  # sort dimensions
  # aperm
  new.ordering <- c(unlist(vars1))
  if (length(new.ordering) > 1)
  {
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
      dd <- data.frame(c1 = c(new.ordering), c2=c(1:length(new.ordering)))
      dd <- dd[with(dd,order(c1)),]
      new.ordering <- new.ordering[c(dd[,"c2"])]
      cpt1 <- aperm(cpt1, c(dd[,"c2"]))
      vars1 <- as.list(new.ordering)
    }
  }
  return(list("cpt"=cpt1, "vars"=vars1))
}
