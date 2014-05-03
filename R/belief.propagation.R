belief.propagation <- function(graph, ctree, orig.cliques, marginals, node.sizes)
{
  cliques    <- orig.cliques
  num.nodes  <- nrow(graph)
  num.cliqs  <- length(cliques)
  messages   <- rep(as.list(c(1)), num.cliqs)
  potentials <- NULL
  
  root <- which.max(rowSums(ctree))
  print(root)
  print(marginals)

  #print("------------------")
  #print(ctree)
  #print(cliques)
  #print(length(cliques))
  
  # assign factors to a cluster graph
  # construct initial potentials
  for (clique in 1:num.cliqs)
  {
    curr.cl <- sort(cliques[[clique]])
    init <- marginals[[curr.cl[[1]]]]
    print(curr.cl)
    for (nodes in 2:length(curr.cl))
    {
      print(init)
      print(marginals[[curr.cl[[nodes]]]])
      #print(rep(marginals[[nodes]], length(init)))
      init <- init %o% marginals[[curr.cl[nodes]]]
    }
    print(init)
    potentials[[clique]] <- init
  }

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
  
  pp <- jpt.to.cpt(potentials[[6]], cliques[[6]], c(2), marginals, node.sizes)
  print(pp)
  
  break
  
  for (clique in 1:(num.cliqs-1))
  {
    #potentials[[cliques[[clique]]]] <- 
    #print(cliques[parents.list[clique]])
    out <- pass.message(process.order[clique], parents.list[clique], cliques, potentials, messages, leaves)
    #messages[[process.order[clique]]]
    messages[[process.order[clique]]] <- out$msg
    #cliques[[clique]] <- out$clique
    #print(potentials)
  }
  print(messages)
  
  # go from root to leaves
}

pass.message <- function(from, to, cliques, potentials, messages, leaves)
{
  sep <- intersect(unlist(cliques[from]), unlist(cliques[to]))
  pot <- potentials[[from]]
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
  msg <- marginalize(from, pot, cliques, sep)
  return(msg)
  #print(potentials[[c]] %o% potentials[[d]])
}

proc.order <- function(node, from, adj)
{
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

marginalize <- function(clique, pot, cliques, sep)
{
  # find dimensions to be marginalized
#   print("###############")
#   print(clique)
#   print(pot)
#   print(cliques)
#   print(sep)
#   print("###############")
  curr.cl <- cliques[[clique]]
  for (s in setdiff(cliques[[clique]],sep))
  {
#     print("@@@@@@@@@@")
#     print(s)
    pos  <- which(curr.cl == s)
    len  <- length(curr.cl)
    dims <- dim(pot)[-pos]
    new.num.vals  <- prod(dims)
#     print("...")
#     print(len)
#     print(dim(pot))
#     print(pos)
#     print(dim(pot)[pos])
    length.of.run <- dim(pot)[pos]
    per  <- c(pos,c(1:len)[-pos])
#     print(per)
#     print(length(pot))
#     print(new.num.vals)
#     print(length.of.run)
#     print(new.num.vals*length.of.run)
#     print("-----")
#     print(pot)
    pot  <- aperm(pot, per)
    pot <- tapply(array(pot), rep(1:new.num.vals, each=length.of.run), sum)
    pot <- array(pot, dims)
    curr.cl <- curr.cl[-pos]
  }
  return(list("msg"=pot, "clique"=curr.cl))
}

jpt.to.cpt <- function(jpt, clique, wrts, margs, node.sizes)
{
  # construct CPT starting from a JPT, conditioning on variable wtr.
  # Needs marginals.
  
  cpt <- jpt
  for (wrt in wrts)
  {
    # get position of the variable to be marginalized in the array dimension list
    marg.dim <- which(clique == wrt)
    
    dims <- dim(cpt)
    
    # switch dimensions in the array:
    # dimension corresponding to the variable to be marginalized goes first
    new.order <- c(marg.dim, (1:length(dims))[-marg.dim])
    z <- rep(array(unlist(margs[clique[marg.dim]])), prod(node.sizes[clique[-marg.dim]]))
    
    cpt <- aperm(cpt, new.order)
    cpt <- cpt / z
    cpt <- aperm(cpt, new.order)
  }
  
  return(cpt)
}