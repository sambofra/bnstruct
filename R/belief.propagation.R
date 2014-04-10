jt.belief.propagation <- function(dgraph, cpts, jpts, dnames, marginals, ctree, cliques, node.sizes)
{
  # Compute belief propagation for a given junction tree, from a given DAG
  # Starting from leaves of the junction tree, compute evidence by passing
  # messages up to the root, and then propagate it back.
  
  # Compute order of propagation:
  # - root is the highest-grade node; break ties by node order in tree
  # - recursively expand tree by moving to adjacent non-visited nodes,
#   prop.order <- jt.propagation.order(ctree)
#   
#   num.nodes <- nrow(ctree)
#   # Initial messages are all set to 1
#   messages <- as.list(rep(1, num.nodes))
#   potentials <- as.list(rep(1, num.nodes))
#   for (i in cs)
#     i <- sort(i)
# 
#   for (cl in cs)
#   {
#     print(cl)
#     env <- list()# new.env()
#     print("lllllllll")
#     for (i in 1:length(cl))
#     {
#       l <- unlist(reconstruct.dependencies.in.clique(cl[i], cl, dgraph))
#       print("l")
#       print(l)
#       env[[i]] <- l
#       print("env[i]")
#       print(unlist(env[i]))
#     }
#     potentials[i] <- initial.potentials(cl, env, cpts)
#     print("##########################################")
#  }
  
  # root = argmax(sum of weights of edges insisting over each node)
  root <- which.max(rowSums(ctree))
  ig <- graph.adjacency(ctree, "undirected", weighted=TRUE, diag=TRUE,
                        add.colnames=NULL, add.rownames=NA)

  potentials <- NULL
  for (i in 1:length(cliques))
  {
    potentials[[i]] <- initial.potential(dgraph, cliques[i], jpts, cpts, dnames, marginals, node.sizes)
  }
  print("potentials")
  print(potentials)
  #final.beliefs <- message.propagation(ctree, ig, root, 0, potentials)
}

jt.propagation.order <- function(ctree)
{
  # Compute propagation order.
  # Given the clique tree adjacency matrix, return the sequence of vertices
  # for the message passing step, in the form of a flattened list of couples
  # of nodes.
  
  num.nodes <- nrow(ctree)
  
  # levels of clique tree
  ctree.levels <- as.vector(rep(0, num.nodes))
  
  # root = argmax(sum of weights of edges insisting over each node)
  root <- which.max(rowSums(ctree))
  
  # make igraph object
  ig <- graph.adjacency(ctree, "undirected", weighted=TRUE, diag=TRUE,
                        add.colnames=NULL, add.rownames=NA) 

  # set root level as 1 and call method for label all the remaining nodes
  ctree.levels[root] <- 1
  ctree.levels <- rec.propagation.order(ctree, ig, root, ctree.levels, 2)
  # print(ctree.levels)
  
  # now, root has neighbours only of lower levels, leaves have only 1 neighbour
  # belonging to a higher level; internal nodes have one parent and one child.
  # Leaves may be at different levels.
  # By knowing this, we can propagate beliefs from leaves up to the root
  # and backward, from the lowest level (highest number) up to the root and back.
  #
  # In the first stage of message passing, each node (except the root)
  # is sending a message to its parent. In order to make the MP consistent,
  # we need to ensure that each internal node computes its message and
  # passes the result forward only when it has received all the messages from
  # its children. 
  #
  # For this, we need to sort the edges according to the computed node levels;
  # the following code is probably a _bea merda_, but it's the only way
  # I've managed to make it run, after too many hours.
  
  # Get edge list from igraph object, for each edge collapse the two vertices
  # into a single element with paste() and insert the couple into a new list.
  # Do the same with the vertices in reversed order, in order to let the
  # further comparison find all the edges. el.tmp has 2x(no. of edges),
  # the first half of it has edges with vertices in ascending order, second half
  # has the edges with vertices in descending order.
  edge.list <- get.edgelist(ig)
  el.tmp <- as.list(rep(0, 2*nrow(edge.list)))
  for (i in 1:nrow(edge.list))
  {
    el.tmp[i] <- paste(i, edge.list[i,2])
    el.tmp[(num.nodes - 1) + i] <- paste(edge.list[i, 2], i)
  }
  
  # Associate each node to its level, sort nodes according to level
  d <- data.frame(nodes = 1:num.nodes, ctls = ctree.levels)
  d <- d[order(d$ctls),]
  #print.data.frame(d)
  
  # Insert edges in the desired order.
  # Starting from leaves, and up to (but excluding) the root, select the edges
  # departing from each node towards a node in the immediately upper level
  # (which will be its parent). Only one among (u,v) and (v,u) will be selected.
  edge.order <- c()
  for (i in num.nodes:2)
  {
    edge.order <- c(edge.order,
                    intersect(
                      paste(d$nodes[i], d$nodes[d$ctls == d$ctls[i] - 1]),
                      unlist(el.tmp)))
  }
  
  # Manipulate result in order to produce a list of nodes, in the form
  # (from, to, from, to, from, to, ...) to easily (for me) use it later.
  prop.order <- c()
  for (i in 1:length(edge.order))
  {
    prop.order <- c(prop.order,unlist(strsplit(edge.order[i], " ")))
  }
  
  # print(prop.order)
  
  return(prop.order)
}

rec.propagation.order <- function(ctree, ig, current.node, levels, next.level)
{
  # Recursive tree visit for computing the tree levels:
  # starting from root (level 1) each node labels its non-visited neighbours
  # (= its descendants, since it's a tree).

  nei <- neighbors(ig, current.node)
  for (i in 1:length(nei))
  {
    if (levels[nei[i]] == 0)
    {
      levels[nei[i]] <- next.level
      levels <- rec.propagation.order(ctree, ig, nei[i], levels, next.level + 1)
    }
  }
  return(levels)
}

reconstruct.dependencies.in.clique <- function(node, cliq, dgraph)
{
  # Reconstruct dependencies of a given node in a given clique of a given DAG.
  # Returns the list of the variables the given node depends on.
  # NULL is returned if node does not depend on any other node of the clique.
  num.nodes <- length(cliq)
  # In the adjacency matrix of a DAG, a non-zero value in cell (i,j) denotes
  # a dependency i->j. Parsing the matrix column-wise will give dependencies
  # for each node.
  dep <- NULL
  for (j in 1:num.nodes)
  {
    if (dgraph[cliq[j],node] > 0)
    {
      dep <- as.list(c(dep, cliq[j]))
    }
  }
  return(dep)
}

initial.potential <- function(dgraph, clique, jpts, cpts, dnames, marginals, node.sizes)
{
   # Compute initial potential for given clique.
  cl <- unlist(clique) # nodes in the clique
  print("DGRAPH")
  print(dgraph)
  cs <- dgraph[cl, cl] # clique subgraph
  print("CS")
  print(cs)
  layering <- detect.structure(cs)
  if(length(cl) == 2)
  {
    # is this possible?
    parent <- unlist(layering[[1]])
    child  <- unlist(layering[[2]])
    
    init.pot <- mult.cpt.marg(cpts[[child]], unlist(dnames[[child]]), parent,
                                   marginals[[parent]], node.sizes)
  }
  else if(length(cl) >= 3)
  {
    # extract first 3 nodes
    # analyze first 3 nodes
    # iterate over remaining nodes
    if (length(layering[[1]]) == 2)
    {
      print("in case 1")
      # case A -> C <- B
      parents <- unlist(layering[[1]])
      A <- parents[1]
      B <- parents[2]
      C <- unlist(layering[[2]])
      ip.tmp <- mult.marg.marg(A, marginals[[A]],
                               B, marginals[[B]], node.sizes)
      init.pot  <- ip.tmp$jpt
      ip.dnames <- ip.tmp$dnames
      init.pot <- mult.cpt.jpt(cpts[[C]], init.pot, node.sizes)
    }
    else
    {
      if (length(layering) == 3)
      {
        print("in case 2")
        # case A -> B -> C
        A <- unlist(layering[[1]])[1]
        B <- unlist(layering[[2]])[1]
        C <- unlist(layering[[3]])[1]
        init.pot <- mult.cpt.marg(cpts[[B]], unlist(dnames[[B]]), A,
                                       marginals[[A]], node.sizes)
        res <- c()
        for (i in 1:node.sizes[B])
          res <- c(res, unlist(array(outer(init.pot[,i], cpts[[C]][,i], '*'))))
        init.pot <- res
        
        init.pot <- array(init.pot, c(2,2,2)) # FIND ORDER FOR NODE.SIZES!!!!!!!
        print(init.pot)
        init.pot <- aperm(init.pot, c(1,3,2))
        print(init.pot)
      }
      else if (length(layering)      == 2 &&
               length(layering[[1]]) == 1 &&
               length(layering[[2]]) == 2)
      {
        print("in case 3")
        # case B <- A -> C
        A <- unlist(layering[[1]])[1]
        children <- unlist(layering[[2]])
        B <- children[1]
        C <- children[2]
        init.pot <- mult.cpt.marg(cpts[[B]], unlist(dnames[[B]]), A,
                                       marginals[[A]], node.sizes)
        print(init.pot)
        res <- c()
        for (i in 1:node.sizes[B])
          res <- c(res, unlist(array(outer(init.pot[i,], cpts[[C]][,i], '*'))))
        init.pot <- res

        init.pot <- array(init.pot, c(2,2,2)) # FIND ORDER FOR NODE.SIZES!!!!!!!
        print(init.pot)
        init.pot <- aperm(init.pot, c(1,3,2))
        print(init.pot)
      }
      else if (length(layering) == 1)
      {
        print("in case 4")
        # A B C with no edges among them in the original graph 
        # e.g. same layer, but there are other nodes in the clique
        
      }
    }
  }
  
  return(init.pot)
  
  
  
  
  
#   print("aaa")
#   cl <- unlist(clique)
#   print(cl)
#   tmp <- cpts[[cl[1]]]
#   print(cpts[[cl[1]]])
#   print(tmp)
#   for (i in 2:length(cl))
#   {
#     # print(cl[i])
#     # print(unlist(dnames[cl[i]]))
#     
#     # identifica variabili in comune tra i-1 e i
#     in.common <- dnames[
#                     dnames = as.vector(
#                                 intersect(
#                                   unlist(
#                                     dnames[cl[i]]
#                                   ),
#                                   unlist(
#                                     dnames[cl[i-1]]
#                                   )
#                                 )
#                               )
#                        ]
#     print("in common")
#     in.common <- unlist(in.common)
#     print(in.common)
#     print(length(in.common))
#     # per ciascuna:
#     if (length(in.common) > 0)
#     {
#       for (j in 1:length(in.common))
#       {
#         # - permuta le dimensioni mettendo all'ULTIMO posto quella in comune
#         dn1 <- unlist(dnames[cl[i-1]])
#         dn2 <- unlist(dnames[cl[i]])
#         common1 <- which(dn1 == in.common[j])
#         common2 <- which(dn2 == in.common[j])
#         
#         dims1 <- dim(tmp)
#         dims2 <- dim(cpts[[cl[i]]])
#         
#         print(tmp)
#         print(dims1)
#         print(dims2)
#         
#         new.order.1 <- c((1:length(dims1))[-common1], common1)
#         new.order.2 <- c((1:length(dims2))[-common2], common2)
#         tmp <- aperm(tmp, new.order.1)
#         tmp2 <- aperm(cpts[[cl[i]]], new.order.2)
#         
#         lor1 <- prod(dims1[-common1])
#         lor2 <- prod(dims2[-common2])
#         
#         vs  <- NULL
#         l1  <- node.sizes[dn1[common1]]
#         print("*?")
#         print(node.sizes[strtoi(dn1[common1])] == node.sizes[strtoi(dn2[common2])])
#         for (k in 1:l1)
#         {
#           vs1 <- c((array(tmp))[((k-1)*l1)+(1:lor1)])
#           vs2 <- c((array(tmp2))[((k-1)*l1)+(1:lor2)])
#           vs[[k]] <- outer(vs1, vs2, '*')
#         }
#         
#         print(vs)
#         
#       }
#     # - suddividi la prima matrice in sottovettori
#     # - moltiplica con outer
#     # - trova come ricostruire la matrice del risultato...
#     }
#     
#     #tmp <- array(outer(tmp, array(cpts[[cl[i]]]), '*'))
#   }
#   # print(tmp)
}

message.propagation <- function(ctree, ig, current.node, from, init.pot)
{
  # Recursive message passing: starting from root, each node requests
  # its neighbours (except the calling node) to pass to it their messages,
  # in order to compute its own message. When each neighbour has received
  # all the expected messages, computes its belief and passes it to the caller.
#   nei <- neighbors(ig, current.node)
#   for (i in 1:length(nei))
#   {
#     if (nei[i] != from)
#     {
#       init.pot[nei[i]] <- init.pot[[nei[i]]] * init.pot[[current.node]]
#       new.pot <- message.propagation(ctree, ig, nei[i], current.node, init.pot)
#       init.pot[nei[i]] <- new.pot[nei[i]]
#       init.pot[current.node] <- init.pot[[current.node]] * init.pot[[nei[i]]]
#     }
#   }
#   return(init.pot)
}

detect.structure <- function(dgraph)
{
  gcopy <- dgraph
  # detect layering of nodes in the given graph
  layering <- NULL
  layer    <- 1
  print("detect structure")
  while (is.array(dgraph) && ncol(dgraph) > 0)
  {
    # find nodes in current layer
    print(layer)
    this.layer <- c()
    prune <- c()
    for (i in 1:ncol(dgraph))
    {
      if (length(dgraph[dgraph[,i] > 0]) == 0)
      {
        this.layer <- c(this.layer, dimnames(dgraph)[[1]][i])
        prune <- c(prune, i)
      }
    }
    # insert in returning list, eliminate rows/cols from matrix, iterate
    layering[[layer]] <- as.list(this.layer)
    layer <- layer + 1
    dgraph <- dgraph[-prune, -prune]
  }
  
  # if there is only one leaf in the graph, the cycle above will not include it
  # Need to manually check and insert it.
  if(length(unlist(layering)) < length(gcopy))
  {
    print("D, E")
    d <- unlist(dimnames(gcopy)[[1]])
    print(d)
    e <- unlist(layering)
    print(e)
    layering[[layer]] <- as.list(c(setdiff(d, e)))
  }
  
  print("**")
  print(layering)
  
  layering <- lapply(layering, as.numeric)
  return(layering)
}
