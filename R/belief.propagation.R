belief.propagation <- function(dgraph, cpts, ctree, cs)
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
  }
  
  # root = argmax(sum of weights of edges insisting over each node)
  root <- which.max(rowSums(ctree))
  ig <- graph.adjacency(ctree, "undirected", weighted=TRUE, diag=TRUE,
                        add.colnames=NULL, add.rownames=NA)
  print("potentials")
  print(potentials)
  final.beliefs <- message.propagation(ctree, ig, root, 0, potentials)
  print(final.beliefs)
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

initial.potentials <- function(cl, env, cpts)
{
  # compute the initial potential of a clique, given the nodes and the
  # dependencies in the original DAG.
#   print("INITIALPOT")
# #   m <- matrix(1,1,1)
# #   how.many.values <- 1
# #   for (j in 1:length(cl))
# #   {
# #     dims <- c(dims, node.sizes[j])
# #     how.many.values <- how.many.values * node.sizes[j]
# #   }
#   print("env")
#   dims <- c()
#   # print(length(env)==length(cl))
#   for (i in 1:length(env))
#   {
#     dims <- c(dims,dim(cpts[[cl[i]]]))
#     print("---")
#     #     print(paste(cl[i],env[[paste(cl[i])]]))
#     #     print(cl[i])
#     #     print(cpts[[cl[i]]])
#     print(dims)
#     print(length(unlist(env[i])))
#     print(cl[i])
#     print(unlist(env[i]))
# #     for (j in unlist(env[i]))
# #     {
# #       dims <- c(dims, dim(cpts[[cl[j]]]))
# #     }
#   }
#   print(dims)
#   print("+++++++++++")
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

marginalization <- function(node, wrt, cpt, prob.wrt, node.sizes)
{
  # Probability of a variable, with respect to another variable. Parameters:
  # - node: variable whose probability we want to compute
  # - wrt: variable to be marginalized
  # - cpt: conditional probability table for variable 'node'
  # - prob.wrt: probability distribution of wrt
  # - node.sizes: array of node sizes
  # Return probability values for variable 'node'
  num.values <- node.sizes[node]
  m <- c(rep(0, num.values))
  for (i in 1:num.values)
  {
    for (j in 1:node.sizes[wrt])
      m[i] <- m[i] + cpt[i,j]*prob.wrt[j]
  }
  return(m)
}