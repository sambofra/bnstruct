directed.cycle.finding <- function(graph, how.many)
{
  # cumbersome, but at least it should work...
  # from any node, try to return to it.
  
  bfs <- function(i, start)
  {
    # breadth-first search
    # Stop at first cycle found and return it (if any)
    neighbours <- which(graph[i,] >= 1)
    if (length(neighbours) == 0) return()
    
    for (j in 1:length(neighbours))
    {
      prev[start] <<- i
      if (neighbours[j] == start)
      {
        k <- i
        cycle <- c(k)
        while (k != start)
        {
          k <- prev[k]
          cycle <- c(cycle, k)
        }
        num.cycles <<- num.cycles + 1
        cycles[[num.cycles]] <<- as.list(cycle)
        return()
      }
      else
      {
        if (how.many <= num.cycles)
          return()
        if (prev[neighbours[j]] == 0) {
          prev[neighbours[j]] <<- i
          bfs(neighbours[j], start)
        }
      }
    }
  }
  
  num.nodes <- nrow(graph)
  cycles <- NULL
  num.cycles <- 0
  for (i in 1:num.nodes)
  {
    prev <- rep(0, num.nodes)
    if (length(which(graph[i,] > 0)) > 0 && length(which(graph[,i] > 0)) > 0)
    {
      bfs(i,i)
    }
  }
  
  return(list("cycles" = cycles, "num" = num.cycles))
}

sink.finding.heuristic <- function(graph, initialization)
{
#   dfs             <- NULL
#   num.nodes       <- nrow(graph)
#   start.cpcs.vars <- initialization$start.cpcs.vars
#   scores          <- c(unlist(initialization$cpcs.scores))
#   for (i in 1:num.nodes)
#   {
#     st.ind   <- start.cpcs.vars[[i]]
#     offset   <- nrow(initialization$combinations[[i]])
#     d        <- data.frame(index = 1:offset, score = scores[st.ind:(st.ind + offset - 1)])
#     dfs[[i]] <- d[order(d$score), decreasing=TRUE]
#   }
}

sink.finding.reviewed <- function(graph, initialization)
{
#   dfs             <- NULL
#   num.nodes       <- nrow(graph)
#   start.cpcs.vars <- initialization$start.cpcs.vars
#   cpcs.vars       <- initialization$cpcs.vars
#   scores          <- c(unlist(initialization$cpcs.scores))
#   combs           <- initialization$combinations
#   heur.solution   <- c()
#   offsets         <- c(unlist(lapply(1:num.nodes, FUN=function(x){nrow(combs[[x]])})))
#   #   print(offsets)
#   #   print(c(unlist(start.cpcs.vars)))
#   total.score <- 0
#   sink.scores <- lapply(1:num.nodes, FUN = function(x){
#     scores[start.cpcs.vars[[x]] + offsets[x] - 1]
#   })
#   max.ind <- which.max(sink.scores)
#   
#   heur.solution <- c(heur.solution, start.cpcs.vars[[max.ind]] + offsets[max.ind] - 1)
#   total.score <- total.score + scores[max.ind]
#   
#   processed.nodes <- c()
#   processed.nodes <- c(max.ind)
#   parents <- which(graph[,max.ind] > 0)
#   
#   while (length(processed.nodes) < num.nodes)
#   {
#     indices <- c()
#     pscores <- c()
#     for (parent in parents)
#     {
#       if(is.element(parent, processed.nodes) == FALSE)
#       {
#         st.ind  <- start.cpcs.vars[[parent]]
#         end.ind <- start.cpcs.vars[[parent]] + offsets[parent] - 1
#         indices <- c(indices, st.ind:end.ind)
#         pstmp <- c(unlist(scores[st.ind:end.ind]))
#         stv <- start.cpcs.vars[[parent]]
#         ps <- lapply(1:(offsets[parent]), FUN=function(x){
#           vic <- c(0)
#           if (length(intersect(c(processed.nodes),c(vic,unlist(cpcs.vars[[stv+x-1]])))) > 0)
#             return(-Inf)
#           else
#             return(pstmp[x])
#         })
#         pscores <- c(pscores, ps)
#       }
#     }
#     max.ind <- indices[which.max(pscores)]
#     chosen.node <- max(which(c(unlist(start.cpcs.vars)) <= max.ind))
#     processed.nodes <- c(processed.nodes, chosen.node)
#     parents <- sort(unique(c(parents, which(graph[,chosen.node] > 0)))  )#,processed.nodes))
#     heur.solution <- c(heur.solution,max.ind)
#     total.score <- total.score + scores[max.ind]
#     cat(scores[max.ind], " ", total.score,"\n")
#   }
#   
#   return(list("heur.solution"  = sort(heur.solution),
#               "heur.sol.score" = total.score))
}

kl.bfs <- function(graph, k, l, s)
{
  # (k,l)-BFS for quickly finding "long" cycles
  # graph: adjacency matrix for a directed or undirected cycle
  # k: max # of cycles to be returned
  # l: max length of cycles to be returned
  # s: starting node
  num.nodes <- nrow(graph)
  pred      <- rep(0, num.nodes) # list of predecessor for each node
  pred[s]   <- s
  frontier  <- c(s)
  max.iters <- floor((l+1)/2)
  iter      <- 1
  num.found <- 0
  Q <- PriorityQueue()
  while (iter <= max.iters && length(frontier) > 0)
  {
    new.frontier <- c()
    while (length(frontier) > 0)
    {
      node <- frontier[1]
      frontier  <- frontier[-1]
      # looking at the rows ensures this works for both directed and undirected graphs
      neighbourhood <- unlist(which(graph[node,] > 0))
      to.delete <- which(neighbourhood == pred[node])
      if(length(to.delete) > 0)
        neighbourhood <- neighbourhood[-to.delete]
      for(i in neighbourhood)
      {
        if(pred[i] != 0 && i != s)
        {
          # need to take into account cliques
          flags <- rep(0,num.nodes)
          # already visited: cycle!
          curr        <- pred[i]
          cycle       <- c(i)
          flags[i]    <- 1
          while (curr != s && flags[curr] == 0)
          {
            cycle <- c(curr, cycle)
            flags[curr] <- 1
            curr  <- pred[curr]
          }
          cycle <- c(s, cycle)
          flags[s] <- 1
          curr  <- node
          while (curr != s && flags[curr] == 0)
          {
            cycle <- c(cycle, curr)
            flags[curr] <- 1
            curr  <- pred[curr]
          }
          #print(cycle)
          Q$insert(num.nodes+1-length(cycle), cycle)
          num.found <- num.found + 1
          pred[i] <- node # broaden cycles
          # remove current node from new.frontier to avoid duplicates and weird cycles
          to.delete <- which(new.frontier == i)
          if(length(to.delete) > 0)
            new.frontier <- new.frontier[-to.delete]
        }
        else
        {
          # never seen before: valid route for next rounds
          pred[i] <- node
          new.frontier <- c(new.frontier, i)
        }
      }
    }
    frontier <- c(unlist(unique(new.frontier)))
    iter <- iter + 1
  }
  # prepare for returning cycles, according to user parameters and items found
  cycles <- NULL
  if (num.found < k)
    k <- num.found
  for (i in 1:k)
  {
    cycles[[i]] <- Q$pop()
  }
  # ok, there is some problem with the length of the returned, but... doesn't matter that much
  return(list("cycles" = cycles, "num.cycles" = k))
}

tarjan.connected.components <- function(graph)
{
  # Tarjan's 1972 O(|V|+|E|) algorithm for finding strongly connected components in a directed graph.
  # Slightly modified: returns a list of connected components with 3+ nodes
  # (as a <-> b are already ruled out by directionality constraints).
  # Taken from wikipedia's pseudocode.
  # Useless for my purposes...
  
  strongconnect <- function(i)
  {
    print(index)
    indices[i] <<- index
    lowlink[i] <<- index
    index <<- index + 1
    s$push(i)
    
    # consider successors of i
    print(graph)
    edges <- which(graph[i,] == 1)
    print("edges")
    print(edges)
    print(length(edges))
    if (length(edges) == 0) return()
    print("***")
    print(edges)
    for (j in 1:length(edges))
    {
      if (indices[edges[j]] == 0)
      {
        print("--")
        print(indices)
        print(lowlink)
        print("--")
        strongconnect(edges[j])
        print("--")
        print(indices)
        print(lowlink)
        print("--")
        lowlink[i] <<- min(lowlink[i], lowlink[edges[j]])
      }
      else
      {
        print(paste("node already visited : ",edges[j]))
        f <- s$find(edges[j])
        print(paste(edges[j], " ", f$count))
        if (f$count > 0)
        {
          #print("a")
          lowlink[i] <<- min(lowlink[i], indices[edges[j]])
        }
      }
    }
    
    # If v is a root node, pop the stack and generate an SCC
    if (lowlink[i] == indices[i])
    {
      print(paste(i, " is a root node"))
      curr.scc <- c()
      repeat
      {
        w <- s$pop()
        curr.scc <- c(curr.scc, w)
        if (w == i) {
          break
        }
      }
      print("size now")
      print(s$size())
      if (length(curr.scc) > 2)
      {
        num.sccs <<- num.sccs + 1
        SCCs[[num.sccs]] <<- as.list(curr.scc)
      }
    }
  }
  
  num.nodes <- nrow(graph)
  index <- 1
  indices <- rep(0, num.nodes)
  lowlink <- rep(0, num.nodes)
  s <<- Stack()
  SCCs <- NULL
  num.sccs <- 0
  for (i in 1:num.nodes){
    if (indices[i] == 0)
    {
      strongconnect(i)
    }
  }
  
  return(list("sccs" = SCCs, "num" = num.sccs))
}

johnson.cycle.finding <- function(graph)
{
  # INCOMPLETE - find cycles in a directed graph using Johnson's algorithm
  
#   unblock <- function(u)
#   {
#     blocked[u] <<- FALSE
#     while (count.bl[u] > 0)
#     {
#       w <- bl.nodes[[u]][count.bl]
#       bl.nodes[[u]] <<- bl.nodes[[u]][-count.bl]
#       if (blocked[w])
#       {
#         unblock(w)
#       }
#     }
#   }
#   
#   circuit <- function(graph, v, s, st)
#   {
#     f <- FALSE
#     st$push(v)
#     blocked[v] <<- TRUE
#     succs <- which(graph[v,] > 0)
#     for (succ in succs)
#     {
#       if (succ == s)
#       {
#         st$push(s)
#         num.circs <<- num.circs + 1
#         circuits[[num.circs]] <<- as.list(c(st$get.elements()) + s - 1)
#         st$pop()
#         f <- TRUE
#       }
#       else
#       {
#         if(blocked[succ] == FALSE)
#         {
#           if(circuit(graph, succ, s, st))
#           {
#             f <- TRUE
#           }
#         }
#       }
#     }
#     
#     if (f)
#     {
#       unblock(v)
#     }
#     else
#     {
#       succs <- which(graph[v,] > 0)
#       for (succ in succs)
#       {
#         bl <- which(bl.nodes[succ] == v)
#         if (length(bl) == 0)
#         {
#           count.bl[succ] <<- count.bl[succ] + 1
#           bl.nodes[succ][[count.bl]] <<- v
#         }
#       }
#     }
#     st$pop()
#     return(f)
#   }
#   
#   
#   
#   num.nodes <- nrow(graph)
#   blocked   <- rep(FALSE, num.nodes)
#   bl.nodes  <- rep(list(NULL), num.nodes) # list of lists of blocked nodes
#   count.bl  <- rep(0, num.nodes) # counters for bl.nodes
#   circuits  <- NULL
#   num.circs <- 0
#   
#   bak <- graph
#   
#   s <- 1
#   while (s < num.nodes)
#   {
#     graph <- bak[s:num.nodes, s:num.nodes]
#     if (sum(graph) > 0)
#     {
#       
#     }
#     else
#     {
#       s <- n
#     }
#   }
  
}
