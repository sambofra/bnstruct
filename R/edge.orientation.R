orientation.init <- function(cplex.problem, graph, data)
{
  # graph : the skeleton
  num.nodes <- nrow(graph)
  # create I(u->v) variables, and initialize them with their value in the skeleton
  print("creating indicator variables for edges...")
  edge.ind.vars <- c(t(graph)) # thank you R
  storage.mode(edge.ind.vars) <- "integer"
  print("done")

  # position of I(u->v): (u-1) x N + v
  # constraint I(u->v) + I(v->u) = 1 becomes eiv[(u-1) x N + v] + eiv[(v-1) x N + u] = 1
  
  # to start, there are:
  # - sum(|CPCs|) variables I(W->v)
  # - |V|^2 variables I(u->v) (edge.ind.vars)
  
  # get cpcs
  print("creating indicator variables for CPCs, with their score...")
  cpcs          <- NULL
  combs         <- NULL
  cpcs.vars     <- NULL
  cpcs.scores   <- NULL
  really.in     <- c()
  in.position   <- c()
  position.ind  <- 1
  start.cpcs.vs <- c()
  k             <- 1
  temp.sum      <- 1
  last.start    <- 1
  overall.dcj   <- c()
  
  # iterate through nodes
  for(i in 1:ncol(graph))
  {
    print(paste("- evaluating node",i))
    combs[[i]]              <- as.list(as.list(1), as.list(2)) # sporcotrucco^(tm), otherwise may fail abruptly if first node is disconnected
    cpcs[[i]]               <- as.list(c(which(graph[,i] > 0)))
    storage.mode(cpcs[[i]]) <- "integer"
    combinations            <- fast.bincombinations(length(cpcs[[i]]))
    
    if (length(cpcs[[i]]) == 0) # need in case of node separated from rest of network
    {
      combinations <- as.matrix(c(0), c(1, 1))
    }
    
    cat("  (has",length(cpcs[[i]]), "possible parents, and therefore", nrow(combinations),"candidate parent sets to evaluate)\n")
    
    combs[[i]]     <- combinations
    cpcs.vars[[k]] <- as.list(c())
    
    # array of flags: 1 if relative cpc has to be evaluated or can be pruned
    # a cpc can be pruned if all of its proper subsets have to be pruned (without evaluating score)
    # or if its score is lower than than the scores of all its proper subsets
    # evaluation can be limited to the (|cpc|-1)-sized subsets
    dc.j.pruning.flag <- rep(1, nrow(combinations))
    
    for (j in 1L:nrow(combinations))
    {
      cat(j,"/",nrow(combinations),"\n")
      subset               <- c(cpcs[[i]][which(combinations[j,] > 0)])
      storage.mode(subset) <- "integer"
      cat("subset : ",subset,"\n")
      # print(subset-1L)
      cpcs.vars[[k]] <- as.list(c(unlist(subset)))
      storage.mode(cpcs.vars[[k]]) <- "integer"
      
      # apply an early DeCampos&Ji pruning: compute score only if really needed ; otherwise, set to 0 and set variable to denied
      if (length(subset) >= 2)
      {
        # look for already discarded subsets: then the variable can already be discarded
        flag <- 1
        discard <- 0
        for (l in 1:length(combinations[j,]))
        {
          if (combinations[j,l]>0)
          {
            offset <- 0
            for (m in 1:length(combinations[j,]))
            {
              if (m == l)
                offset <- 2*offset
              else
                offset <- 2*offset + combinations[j,m]
            }
            offset <- offset + 1
            cat("(",offset,",",last.start,",", dc.j.pruning.flag[offset+last.start-1], ") ") 
            if (!is.element(offset+last.start-1, really.in))
            {
              flag <- 0
              discard <- 1
              # cat("**", offset, offset+last.start-1, "\n")
              # readLines(file("stdin"),1)
              break
            }
          }
          cat("\n")
        }
        if (flag == 1)
        {
          # if CPC cannot be discarded yet, compute its score, and evaluate against all of its subsets
          score <- .Call( "score_node", data, node.sizes, i-1L, subset-1L, 
                           ess = 1, PACKAGE = "bnstruct" );
          for (l in 1:length(combinations[j,]))
          {
            if (combinations[j,l]>0)
            {
              offset <- 0
              discard <- 0
              for (m in 1:length(combinations[j,]))
              {
                if (m == l)
                  offset <- 2*offset
                else
                  offset <- 2*offset + combinations[j,m]
              }
              offset <- offset + 1
              if (dc.j.pruning.flag[offset] == 1 && cpcs.scores[[offset+last.start-1]] > score ) 
              {
                dc.j.pruning.flag[j] <- 0
                discard <- 1
                break
              }
            }
          } 
          if (discard == 0)
          {
            really.in    <- c(really.in, j+last.start-1)
            in.position  <- c(in.position, position.ind)
            position.ind <- position.ind + 1
          }
        }
        else # if flag = 0, discard immediately
        {
          dc.j.pruning.flag[j] <- 0
          score <- 0
        }
      }
      else # if length (subset) <= 1
      {
        # normal scoring
        score        <- .Call( "score_node", data, node.sizes, i-1L, subset-1L, 
                               ess = 1, PACKAGE = "bnstruct" );
        really.in    <- c(really.in, j+last.start-1)
        in.position  <- c(in.position, position.ind)
        position.ind <- position.ind + 1   
      }
      
      cpcs.scores[[k]] <- score# 1+runif(1, -1, 0)
      k <- k + 1
    }
    start.cpcs.vs <- c(start.cpcs.vs, temp.sum*1L)
    last.start    <- last.start + length(dc.j.pruning.flag)
    temp.sum      <- temp.sum + nrow(combinations)
    overall.dcj   <- c(overall.dcj, dc.j.pruning.flag)
  }
  
  start.edge.vs <- temp.sum*1L
  num.vars      <- temp.sum + num.nodes * num.nodes - 1L
  storage.mode(start.cpcs.vs) <- "integer"
  
  # cat(length(overall.dcj), "=?=", start.edge.vs-1,"\n")
  # print(overall.dcj)
  # readLines(file("stdin"), 1)
  
  print("done")
  
  print("populating problem...")
  env  <- cplex.problem$env
  prob <- cplex.problem$prob
  # num of columns : variables
  nc <- num.vars # remember that R counts from 1, cplex from 0... 
  # num of rows : constraints : nothing for now, to be added with addRows
  nr <- 1 #(num.nodes * num.nodes - num.nodes)/2 + 2*(temp.sum-1) # stiamo larghi
  # lower bounds = 0 for all
  lb <- c(rep(0, num.vars))
  # upper bounds = 1 for all; then set to 0 the ub for the edges forbidden by the MMPC step
  ub <- c(rep(1, num.vars))
  ub[which(edge.ind.vars == 0)+start.edge.vs-1] <- 0
  
  # objective function coefficients: the scores associated to I(W->v) vars
  obj <- c(c(unlist(cpcs.scores)), rep(0, num.nodes * num.nodes))

  # it's a max problem
  setObjDirCPLEX(env, prob, CPX_MAX)
  
  # create CPLEX variables
  ctype = c(rep('C', num.vars - num.nodes*num.nodes), rep('B', num.nodes*num.nodes))
  newColsCPLEX(env, prob, num.vars, obj, lb, ub, ctype, NULL)
  
  # constraints: I(u->v) + I(v->u) = 1
  print(" - adding directionality constraints...")
  bst <- 0
  beg <- c()
  vst <- c(1,1)
  val <- c()
  rhs <- c()
  sen <- c()
  ind <- c()
  nr  <- 0
  nnz <- 0
  for (i in 2:num.nodes)
  {
    for (j in 1:(i-1))
    {
      if (graph[i,j] == 1)
      {
        beg <- c(beg, bst)
        bst <- bst + 2
        val <- c(val, vst)
        rhs <- c(rhs, 1)
        # sen <- c(sen, 'E')
        sen <- c(sen, 'L')
        nr  <- nr + 1
        nnz <- nnz + 2
        # all of the above can be replaced by reps, but I doubt this is the bottleneck...
        ind <- c(ind, (j-1)*num.nodes+i, (i-1)*num.nodes+j)
      }
    }
  }
  ind <- ind + start.edge.vs - 2
  addRowsCPLEX(env, prob, 0, nr, nnz, beg, ind, val, rhs, sen, NULL, NULL)
  print("   directionality constraints added")
  
  
  # decampos+ji: remove useless variables
  remove <- c(setdiff((1:length(overall.dcj)),really.in))
  print(remove)
  readLines(file("stdin"),1)
  chgColTypeCPLEX(cplex.problem$env,
                  cplex.problem$prob,
                  length(remove),
                  c(remove)-1,
                  rep('B',length(remove)))
  chgBndsCPLEX(cplex.problem$env,
               cplex.problem$prob,
               length(remove),
               c(remove)-1,
               rep('B', length(remove)),
               rep(0, length(remove)))

  # constraints I(W->v) = prod...
  print(" - adding bounding constraints...")
  for (i in 1:num.nodes)
  {
    nnz <- length(cpcs[[i]]) + 1
    print(paste(i, " ", nrow(combs[[i]])))
    if (nnz == 1)
    {
      chgBndsCPLEX(cplex.problem$env,
                   cplex.problem$prob,
                   1,
                   c(start.cpcs.vs[[i]])-1,
                   rep('B', 1),
                   rep(1, 1))
    }
    else
    {
      for (j in 1:nrow(combs[[i]]))
      {
        if (is.element(j + start.cpcs.vs[[i]]-1, really.in))
        {
          tmp <- build.bounding.constraint(num.nodes, i, cpcs[[i]],combs[[i]][j,],j,
                                           start.cpcs.vs[[i]], start.edge.vs)
          addRowsCPLEX(env, prob, 0, 1, nnz, tmp$beg, tmp$ind, tmp$uval,
                       tmp$urhs, tmp$usen, NULL, NULL)
          addRowsCPLEX(env, prob, 0, 1, nnz, tmp$beg, tmp$ind, tmp$lval,
                       tmp$lrhs, tmp$lsen, NULL, NULL)          
        }
      }
      # need to impose at least one candidate parent set for each node
      at.least.one <- c(1:nrow(combs[[i]])) + start.cpcs.vs[[i]] - 1
      addRowsCPLEX(env, prob, 0, 1, length(at.least.one), c(0), at.least.one-1, rep(1, length(at.least.one)),
                   c(1), c('E'), NULL, NULL) # only one
    }
  }
  print("   bounding constraints added")
  
  print("done")
  
  return(list("cpcs"            = cpcs,
              "combinations"    = combs,
              "cpcs.vars"       = cpcs.vars,
              "edge.vars"       = edge.ind.vars,
              "start.cpcs.vars" = start.cpcs.vs, # remember: start counting from 1
              "start.edge.vars" = start.edge.vs, # idem
              "num.vars"        = num.vars,
              "num.cpcs.vars"   = temp.sum - 1,
              "cpcs.scores"     = cpcs.scores))
}

decampos.ji.pruning <- function(num.nodes, cplex.problem, initialization)
{
  # DeCampos&Ji pruning _after_ all the initialization: not used anymore
  # for every CPC, compare against all of its subsets
  print("De Campos & Ji pruning")
  remove  <- c()
  offsets <- c()
  for (i in 1:num.nodes)
  {
    offsets <- c(offsets, nrow(initialization$combinations[[i]]))
  }
  ranges <- c(c(initialization$start.cpcs.vars), initialization$start.edge.vars)
  for (i in 1:num.nodes)
  {
    if(offsets[i] <= 2) next
    cat(i, "", ranges[i+1]-1, "", ranges[i]+1, "\n")
    for (j in (ranges[i+1]-1):(ranges[i]+2))
    {
      for (k in (j-1):(ranges[i]+1))
      {
        if (initialization$cpcs.scores[j] < initialization$cpcs.scores[k] &&
            !is.element(FALSE, is.element(c(unlist(initialization$cpcs.vars[[k]])),
                                          c(unlist(initialization$cpcs.vars[[j]])))))
        {
          #cat(k,"", c(unlist(initialization$cpcs.vars[[k]])), "", initialization$cpcs.scores[k],
          #    "",j, "", c(unlist(initialization$cpcs.vars[[j]])), "",initialization$cpcs.scores[j], "\n")
          remove <- c(remove, j)
        }
      }
      remove <- unique(remove)
    }
  }
  remove <- sort(remove)
  cat("De Campos & Ji has pruned:", length(remove), "variables\n")
  
  chgColTypeCPLEX(cplex.problem$env,
                  cplex.problem$prob,
                  length(remove),
                  c(remove)-1,
                  rep('B',length(remove)))
  chgBndsCPLEX(cplex.problem$env,
               cplex.problem$prob,
               length(remove),
               c(remove)-1,
               rep('B', length(remove)),
               rep(0, length(remove)))
  
  return(remove)
}

build.bounding.constraint <- function(num.nodes, node, cpc, combination,
                                      comb.ind, start.cpc, start.edge.vs)
{
  # Builds the variables cplex needs to add a bounding constraint
  # for given CPC and combination of parents and children.
  # Returns a list containing the arrays of variables.
  # params:
  # - num.nodes : number of nodes
  # - node : current node
  # - cpc : list of nodes forming the cpc evaluated for node
  # - combination : {0,1} array
  # - comb.ind : relative position of 'combination' among the possible combinations
  #              for that node (corresponds to decimal value of combination)
  # - start.cpc : index of first combination for that node; 
  #               start.cpc + comb.ind - 1 = index of current I(W->v) variable
  # - start.edge.vs : starting position if I(u->v) variables
  var.index <- start.cpc + comb.ind - 1
  # uvar : upper bound constraint
  # lvar : lower bound constraint
  # beg, ind are in common
  beg <- c(0)
  ind <- c(var.index)
  uval <- c(1)
  urhs <- 0
  usen <- c('G')
  lval <- c(length(cpc))
  lrhs <- c(0)
  lsen <- c('L')
  # now iteratively build up the whole constraints
  #print("node")
  #print(node)
  #print("cpc")
  cpc <- c(unlist(cpc))
  #print(cpc)
  combination <- c(unlist(combination))
  tmp.ind <- c()
  tmp.val <- c()
  if (length(cpc) >= 1)
    for (i in 1:length(cpc))
    {
      tmp.ind <- c(tmp.ind, (cpc[i]-1)*num.nodes+node + start.edge.vs - 1)
      #print((cpc[i]-1)*num.nodes+node)
      if (combination[i] == 1)
      {
        tmp.val <- c(tmp.val, -1)
        urhs[1] <- urhs[1] - 1
        # lrhs unchanged
      }
      else
      {
        tmp.val <- c(tmp.val, 1)
        lrhs[1] <- lrhs[1] + 1
        # urhs[1] <- urhs[1] + 0 # -1 for the variable + 1 for the NEGATED variable
      }
    }
  lval <- c(lval, tmp.val)
  uval <- c(uval, tmp.val)
  ind  <- c(ind, tmp.ind) - 1
  #print(ind)
  #print(lval)
  #print(uval)
  return(list("beg"  = beg,
              "ind"  = ind,
              "uval" = uval,
              "urhs" = urhs,
              "usen" = usen,
              "lval" = lval,
              "lrhs" = lrhs,
              "lsen" = lsen))
}

build.source.sink.constraints <- function(graph, cycle, initialization, cplex.problem)
{
  # Build subtour elimination constraints - TODO rename method
  num.nodes       <- nrow(graph)
  beg <- c(0)
  ind <- c()
  val <- c(rep(1, length(cycle)))
  rhs <- c(length(cycle)-1)
  sen <- c('L')
  rcycle <- c(cycle, cycle[1])
  for (i in 1:length(cycle))
  {
    # cat(i," ", rcycle[i]," ",rcycle[i+1],"\n")
    ind <- c(ind, initialization$start.edge.vars + (rcycle[i+1]-1) * num.nodes + rcycle[i] - 2)
  }
  int <- sort(ind)
  addRowsCPLEX(cplex.problem$env, cplex.problem$prob, 0, 1, length(cycle), beg, ind, val, rhs, sen, NULL, NULL)
  ind <- c()
  for (i in length(cycle):1)
  {
    # cat(i," ", rcycle[i]," ",rcycle[i+1],"\n")
    ind <- c(ind, initialization$start.edge.vars + (rcycle[i]-1) * num.nodes + rcycle[i+1] - 2)
  }
  ind <- sort(ind)
  addRowsCPLEX(cplex.problem$env, cplex.problem$prob, 0, 1, length(cycle), beg, ind, val, rhs, sen, NULL, NULL)
}

solve.cplex.problem <- function(cplex.problem, initialization, starting.sol)
{
  # solve the relaxated problem using CPLEX, until a DAG is found. Cost will be an upper bound of the real graph.
  env             <- cplex.problem$env
  prob            <- cplex.problem$prob
  num.nodes       <- length(initialization$cpcs)
  start.edge.vars <- initialization$start.edge.vars

  solved <- FALSE
  
  num.iters <- 0
  stat <- 1
  
  while (!solved)
  {
    num.iters <- num.iters + 1
    
    # add starting.sol with CPXaddmipstarts or CPXchgmipstarts
#     if (length(starting.sol) > 0)
#     {
#       setIntParmCPLEX(cplex.problem$env, CPX_PARAM_ADVIND, 2)
#       status = chgMIPstartsCPLEX(cplex.problem$env, cplex.problem$prob, 1, c(0), length(starting.sol), c(0),
#                                  c(1:length(starting.sol))-1, starting.sol, NULL)
#       if (status != 0)
#       {
#         status = addMIPstartsCPLEX(cplex.problem$env, cplex.problem$prob, 1, c(0), length(starting.sol), c(0),
#                                    c(1:length(starting.sol))-1, starting.sol, NULL, NULL)
#       }
#     }
    
    mipoptCPLEX(env, prob)
    sol <- solutionCPLEX(env, prob)
    #print(sol)
    if (!is.list(sol))
    {
      stat   <- sol$lpstat
      solved <- TRUE
      dag    <- NULL
      x      <- NULL
      z      <- -Inf 
    }
    else
    {
      x   <- sol$x
      z   <- sol$objval
      evs <- x[start.edge.vars:(start.edge.vars+num.nodes*num.nodes-1)]
      iwv <- x[1:(start.edge.vars-1)]
      
      {
#         print("------")
#         q1 <- length(which(iwv == 0))
#         q2 <- length(which(iwv == 1))
#         q3 <- length(iwv)
#         q4 <- q3 - q2 - q1
#         print(q1)
#         print(q2)
#         print(q3)
#         print(q4)
#         print(".")
#         q1 <- length(which(evs == 0))
#         q2 <- length(which(evs == 1))
#         q3 <- length(evs)
#         q4 <- q3 - q2 - q1
#         print(q1)
#         print(q2)
#         print(q3)
#         print(q4)
#         print("------")
      }
      
      # for CPLEX numerical issues, che non si sa mai
      evs   <- matrix(lapply(evs, FUN = function(x){
                                          if(x >= 0.999){
                                            x <- 1
                                          } else if(x <= 0.001){
                                            x <- 0
                                          }
                                        }
                      ), c(num.nodes, num.nodes))
      storage.mode(evs) <- "integer"
      dir.g <- matrix(evs, c(num.nodes, num.nodes), byrow=TRUE)
      print(dir.g)
      
      #plot.mat(dir.g)
      
      if (is.acyclic(dir.g))
      {
        # EUREKA!
        dag <- dir.g
        print("ACYCLIC!")
        solved <- TRUE
        cat("total iterations:",num.iters,"\n")
      }
      else
      {
        # look for cycles, add relative sink/source constraints
        cs <- directed.cycle.finding(dir.g, 20)
        print(cs)
        
        num.cycles <- cs$num
        cycles     <- cs$cycles
        # note that at least one cycle has to be found!
        
        # if >1 cycle, check they are different
        continue <- TRUE
        while (continue)
        {
          continue <- FALSE
          if (num.cycles > 1)
          {
            for (i in 1:(num.cycles-1))
            {
              if (!is.element(FALSE, is.element(cycles[[i]],cycles[[i+1]])) &&
                  !is.element(FALSE, is.element(cycles[[i+1]],cycles[[i]]))){
                cycles[[i+1]] <- NULL
                continue <- TRUE
                print("discarded one cycle")
                break
              }
            } 
            if(continue)
              num.cycles <- num.cycles - 1
          }
        }
        print(cycles)
        
        for (i in 1:num.cycles)
        {
          cycle <- rev(c(unlist(cycles[[i]])))
          snv   <- getNumColsCPLEX(env, prob) + 1
          num.vars <- 2*length(cycle) # both for source&sink
          tmp   <- build.source.sink.constraints(dir.g, cycle, initialization, cplex.problem)
        }
        dag <- dir.g
      }
      starting.sol = x
    }
  }
  
  return(list("dag"   = dag,
              "score" = z,
              "solx"  = x,
              "cplex.status" = stat))
}

solve.and.full.rounding <- function(num.nodes, cplex.problem, initialization)
{
  # solve the problem, iterating the call to solve.cplex.problem until an integral solution is found
  bak        <- rep(0, num.nodes*num.nodes)
  solving.iterations  <- 0
  best.score <- -Inf
  best.dag   <- bak
  best.sol   <- c()
  
  starting.sol = c()
  
  while(TRUE){
    print("about to solve")
    out <- solve.cplex.problem(cplex.problem, initialization, starting.sol)
    print("ok, back from solved")
    
    # error
    if(out$cplex.status == 1217) break
    
    fract <- which(out$solx > 0.001 & out$solx < 0.999)
#     print(fract)
#     print(out$score)
#     print(shd(bak, out$dag))
    
    if(length(fract) == 0) {
      print("INTEGRAL SOLUTION")
      best.score <- out$score
      best.dag   <- out$dag
      plot.mat(out$dag)
      readLines(file("stdin"),1)
      break
    }
    
    if(out$score < best.score)
    {
      #should not happen
      print("lb > ub :::: stop")
      break
    }
    
    solving.iterations <- solving.iterations + 1
    cat("# round : ",solving.iterations, "\n")
    if(shd(best.dag, out$dag) == 0)
    {
      print("eureka!")
      #break
    }
    else if(solving.iterations > 0)
    {
      print("tadà")
    }
    if(solving.iterations >= 200) break
    
    rs <- recompute.score.from.lp.sol(out$dag, initialization)
    print(rs$score)
    if (rs$score >= best.score) {
      print("*********_______________+++++++++++++++++++++++°°°°°°°°°°°°°°°°°°°°")
      best.score <- rs$score
      best.sol   <- rs$cpcs.ind.vars
      best.dag   <- out$dag
      plot.mat(out$dag)
      
      starting.sol = c(best.sol, best.dag)
      
    } else {
      cat(rs$score, "<", best.score,", currently the best score\n")
      print("adding tabu cuts")
      num.edges <- length(which(t(out$dag) > 0))
      # one of us cannot be wrong (cit.)
      addRowsCPLEX(cplex.problem$env, cplex.problem$prob, 0, 1, num.edges,
                   c(0), c(which(t(out$dag) > 0) + initialization$start.edge.vars - 2),
                   rep(1, num.edges), c(num.edges - 1), c('L'), NULL, NULL)
#       addRowsCPLEX(cplex.problem$env, cplex.problem$prob, 0, 1, num.edges,
#                    c(0), c(which(out$dag > 0) + initialization$start.edge.vars - 2),
#                    rep(1, num.edges), c(num.edges - 1), c('L'), NULL, NULL)
      chgColTypeCPLEX(cplex.problem$env,
                      cplex.problem$prob,
                      length(rs$cpcs.ind.vars),
                      c(rs$cpcs.ind.vars)-1,
                      rep('B',length(rs$cpcs.ind.vars)))
    }
    
    print("rounding")
    chgColTypeCPLEX(cplex.problem$env,
                    cplex.problem$prob,
                    length(fract),
                    fract-1,
                    rep('B',length(fract)))

    setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIRCUTS, 2)
    
    bak <- out$dag
    
  }
  return(list("dag"   = best.dag,
              "cpcs"  = best.sol,
              "score" = best.score))
}

old.solve.and.local.rounding <- function(graph, cplex.problem, initialization)
{
  num.nodes <- nrow(graph)
  bak <- rep(0, num.nodes*num.nodes)
  best.score <- -Inf
  best.dag   <- rep(0, num.nodes*num.nodes)
  best.sol   <- c()
  while (TRUE)
  {
    writeProbCPLEX(cplex.problem$env, cplex.problem$prob, "final_prob.lp")
    out <- solve.cplex.problem(cplex.problem, initialization)
    
    fract <- which(out$solx > 0.001 & out$solx < 0.999)
    
    print("-----------------------------------------------------------------------------")
    print(fract)
    print(out$score)
    print(shd(best.dag, out$dag))
    
    if(length(fract) == 0) break
    if(shd(best.dag, out$dag) == 0)
    {
      print("eureka!")
      # break
    }
    
    # fix the already integer variables
    ind <- c(1:(initialization$start.edge.vars-1))
    x   <- out$solx
    q1  <- which(x[ind] == 0)
    lq1 <- length(q1)
    q2  <- which(x[ind] == 1)
    lq2 <- length(q2)
    print(q2)
    #print(initialization$start.edge.vars)
    
#     chgColTypeCPLEX(cplex.problem$env,
#                     cplex.problem$prob,
#                     lq1,
#                     c(x[q1])-1,
#                     rep('B',lq1))
    chgColTypeCPLEX(cplex.problem$env,
                    cplex.problem$prob,
                    lq2,
                    c(x[q2])-1,
                    rep('B',lq2))
#     print(ind[q1])
#     print(length(ind[q1]))
#    chgBndsCPLEX(cplex.problem$env,
#                  cplex.problem$prob,
#                  lq1,
#                  c(ind[q1])-1,
#                  rep('B', lq1),
#                  rep(0, lq1))
    chgBndsCPLEX(cplex.problem$env,
                 cplex.problem$prob,
                 lq2,
                 c(ind[q2])-1,
                 rep('B', lq2),
                 rep(1, lq2))
    
    rs <- recompute.score.from.lp.sol(out$dag, initialization)
    print(initialization$start.edge.vars)
    print(initialization$start.cpcs.vars)
    offs <- c()
    for (i in 1:num.nodes)
    {
      offs <- c(offs, nrow(initialization$combinations[[i]]))
    }
    print(offs)
    cat("rs$score", rs$score, "\n")
    print(rs$cpcs.ind.vars)
    # readLines(file("stdin"),1)
    if (rs$score >= best.score) {
      best.score <- rs$score
      best.sol   <- rs$cpcs.ind.vars
      best.dag   <- out$dag
    } else {
      cat(rs$score, "<", best.score,", currently the best score\n")
      print("adding tabu cuts")
      num.edges <- length(which(t(out$dag) > 0))
      addRowsCPLEX(cplex.problem$env, cplex.problem$prob, 0, 1, num.edges,
                   c(0), c(which(t(out$dag) > 0) + initialization$start.edge.vars - 2),
                   rep(1, num.edges), c(num.edges - 1), c('L'), NULL, NULL)
      chgColTypeCPLEX(cplex.problem$env,
                      cplex.problem$prob,
                      length(rs$cpcs.ind.vars),
                      c(rs$cpcs.ind.vars)-1,
                      rep('B',length(rs$cpcs.ind.vars)))
    }
    
    print("rounding")
    chgColTypeCPLEX(cplex.problem$env,
                    cplex.problem$prob,
                    length(fract),
                    fract-1,
                    rep('B',length(fract)))
    
    setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIRCUTS, 2)
    
    #bak <- out$dag
  }
  return(list("dag"   = best.dag,
              "cpcs"  = best.sol,
              "score" = best.score))
}

solve.and.local.rounding <- function(graph, cplex.problem, initialization)
{
#   num.nodes  <- nrow(graph)
#   bak        <- rep(0, num.nodes*num.nodes)
#   best.score <- -Inf
#   best.dag   <- rep(0, num.nodes*num.nodes)
#   best.sol   <- c()
#   while (TRUE)
#   {
#     writeProbCPLEX(cplex.problem$env, cplex.problem$prob, "final_prob.lp")
#     out <- solve.cplex.problem(cplex.problem, initialization)
#     
#     fract <- which(out$solx > 0.001 & out$solx < 0.999)
#     
#     print("-----------------------------------------------------------------------------")
#     print(fract)
#     print(out$solx[fract])
#     print(out$score)
#     print(shd(best.dag, out$dag))
#     
#     if(length(fract) == 0) {
#       print("SOLUZIONE INTERA")
#       break
#     }
#     if(shd(best.dag, out$dag) == 0)
#     {
#       print("eureka!")
#       # break
#     }
#     
#     # raise the highest variables the already integer variables
#     ind <- c(1:(initialization$start.edge.vars-1))
#     cpcs.vars.range <- c(unlist(initialization$start.cpcs.vars), initialization$start.edge.vars)
#     raised <- c()
#     for (i in 1:num.nodes)
#     {
#       i.fract <- c(fract[which(fract >= cpcs.vars.range[i] & fract < cpcs.vars.range[i+1])])
#       m <- which.max(out$solx[i.fract])
#       print("**")
#       cat(i.fract, "|", m, "| ", i.fract[m],"|",out$solx[i.fract[m]], "\n")
#       cat(".. ", out$solx[i.fract], "|", out$solx[i.fract[m]], "\n")
#       print(which(out$solx[i.fract] == out$solx[i.fract[m]]))
#       print(length(which(out$solx[i.fract] == out$solx[i.fract[m]])))
#       
#       if(length(which(out$solx[i.fract] == out$solx[i.fract[m]])) == 1)
#       {
#         cat("#",i.fract[m] ,"",ind[i.fract[m]],"\n")
#         chgColTypeCPLEX(cplex.problem$env,
#                         cplex.problem$prob,
#                         1,
#                         c(i.fract[m])-1,
#                         c('B'))
#         chgBndsCPLEX(cplex.problem$env,
#                      cplex.problem$prob,
#                      1,
#                      c(i.fract[m])-1,
#                      c('B'),
#                      c(1))
#         raised <- c(raised, i.fract[m])
#         break
#       }
#     }
#     print("raised")
#     print(raised)
#     
#     rs <- recompute.score.from.lp.sol(out$dag, initialization)
#     print(initialization$start.edge.vars)
#     print(initialization$start.cpcs.vars)
#     offs <- c()
#     for (i in 1:num.nodes)
#     {
#       offs <- c(offs, nrow(initialization$combinations[[i]]))
#     }
#     print(offs)
#     cat("rs$score", rs$score, "\n")
#     print(rs$cpcs.ind.vars)
#     # readLines(file("stdin"),1)
#     if (rs$score >= best.score) {
#       best.score <- rs$score
#       best.sol   <- rs$cpcs.ind.vars
#       best.dag   <- out$dag
#     } else {
#       cat(rs$score, "<", best.score,", currently the best score\n")
#       print("adding tabu cuts")
#       num.edges <- length(which(t(out$dag) > 0))
#       addRowsCPLEX(cplex.problem$env, cplex.problem$prob, 0, 1, num.edges,
#                    c(0), c(which(t(out$dag) > 0) + initialization$start.edge.vars - 2),
#                    rep(1, num.edges), c(num.edges - 1), c('L'), NULL, NULL)
#       chgColTypeCPLEX(cplex.problem$env,
#                       cplex.problem$prob,
#                       length(rs$cpcs.ind.vars),
#                       c(rs$cpcs.ind.vars)-1,
#                       rep('B',length(rs$cpcs.ind.vars)))
#     }
#     
#     print("rounding")
#     chgColTypeCPLEX(cplex.problem$env,
#                     cplex.problem$prob,
#                     length(fract),
#                     fract-1,
#                     rep('B',length(fract)))
#     
#     setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIRCUTS, 2)
#     
#     #bak <- out$dag
#   }
#   return(list("dag"   = best.dag,
#               "cpcs"  = best.sol,
#               "score" = best.score))
}

create.cplex.problem <- function(name = "EdgeOrientation")
{
  env  <- openEnvCPLEX()
  prob <- initProbCPLEX(env, name)
  #chgProbNameCPLEX(env, prob, name)
  return(list("env"=env, "prob"=prob))
}

close.cplex.problem <- function(cplex.problem)
{
  delProbCPLEX(cplex.problem$env, cplex.problem$prob)
  closeEnvCPLEX(cplex.problem$env)
}

recompute.score.from.lp.sol <- function(graph, initialization)
{
  # given the graph with a fractional optimal solution, compute the candidate
  # parent set obtained with the selected edge indicator variables
  ps            <- NULL
  real.ind.vars <- NULL
  num.nodes     <- nrow(graph)
  scores        <- c(unlist(initialization$cpcs.scores))
  overall.score <- 0
  start.cpcs.vs <- initialization$start.cpcs.vars
  cpcs.vars     <- initialization$cpcs.vars
  offsets       <- c()
  for (i in 1:num.nodes)
  {
    offsets <- c(offsets, nrow(initialization$combinations[[i]]))
  }
  for (i in 1:num.nodes)
  {
    p       <- c(unlist(initialization$cpcs[[i]]))
    offset  <- 0
    if (length(p) > 0)
    {
      for (j in 1:length(p))
      {
        offset <- 2*offset + graph[p[j],i]
      }
    }
    # cat(i, " ", start.cpcs.vs[[i]] + offset, " ", scores[start.cpcs.vs[[i]] + offset],"\n")
    overall.score      <- overall.score + scores[start.cpcs.vs[[i]] + offset]
    real.ind.vars[[i]] <- start.cpcs.vs[[i]] + offset #cpcs.vars[[start.cpcs.vs[[i]] + offset]]
  }
  return(list("score"         = overall.score,
              "cpcs.ind.vars" = c(unlist(real.ind.vars))))
}

meek.orientation <- function(graph)
{
#   # orientation of edges according to the Meek rules
#   num.nodes <- nrow(graph)
#   for (i in 1:num.nodes)
#   {
#     ni <- unlist(which(graph[,i] > 0))
#     for (j in 1:length(ni))
#     {
#       nj <- unlist(which(graph[,ni[j]] > 0))
#       
#       for (k in 1:length(nj))
#       {
#         if (nj[k] == i) next; # undirected edge : cannot direct
#         if (graph[nj[k],ni[j]] == 1) next; # undirected edge : cannot direct
#         if (graph[i,nj[k]] == 1 && graph[nj[k],i] == 1) {
#           # undirected edge that can be directed with rule 2
#           graph[nj[k],i] == 0
#         }
#       }
#     }
#   }
}
