eocp <- function(data, node.sizes, scoring.func, cont.nodes, alpha, layering, params)
{
  num.nodes  <- length(node.sizes)  
  cpcs.table <- mmpc(data, node.sizes, cont.nodes, alpha, layering)
  
  cplex.problem  <- create.cplex.problem("orientation")
  
  # disable CPLEX preprocessing
  setIntParmCPLEX(cplex.problem$env, CPX_PARAM_SCRIND, params@CPX_PARAM_SCRIND);
  setIntParmCPLEX(cplex.problem$env, CPX_PARAM_PRELINEAR, params@CPX_PARAM_PRELINEAR);
  setIntParmCPLEX(cplex.problem$env, CPX_PARAM_PREIND, params@CPX_PARAM_PREIND);
  setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIPCBREDLP, params@CPX_PARAM_MIPCBREDLP);
  setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIPSEARCH, params@CPX_PARAM_MIPSEARCH);
  presolveCPLEX(cplex.problem$env, cplex.problem$prob, params@cplex_presolve_algo);
  
  initialization <- orientation.init(cplex.problem, cpcs.table, data, node.sizes, scoring.func)
  
#   save.scores(instance.name,
#               num.nodes, c(initialization$cpcs.scores), initialization$cpcs.vars,
#               c(initialization$start.cpcs.vars, initialization$start.edge.vars))
  
  storage.mode(num.nodes)                      <- "integer"
  storage.mode(initialization$edge.vars)       <- "integer"
  storage.mode(initialization$start.cpcs.vars) <- "integer"
  storage.mode(initialization$start.edge.vars) <- "integer"
  storage.mode(initialization$num.vars)        <- "integer"
  storage.mode(initialization$num.cpcs.vars)   <- "integer"
  
  out <- solve.cplex.callbacks(num.nodes, cplex.problem, initialization)

  return(out$dag)
}

orientation.init <- function(cplex.problem, graph, data, node.sizes, scoring.func = 0)
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
  cpcs.vars     <- NULL
  cpcs.scores   <- NULL
  combinations  <- NULL
  offsets       <- c()
  start.cpcs.vs <- c()
  k             <- 1
  temp.sum      <- 1
  last.start    <- 1
  overall.dcj   <- c()
  total.counter <- 0
  
  # iterate through nodes
  for(i in 1:ncol(graph))
  {
    print(paste("- evaluating node",i))
    # combinations[[i]]       <- as.list(as.list(1), as.list(2)) # sporcotrucco^(tm), otherwise may fail abruptly if first node is disconnected
    cpcs[[i]]               <- as.list(c(which(graph[,i] > 0)))
    storage.mode(cpcs[[i]]) <- "integer"
    
    len.cpc   <- length(cpcs[[i]])
    num.combs <- 2 ** len.cpc
    
    # combinations            <- fast.bincombinations(length(cpcs[[i]]))
    
    cat("  (has",len.cpc, "possible parents, and therefore up to", num.combs ,"candidate parent sets to evaluate)\n")
    
    cpcs.vars[[k]] <- as.list(c())
    
    # local.is.here contains the list of subsets in the current round that have not been pruned by the De Campos & Ji pruning
    local.is.here  <- c()
    local.cnt      <- 0
    
    bigger.cps.size <- 0
    
    # empty set: treat apart
    comb   <- rep(0, len.cpc)
    subset <- c(cpcs[[i]][which(comb > 0)])
    storage.mode(subset) <- "integer"
    combinations[[k]]    <- as.list(unlist(comb))
    cpcs.vars[[k]]       <- as.list(c(unlist(subset)))
    storage.mode(cpcs.vars[[k]]) <- "integer"
    storage.mode(data) <- "integer"
    storage.mode(node.sizes) <- "integer"
    score        <- .Call( "score_node", data, node.sizes, i-1L, subset-1L, 
                           scoring.func*1L, ess = 1, PACKAGE = "bnstruct" );
    local.is.here    <- c(local.is.here, 1)
    local.cnt        <- local.cnt + 1
    cpcs.scores[[k]] <- score
    k <- k + 1
    
    inserted.in.last.round <- 1
    to.be.inserted <- 0
    
    if (len.cpc > 0)
    {
      for (j in 1L:len.cpc)
      {
        print(j)
        start.num = 2 ** j - 1
        last.num  = 2 ** len.cpc - 1 - (2 ** (len.cpc - j) - 1) + 1
        curr.num  = start.num
        
        inserted.in.last.round <- 0
        
        while (curr.num <= last.num)
        {
          comb <- as.integer(rev(intToBits(curr.num)[c(1:len.cpc)]))
          subset <- c(cpcs[[i]][which(comb > 0)])
          #cat(curr.num, " ", comb, " ", subset , " \n")
          storage.mode(subset) <- "integer"
          # insert: if is not the case, it will be overwritten
          combinations[[k]] <- as.list(unlist(comb))
          cpcs.vars[[k]] <- as.list(c(unlist(subset)))
          storage.mode(cpcs.vars[[k]]) <- "integer"
          
          # look for already discarded subsets: then the variable can already be discarded
          flag <- 1
          discard <- 0
          missed  <- 0
          for (l in 1:length(comb))
          {
            if (comb[l] > 0)
            {
              offset <- 0
              for (m in 1:length(comb))
              {
                if (m == l)
                  offset <- 2*offset
                else
                  offset <- 2*offset + comb[m]
              }
              offset <- offset + 1
              if (!is.element(offset, local.is.here))
                missed <- missed + 1
            }
          }
          if (missed*1L == j)
          {
            flag <- 0
            discard <- 1
            #break
          }
          if (flag == 1)
          {
            # if CPC cannot be discarded yet, compute its score, and evaluate against all of its subsets
            score <- .Call( "score_node", data, node.sizes, i-1L, subset-1L, 
                            scoring.func, ess = 1, PACKAGE = "bnstruct" );
            {
              if (comb[l] > 0)
              {
                offset  <- 0
                discard <- 0
                for (m in 1:length(comb))
                {
                  if (m == l)
                    offset <- 2*offset
                  else
                    offset <- 2*offset + comb[m]
                }
                offset <- offset + 1
                if (is.element(offset, local.is.here))
                {
                  check.at <- which(local.is.here == offset)
                  if (cpcs.scores[[check.at+last.start-1]] > score)# && j >= 2L)
                  {
                    discard <- 1
                    break
                  }
                }
              }
            } 
            if (discard == 0)
            {
              to.be.inserted <- 1
            }
          }
          
          if (to.be.inserted == 1)
          {
            local.is.here    <- c(local.is.here, curr.num+1)
            local.cnt        <- local.cnt + 1
            cpcs.scores[[k]] <- score
            k <- k + 1
            to.be.inserted <- 0
            inserted.in.last.round <- inserted.in.last.round + 1
          }
          
          curr.num = .Call("nextLexicalBitComb", as.integer(curr.num*1L), PACKAGE = "bnstruct")*1L
        }

        cat(inserted.in.last.round, "/", choose(len.cpc,j), "\n")
        if (inserted.in.last.round == 0)
        {
          j <- len.cpc + 1
          break
        }
      }
    }

    start.cpcs.vs <- c(start.cpcs.vs, last.start*1L)
    
    total.counter <- total.counter + local.cnt
    last.start    <- last.start + local.cnt
    offsets       <- c(offsets, local.cnt)
  }
  
  start.edge.vs <- last.start*1L
  num.vars      <- (total.counter + num.nodes * num.nodes)*1L
  storage.mode(start.cpcs.vs) <- "integer"
  
  print("done")
  
  print("populating problem...")
  env  <- cplex.problem$env
  prob <- cplex.problem$prob
  # num of columns : variables
  nc <- num.vars # remember that R counts from 1, cplex from 0... 
  # num of rows : constraints : nothing for now, to be added with addRows
  nr <- 1
  # lower bounds = 0 for all
  lb <- c(rep(0, num.vars))
  # upper bounds = 1 for all; then set to 0 the ub for the edges forbidden by the MMPC step
  ub <- c(rep(1, num.vars))
  ub[which(edge.ind.vars == 0) + start.edge.vs - 1] <- 0
  
  # objective function coefficients: the scores associated to I(W->v) vars
  obj <- c(c(unlist(cpcs.scores)), rep(0, num.nodes * num.nodes))

  # it's a max problem
  setObjDirCPLEX(env, prob, CPX_MAX)
    
  # create CPLEX variables (B/B or C/B)
  ctype = c(rep('B', total.counter), rep('B', num.nodes*num.nodes))
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

  # constraints I(W->v) = prod...
  print(" - adding bounding constraints...")
  for (i in 1:num.nodes)
  {
    nnz <- length(cpcs[[i]]) + 1
    #print(paste(i, " ", nrow(combs[[i]])))
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
      for (j in 1:offsets[i])
      {
        tmp <- build.bounding.constraint(num.nodes, i, cpcs[[i]],combinations[[j+start.cpcs.vs[i]-1]],j,
                                         start.cpcs.vs[[i]], start.edge.vs)
        addRowsCPLEX(env, prob, 0, 1, nnz, tmp$beg, tmp$ind, tmp$uval,
                     tmp$urhs, tmp$usen, NULL, NULL)
        addRowsCPLEX(env, prob, 0, 1, nnz, tmp$beg, tmp$ind, tmp$lval,
                     tmp$lrhs, tmp$lsen, NULL, NULL)          
      }
      # need to impose at least one candidate parent set for each node
      at.least.one <- c(1:offsets[i]) + start.cpcs.vs[[i]] - 1
      addRowsCPLEX(env, prob, 0, 1, length(at.least.one), c(0), at.least.one-1, rep(1, length(at.least.one)),
                   c(1), c('E'), NULL, NULL) # only one
    }
  }
  print("   bounding constraints added")
  
  print("done")

  cpcs.scores <- c(unlist(cpcs.scores))
  
  return(list("cpcs"            = cpcs,
              "combinations"    = combinations,
              "offsets"         = offsets,
              "cpcs.vars"       = cpcs.vars,
              "edge.vars"       = edge.ind.vars,
              "start.cpcs.vars" = start.cpcs.vs, # remember: start counting from 1
              "start.edge.vars" = start.edge.vs, # idem
              "num.vars"        = num.vars,
              "num.cpcs.vars"   = total.counter,
              "cpcs.scores"     = cpcs.scores))
}

decampos.ji.pruning <- function(num.nodes, cplex.problem, initialization)
{
  # DeCampos&Ji pruning _after_ all the initialization: not used anymore
  # for every CPC, compare against all of its subsets
  print("De Campos & Ji pruning")
  remove  <- c()
  offsets <- initialization$offsets
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

build.bounding.constraint <- function(num.nodes, node, cpc, comb,
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
  cpc <- c(unlist(cpc))
  comb <- c(unlist(comb))
  if (is.element(FALSE, is.element(1, comb))) # weird, but...
    combination <- c(unlist(rep(0, length(cpc))))
  else
    combination <- comb
  tmp.ind <- c()
  tmp.val <- c()
  if (length(cpc) >= 1)
    for (i in 1:length(cpc))
    {
      tmp.ind <- c(tmp.ind, (cpc[i]-1)*num.nodes+node + start.edge.vs - 1)
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
      evs <- x[start.edge.vars:length(x)]
      iwv <- x[1:(start.edge.vars-1)]
        
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
      #print(dir.g)
      
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

solve.cplex.callbacks <- function(num.nodes, cplex.problem, initialization)
{
  final <- .Call( "solve_BNSL_with_callbacks",
                  cplexPointer(cplex.problem$env),
                  cplexPointer(cplex.problem$prob),
                  num.nodes*1L,
                  initialization$cpcs,
                  initialization$combinations,
                  initialization$cpcs.vars,
                  initialization$edge.vars,
                  as.integer(c(initialization$start.cpcs.vars)-1L),
                  as.integer(c(initialization$start.edge.vars)),
                  as.integer(initialization$num.vars),
                  as.integer(initialization$num.cpcs.vars),
                  as.numeric(initialization$cpcs.scores),
                  PACKAGE = "bnstruct" );
    
  final.cpcs <- final[c(1:(initialization$start.edge.vars-1))]
  final.dag  <- matrix(c(final[(c(1:(num.nodes*num.nodes)) + initialization$start.edge.vars - 1)]),
                       nrow = num.nodes, ncol = num.nodes, byrow = TRUE)
  
  # print(which(final.cpcs == 1))
  final.score <- sum(initialization$cpcs.scores[which(final.cpcs == 1)])
  
  return(list("dag"   = final.dag,
              "score" = final.score,
              "solx"  = final))
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
  offsets       <- initialization$offsets
  
  
  for (i in 1:num.nodes)
  {
    selected.ps   <- c(which(graph[,i] > 0))

    for (j in 1:offsets[i])
    {
      if (!is.element(FALSE, is.element(selected.ps, c(unlist(cpcs.vars[[j + start.cpcs.vs[[i]] -1]])))) &&
          !is.element(FALSE, is.element(c(unlist(cpcs.vars[[j + start.cpcs.vs[[i]] -1]])), selected.ps)))
      {
        overall.score      <- overall.score + scores[start.cpcs.vs[[i]] + j - 1]
        real.ind.vars[[i]] <- start.cpcs.vs[[i]] + j - 1 #cpcs.vars[[start.cpcs.vs[[i]] + offset]]
      }
    }
  }
  return(list("score"         = overall.score,
              "cpcs.ind.vars" = c(unlist(real.ind.vars))))
}
