library("bnstruct")
library("igraph")
library("cplexAPI")
library("Rgraphviz")

# read data and set parameters

# a <- read.delim("Child_data_na_5000.txt",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/Alarm1_s1000_v1.txt",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/Barley_s5000_v3.txt",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/Insurance_s5000_v1.txt",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/Child_s5000_v1.txt",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/Gene_s5000_v3.txt",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/Water_100.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/onlydata_Mildew_10000.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/onlydata_Diabetes_1000.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/onlydata_alarm_1000.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/onlydata_insurance_1000.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/onlydata_asia_10000.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/onlydata_carpo_100.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/onlydata_hailfinder_1000.data",na.strings="?",header=FALSE,sep="") + 1
# a <- read.delim("networks/HailFinder_s1000_v1.txt",na.strings="?",header=FALSE,sep="") + 1 # ERROR

data <- read.dataset(filename = "networks/insurance_10000.data", imputation = FALSE)

num.nodes  <- data$num.nodes
node.sizes <- data$node.sizes
num.items  <- data$num.items
impa       <- data$dataset

print("Starting MMPC...")
cpcs.table <- mmpc(impa, node.sizes, chi.th=0.05)
print("MMPC done")
print(cpcs.table)

# cycles.found.out <- kl.bfs(cpcs.table, 3, 3, 2)
# num.cycles.found <- cycles.found.out$num.cycles
# cycles.found     <- cycles.found.out$cycles
# 
# print(num.cycles.found)
# print(cycles.found)
# 
# print("----------------------------")
# print(cpcs.table)
# print("----------------------------")

# cpcs.table <- array(c(0,1,0,1,0,1,0,0,
#                       1,0,1,0,1,0,0,0,
#                       0,1,0,0,0,0,0,1,
#                       1,0,0,0,1,0,1,0,
#                       0,1,0,1,0,0,0,1,
#                       1,0,0,0,0,0,1,0,
#                       0,0,0,1,0,1,0,1,
#                       0,0,1,0,1,0,1,0),c(8,8))

# cpcs.table <- array(c(0,1,0,1,0,1,0,0,
#                       0,0,1,0,1,0,0,0,
#                       0,0,0,0,0,0,0,1,
#                       0,0,0,0,1,0,1,0,
#                       0,0,0,0,0,0,0,1,
#                       0,0,0,0,0,0,1,0,
#                       0,0,0,0,0,0,0,1,
#                       1,0,0,0,0,0,0,0),c(8,8))

# cpcs.table <- array(c(0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0), c(4,4))
# cpcs.table <- array(c(0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0), c(4,4))
# 
# print(directed.cycle.finding(cpcs.table,4))
# 
# break

# plot.mat(cpcs.table)

cplex.problem  <- create.cplex.problem("orientation")

setIntParmCPLEX(cplex.problem$env, CPX_PARAM_SCRIND, CPX_ON);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_PRELINEAR, 0);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_PREIND, CPX_OFF);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIPCBREDLP, CPX_OFF);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIPSEARCH, CPX_MIPSEARCH_TRADITIONAL);
# presolveCPLEX(cplex.problem$env, cplex.problem$prob, CPX_ALG_NONE);

initialization <- orientation.init(cplex.problem, cpcs.table, impa)

storage.mode(num.nodes) <- "integer"
storage.mode(initialization$edge.vars) <- "integer"
storage.mode(initialization$start.cpcs.vars) <- "integer"
storage.mode(initialization$start.edge.vars) <- "integer"
storage.mode(initialization$num.vars) <- "integer"
storage.mode(initialization$num.cpcs.vars) <- "integer"

writeProbCPLEX(cplex.problem$env, cplex.problem$prob, "aaprob.lp")

# score <- .Call( "solve_BNSL_with_callbacks", cplexPointer(cplex.problem$env), cplexPointer(cplex.problem$prob),
#                 num.nodes*1L, initialization$cpcs, initialization$combinations, initialization$cpcs.vars, initialization$edge.vars,
#                 as.integer(c(initialization$start.cpcs.vars)-1L), as.integer(c(initialization$start.edge.vars)),
#                 as.integer(initialization$num.vars), as.integer(initialization$num.cpcs.vars), as.numeric(initialization$cpcs.scores), PACKAGE = "bnstruct" );
# 
# break

# remove <- decampos.ji.pruning(num.nodes, cplex.problem, initialization)

# sf <- sink.finding.reviewed(cpcs.table, initialization)
# print(sf)
#selvs <- sf$heur.solution

# new.dag <- matrix(rep(0, num.nodes*num.nodes), c(num.nodes, num.nodes))
# 
# for (i in 1:length(selvs))
# {
#   cpc <- c(unlist(initialization$cpcs.vars[[selvs[i]]]))
#   print(cpc)
#   new.dag[cpc,i] <- 1
#   #print(new.dag)
# }
# 
# # plot.mat(new.dag)
# 
# print(is.acyclic(new.dag))


#break

# print(initialization)
# print(initialization$start.cpcs.vars)
# print(initialization$start.edge.vars)
# build.bounding.constraint(20, 14, initialization$cpcs[[14]], c(1,1,0), 7, 17167, 17261)

# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_PREIND, CPX_OFF)

cycles.found.out <- kl.bfs(cpcs.table, 10, nrow(cpcs.table), 2)
num.cycles <- cycles.found.out$num.cycles
cycles     <- cycles.found.out$cycles
for (i in 1:num.cycles)
{
  cycle <- rev(c(unlist(cycles[[i]])))
  snv   <- getNumColsCPLEX(cplex.problem$env, cplex.problem$prob) + 1
  num.vars <- 2*length(cycle) # both for source&sink
  #obj <- rep(0, num.vars)
  #lb  <- rep(0, num.vars)
  #ub  <- rep(1, num.vars)
  #ctype <- rep('B', num.vars)
  #newColsCPLEX(cplex.problem$env, cplex.problem$prob, num.vars, obj, lb, ub, ctype, NULL)
  tmp   <- build.source.sink.constraints(cpcs.table, cycle, initialization, cplex.problem) 
}

nodes.involved <- unique(c(unlist(cycles)))
print(nodes.involved)
# casso <- readLines(file("stdin"),1)
old.rem.nodes <- c(1:num.nodes)
if (length(nodes.involved) < num.nodes)
{
  while(TRUE)
  {
    remaining.nodes <- setdiff(old.rem.nodes, nodes.involved)
    print(remaining.nodes)
    if (!(is.element(FALSE, is.element(remaining.nodes,old.rem.nodes)))) break
    old.rem.nodes < remaining.nodes
    #casso <- readLines(file("stdin"),1)
    cycles.found.out <- kl.bfs(cpcs.table, 10, nrow(cpcs.table), remaining.nodes[1])
    num.cycles <- cycles.found.out$num.cycles
    cycles     <- cycles.found.out$cycles
    for (i in 1:num.cycles)
    {
      cycle <- rev(c(unlist(cycles[[i]])))
      snv   <- getNumColsCPLEX(cplex.problem$env, cplex.problem$prob) + 1
      num.vars <- 2*length(cycle) # both for source&sink
      #obj <- rep(0, num.vars)
      #lb  <- rep(0, num.vars)
      #ub  <- rep(1, num.vars)
      #ctype <- rep('B', num.vars)
      #newColsCPLEX(cplex.problem$env, cplex.problem$prob, num.vars, obj, lb, ub, ctype, NULL)
      tmp   <- build.source.sink.constraints(cpcs.table, cycle, initialization, cplex.problem) 
    }
  }
}

# cycles.found.out <- kl.bfs(cpcs.table, 10, 3, 2)
# num.cycles <- cycles.found.out$num.cycles
# cycles     <- cycles.found.out$cycles
# for (i in 1:num.cycles)
# {
#   cycle <- rev(c(unlist(cycles[[i]])))
#   snv   <- getNumColsCPLEX(cplex.problem$env, cplex.problem$prob) + 1
#   num.vars <- 2*length(cycle) # both for source&sink
#   #obj <- rep(0, num.vars)
#   #lb  <- rep(0, num.vars)
#   #ub  <- rep(1, num.vars)
#   #ctype <- rep('B', num.vars)
#   #newColsCPLEX(cplex.problem$env, cplex.problem$prob, num.vars, obj, lb, ub, ctype, NULL)
#   tmp   <- build.source.sink.constraints(cpcs.table, cycle, initialization, cplex.problem) 
# }
# 
# nodes.involved <- unique(c(unlist(cycles)))
# print(nodes.involved)
# # casso <- readLines(file("stdin"),1)
# old.rem.nodes <- c(1:num.nodes)
# if (length(nodes.involved) < num.nodes)
# {
#   while(TRUE)
#   {
#     remaining.nodes <- setdiff(old.rem.nodes, nodes.involved)
#     print(remaining.nodes)
#     if (!(is.element(FALSE, is.element(remaining.nodes,old.rem.nodes)))) break
#     old.rem.nodes < remaining.nodes
#     #casso <- readLines(file("stdin"),1)
#     cycles.found.out <- kl.bfs(cpcs.table, 10, 3, remaining.nodes[1])
#     num.cycles <- cycles.found.out$num.cycles
#     cycles     <- cycles.found.out$cycles
#     for (i in 1:num.cycles)
#     {
#       cycle <- rev(c(unlist(cycles[[i]])))
#       snv   <- getNumColsCPLEX(cplex.problem$env, cplex.problem$prob) + 1
#       num.vars <- 2*length(cycle) # both for source&sink
#       #obj <- rep(0, num.vars)
#       #lb  <- rep(0, num.vars)
#       #ub  <- rep(1, num.vars)
#       #ctype <- rep('B', num.vars)
#       #newColsCPLEX(cplex.problem$env, cplex.problem$prob, num.vars, obj, lb, ub, ctype, NULL)
#       tmp   <- build.source.sink.constraints(cpcs.table, cycle, initialization, cplex.problem) 
#     }
#   }
# }

writeProbCPLEX(cplex.problem$env, cplex.problem$prob, "initial_prob.lp")

#mipoptCPLEX(cplex.problem$env, cplex.problem$prob)

# print(solutionCPLEX(cplex.problem$env, cplex.problem$prob))

out <- solve.and.full.rounding(num.nodes, cplex.problem, initialization)
print(out)

# solve.and.local.rounding(new.dag, cplex.problem, initialization)
# print(out)

# mipoptCPLEX(cplex.problem$env, cplex.problem$prob)
# sol <- solutionCPLEX(cplex.problem$env, cplex.problem$prob)
# #stat <- sol$lpstat
# x <- sol$x
# z <- sol$objval
# num.nodes <- nrow(cpcs.table)
# evs <- x[initialization$start.edge.vars:(initialization$start.edge.vars+num.nodes*num.nodes-1)]
# evs   <- matrix(lapply(evs, FUN = function(x){
#   if(x >= 0.999){
#     x <- 1
#   } else if(x <= 0.001){
#     x <- 0
#   }
# }
# ), c(num.nodes, num.nodes))
# storage.mode(evs) <- "integer"
# dir.g <- matrix(evs, c(num.nodes, num.nodes), byrow=TRUE)


close.cplex.problem(cplex.problem)

print(out$dag)
print(out$score)

dag <- out$dag

rs <- recompute.score.from.lp.sol(dag, initialization)
print(rs$score)

# 
# print(initialization$cpcs.vars[sf$heur.solution])

#tcc <- tarjan.connected.components(out$dag)

# dag <- t(matrix(c(0,0,1,1,0,0,
#                   1,0,0,0,0,0,
#                   0,0,0,0,1,0,
#                   0,0,0,0,0,1,
#                   0,1,0,0,0,0,
#                   0,1,0,0,0,0),c(6,6)))

#tcc <- tarjan.connected.components(dag)

plot.mat(dag)

#print(c(unlist(x)))

#print(shd(bak, dir.g))

# print("..... sm")
# 
# res.single <- sm(impa,node.sizes,cont.nodes,max.fanin,layering,max.fanin.layers)
# print(shd(res.single,dag))
# 
# rs <- recompute.score.from.lp.sol(res.single, initialization)
# print(rs$score) # 61534.39
# print(rs$score - out$score)
# cat("comeprima = ",comeprima,"\n")

# table <- array(c(0,1,0,1,0,1,0,0,
#                  1,0,1,0,1,0,0,0,
#                  0,1,0,0,0,0,0,1,
#                  1,0,0,0,1,0,1,0,
#                  0,1,0,1,0,0,0,1,
#                  1,0,0,0,0,0,1,0,
#                  0,0,0,1,0,1,0,1,
#                  0,0,1,0,1,0,1,0),c(8,8))

#print(kl.bfs(table, 3, 7, 1))
