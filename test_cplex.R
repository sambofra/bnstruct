library("bnstruct")
library("igraph")
library("cplexAPI")
library("Rgraphviz")

# read data and set parameters

args       <- commandArgs(trailingOnly = TRUE)
input.file <- args[1]

instance.name <- unlist(strsplit(input.file, "\\."))[1]
data          <- read.dataset(filename = input.file, imputation = FALSE)

num.nodes  <- data$num.nodes
node.sizes <- data$node.sizes
num.items  <- data$num.items
impa       <- data$dataset

print("Starting MMPC...")
cpcs.table <- mmpc(impa, node.sizes, chi.th=0.05)
print("MMPC done")
print(cpcs.table)

# cont.nodes = c()
# ptm <- proc.time()
# res.mmhc <- hc( impa, node.sizes, cpcs.table, cont.nodes )
# print("time")
# print(proc.time()-ptm)
# print(res.mmhc)
# print(graph.to.factors(res.mmhc, '['))
# cplex.problem  <- create.cplex.problem("orientation")
# initialization <- orientation.init(cplex.problem, res.mmhc, impa)
# rsmmhc <- recompute.score.from.lp.sol(res.mmhc, initialization)
# print(rsmmhc$score)
# break


cplex.problem  <- create.cplex.problem("orientation")

# disable CPLEX preprocessing
setIntParmCPLEX(cplex.problem$env, CPX_PARAM_SCRIND, CPX_ON);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_PRELINEAR, 0);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_PREIND, CPX_OFF);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIPCBREDLP, CPX_OFF);
# setIntParmCPLEX(cplex.problem$env, CPX_PARAM_MIPSEARCH, CPX_MIPSEARCH_TRADITIONAL);
# presolveCPLEX(cplex.problem$env, cplex.problem$prob, CPX_ALG_NONE);

initialization <- orientation.init(cplex.problem, cpcs.table, impa)

save.scores(instance.name, num.nodes, c(initialization$cpcs.scores), initialization$cpcs.vars,
                        c(initialization$start.cpcs.vars, initialization$start.edge.vars))

storage.mode(num.nodes)                      <- "integer"
storage.mode(initialization$edge.vars)       <- "integer"
storage.mode(initialization$start.cpcs.vars) <- "integer"
storage.mode(initialization$start.edge.vars) <- "integer"
storage.mode(initialization$num.vars)        <- "integer"
storage.mode(initialization$num.cpcs.vars)   <- "integer"

writeProbCPLEX(cplex.problem$env, cplex.problem$prob, "aaprob.lp")

# look for large cycles in the skeleton
# cycles.found.out <- kl.bfs(cpcs.table, 10, nrow(cpcs.table), 2)#nrow(cpcs.table), 2)
# num.cycles <- cycles.found.out$num.cycles
# cycles     <- cycles.found.out$cycles
# if (num.cycles > 0)
# {
#   for (i in 1:num.cycles)
#   {
#     cycle <- rev(c(unlist(cycles[[i]])))
#     if(length(cycle) > 0)
#       tmp   <- build.source.sink.constraints(cpcs.table, cycle, initialization, cplex.problem) 
#   }
# }
# 
# nodes.involved <- unique(c(unlist(cycles)))
# print(nodes.involved)
# print(length(nodes.involved))
# # readLines(file("stdin"),1)
# old.rem.nodes <- c(1:num.nodes)
# if (length(nodes.involved) < num.nodes)
# {
#   while(TRUE)
#   {
#     remaining.nodes <- setdiff(old.rem.nodes, nodes.involved)
#     print(remaining.nodes)
#     if (!(is.element(FALSE, is.element(remaining.nodes,old.rem.nodes)))) break
#     old.rem.nodes < remaining.nodes
#     #readLines(file("stdin"),1)
#     cycles.found.out <- kl.bfs(cpcs.table, 10, nrow(cpcs.table), remaining.nodes[1])#nrow(cpcs.table), remaining.nodes[1])
#     num.cycles <- cycles.found.out$num.cycles
#     cycles     <- cycles.found.out$cycles
#     if (num.cycles > 0)
#     {
#       for (i in 1:num.cycles)
#       {
#         cycle <- rev(c(unlist(cycles[[i]])))
#         if (length(cycle) > 0)
#           tmp   <- build.source.sink.constraints(cpcs.table, cycle, initialization, cplex.problem) 
#       }
#     }
#   }
# }

writeProbCPLEX(cplex.problem$env, cplex.problem$prob, "initial_prob.lp")

out <- solve.cplex.callbacks(num.nodes, cplex.problem, initialization)

#plot.mat(out$dag)

print("stats & fun facts:")
cat("# nodes:     ", num.nodes,"\n")
cat("# cpcs vars: ", initialization$start.edge.vars - 1,"\n")
cat("# edge vars: ", num.nodes*num.nodes,"\n")
cat("net: ", graph.to.factors(out$dag, sep = '['),"\n")

break
