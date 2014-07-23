library("bnstruct")
library("igraph")
#library("gRain")

# read data and set parameters

args       <- commandArgs(trailingOnly = TRUE)
input.file <- args[1]

instance.name <- unlist(strsplit(input.file, "\\."))[1]
data          <- read.dataset(filename = input.file, imputation = FALSE)

num.nodes  <- data$num.nodes
node.sizes <- data$node.sizes
num.items  <- data$num.items
impa       <- data$dataset

cpcs.table <- mmpc(impa, node.sizes, chi.th=0.05)
res.mmhc <- hc( impa, node.sizes, cpcs.table, c() )

res.mmhc  <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 1, 0, 0,
                      0, 0, 0, 1, 1, 0, 0, 0,
                      0, 0, 0, 0, 0, 1, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 1,
                      0, 0, 0, 0, 0, 0, 1, 1,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0), nrow = 8, ncol = 8, byrow=TRUE)

print(res.mmhc)
jt <- junction.tree(res.mmhc)

# jjpts <- multinomial.map(impa, node.sizes, res.mmhc, ess=1)
# jpts <- jjpts$jpts
# print(jpts)
# print("marginals")
# print(jjpts$marginals)


dim.vars <- NULL
dim.vars[[1]] <- as.list(c(1))
dim.vars[[2]] <- as.list(c(2,1))
dim.vars[[3]] <- as.list(c(3))
dim.vars[[4]] <- as.list(c(4,3))
dim.vars[[5]] <- as.list(c(5,3))
dim.vars[[6]] <- as.list(c(6,2,4))
dim.vars[[7]] <- as.list(c(7,6))
dim.vars[[8]] <- as.list(c(8,5,6))

cpts <- NULL
cpts[[1]] <- array(c(0.01, 0.99), dim=c(1,2), dimnames=c(unlist(dim.vars[[1]])))
cpts[[2]] <- array(c(0.05, 0.95, 0.01, 0.99), dim=c(2,2), dimnames=c(unlist(dim.vars[[2]])))
cpts[[3]] <- array(c(0.5, 0.5), dim=c(1,2), dimnames=c(unlist(dim.vars[[3]])))
cpts[[4]] <- array(c(0.1, 0.9, 0.01, 0.99), dim=c(2,2), dimnames=c(unlist(dim.vars[[4]])))
cpts[[5]] <- array(c(0.6, 0.4, 0.3, 0.7), dim=c(2,2), dimnames=c(unlist(dim.vars[[5]])))
cpts[[6]] <- array(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), dim=c(2,2,2), dimnames=c(unlist(dim.vars[[6]])))
cpts[[7]] <- array(c(0.98, 0.02, 0.05, 0.95), dim=c(2,2), dimnames=c(unlist(dim.vars[[7]])))
cpts[[8]] <- array(c(0.9, 0.1, 0.8, 0.2, 0.7, 0.3, 0.1, 0.9), dim=c(2,2,2), dimnames=c(unlist(dim.vars[[8]])))



#dimnames(cpts[[1]]) <- dim.vars[[1]]

# for (i in 1:6)
#   print(dimnames(cpts[[i]]))
# 
# readLines(file("stdin"),1)

res.mmhc <- matrix(c(0,1,1,0,0,0,
                     0,0,0,1,0,0,
                     0,0,0,0,1,0,
                     0,0,0,0,0,1,
                     0,0,0,0,0,1,
                     0,0,0,0,0,0), nrow=6, ncol=6, byrow=TRUE)

node.sizes <- c(2,2,2,2,2,2)

dim.vars <- NULL
dim.vars[[1]] <- as.list(c(1))
dim.vars[[2]] <- as.list(c(2,1))
dim.vars[[3]] <- as.list(c(3,1))
dim.vars[[4]] <- as.list(c(4,2))
dim.vars[[5]] <- as.list(c(5,3))
dim.vars[[6]] <- as.list(c(6,4,5))

dim.names <- c("A", "B", "C", "D", "E", "F")

cpts <- NULL
cpts[[1]] <- array(c(0.1, 0.9), dim=c(2), dimnames=list(c(1,2)))
names(dimnames(cpts[[1]])) <- c("A")
cpts[[2]] <- array(c(0.1, 0.9, 0.9, 0.1), dim=c(2,2), dimnames=list(c(1,2), c(1,2)))
names(dimnames(cpts[[2]])) <- c("B","A")
cpts[[3]] <- array(c(0.7, 0.3, 0.2, 0.8), dim=c(2,2), dimnames=list(c(1,2), c(1,2)))
names(dimnames(cpts[[3]])) <- c("C","A")
cpts[[4]] <- array(c(0.4, 0.6, 0.7, 0.3), dim=c(2,2), dimnames=list(c(1,2), c(1,2)))
names(dimnames(cpts[[4]])) <- c("D","B")
cpts[[5]] <- array(c(0.5, 0.5, 0.4, 0.6), dim=c(2,2), dimnames=list("E" = c(1,2), "C"=c(1,2)))
cpts[[6]] <- array(c(0.1, 0.9, 0.4, 0.6, 0.5, 0.5, 0.8, 0.2), dim=c(2,2,2), dimnames=list("F" = c(1,2), "D"=c(1,2), "E"=c(1,2)))
  
# cpts <- NULL
# cpts[[1]] <- array(c(0.1, 0.9), dim=c(1,2))
# cpts[[2]] <- array(c(0.1, 0.9, 0.9, 0.1), dim=c(2,2))
# cpts[[3]] <- array(c(0.7, 0.3, 0.2, 0.8), dim=c(2,2))
# cpts[[4]] <- array(c(0.4, 0.6, 0.7, 0.3), dim=c(2,2))
# cpts[[5]] <- array(c(0.5, 0.5, 0.4, 0.6), dim=c(2,2))
# cpts[[6]] <- array(c(0.1, 0.9, 0.4, 0.6, 0.5, 0.5, 0.8, 0.2), dim=c(2,2,2))

print(cpts)
readLines(file("stdin"),1)

jt$triangulated.graph <- array(c(0,1,1,0,0,0,
                                 1,0,1,1,0,0,
                                 1,1,0,1,1,0,
                                 0,1,1,0,1,1,
                                 0,0,1,1,0,1,
                                 0,0,0,1,1,0), dim=c(6,6))

jt$jtree <- array(c(0,2,0,0,
                    2,0,2,0,
                    0,2,0,2,
                    0,0,2,0),dim=c(4,4))

jt$cliques <- list(as.list(c(1,2,3)),
                   as.list(c(2,3,4)),
                   as.list(c(3,4,5)),
                   as.list(c(4,5,6)))


jpts <- belief.propagation(jt$triangulated.graph, jt$jtree, jt$cliques, cpts, dim.names, c(), c(), node.sizes)

print("ciao")
for (i in 1:6)
{
  print(bp.query(jpts, jt$cliques, paste(i)))
}

break

###########################################################à
#
# trying CPTs


# node.sizes <- c(2,2,2,2,2,2)
# example.am <- matrix(c(0,1,1,0,0,0,
#                        0,0,0,1,0,0,
#                        0,0,0,0,1,0,
#                        0,0,0,0,0,1,
#                        0,0,0,0,0,1,
#                        0,0,0,0,0,0),
#                      nrow = 6, ncol = 6, byrow = TRUE)
# 
# # construct sample CPTs
# cpts <- list()
# for (i in 1:nrow(example.am))
# {
#   dims <- c(node.sizes[i])
#   how.many.values <- node.sizes[i]
#   for (j in which(example.am[,i] > 0))
#   {
#     dims <- c(dims, node.sizes[j])
#     # print(paste(i, dims))
#     how.many.values <- how.many.values * node.sizes[j]
#   }
#   cpts[[i]] <- array(c(rep(0, how.many.values)), dims)
# }
# 
# cpts[[1]][[1]] <- 0.1
# cpts[[1]][[2]] <- 0.9
# 
# cpts[[2]][[1,1]] <- 0.1
# cpts[[2]][[1,2]] <- 0.8
# cpts[[2]][[2,1]] <- 0.9
# cpts[[2]][[2,2]] <- 0.2
# 
# cpts[[3]][[1,1]] <- 0.7
# cpts[[3]][[1,2]] <- 0.2
# cpts[[3]][[2,1]] <- 0.3
# cpts[[3]][[2,2]] <- 0.8
# 
# cpts[[4]][[1,1]] <- 0.4
# cpts[[4]][[1,2]] <- 0.7
# cpts[[4]][[2,1]] <- 0.6
# cpts[[4]][[2,2]] <- 0.3
# 
# cpts[[5]][[1,1]] <- 0.5
# cpts[[5]][[1,2]] <- 0.4
# cpts[[5]][[2,1]] <- 0.5
# cpts[[5]][[2,2]] <- 0.6
# 
# cpts[[6]][[1,1,1]] <- 0.1
# cpts[[6]][[1,1,2]] <- 0.5
# cpts[[6]][[1,2,1]] <- 0.9
# cpts[[6]][[1,2,2]] <- 0.5
# cpts[[6]][[2,1,1]] <- 0.4
# cpts[[6]][[2,1,2]] <- 0.8
# cpts[[6]][[2,2,1]] <- 0.6
# cpts[[6]][[2,2,2]] <- 0.2

names <- c('a', 'b', 'c', 'd', 'e', 'f')
pnames <- c(~a, ~b|a, ~c|a, ~d|b, ~e|c, ~f|d:e)

yn <- c("yes", "no")

cpts <- NULL
cpts[[1]] <- cptable(names[1], values = c(10, 90), levels = yn)
cpts[[2]] <- cptable(paste(names[2],',',names[1]), values = c(10, 90, 90, 10), levels = yn)
# order of values: b|a, !b|a, b|!a, !b|!a
#cpts[[2]] <- cptable(~b|a, values = c(10, 90, 90, 10), levels = yn)
print(cpts[[2]])
print(as.name(paste(names[2],'+',names[1])))
cpts[[3]] <- cptable(~c|a, values = c(70, 30, 20, 80), levels = yn)
cpts[[4]] <- cptable(~d|b, values = c(40, 60, 70, 30), levels = yn)
cpts[[5]] <- cptable(~e|c, values = c(50, 50, 40, 60), levels = yn)
cpts[[6]] <- cptable(~f|d:e, values = c(10, 90, 50, 50, 40, 60, 80, 20), levels = yn)

plist <- compileCPT(cpts)
print(plist)

# print(ls(plist))
# for (i in 1:length(ls(plist)))
#   print(plist[[i]])

# print(plist$a)
# print(plist$b)

net <- grain(plist)

print(querygrain(net, nodes=c("c", "f"), type="joint"))

#for (i in 1:length(plist))
#  print(plist$names[i])

# 
# s <- c(1,2,3)
# dims <- c(2,2,2)
# # cc <- c()
# # for (i in c(1,4,5,6))
# # {
# #   print(dim(cpts[[i]]))
# # #   for (j in 1:node.sizes[i])
# # #   {
# # #     cc <- c(cc, cpts[[i]]%*%cpts[[j]])
# # #   }
# # }
# # cc <- array(cc, dims)
# # print(cc)
# 
# processed.cpts <- c(rep(0,length(dim)))
# nodes.cl <- c(1,2,3)
# deps <- NULL
# for (i in 1:length(nodes.cl))
# {
#   deps[[i]] <- reconstruct.dependencies.in.clique(nodes.cl[i], nodes.cl, example.am)
# }
# print("dependencies")
# print(deps)
# procd <- NULL
# procd.counter <- 0
# # for every node in the clique
# for (i in 1:length(nodes.cl))
# {
#   # for every direct ancestor of it
#   for (j in 1:length(unlist(deps[[i]])))
#   {
#     # if ancestor has not been processed yet, and therefore its multiplicative
#     # contribution has already been inserted, multiply respective columns.
#     # Assumes for now only ONE ancestor per node and no marginalized variables.
#     if (length(intersect(j,procd)) == 0)
#     {
#       s <- intersect(j,procd)
#       for (k in 1:length(s))
#       {
#         
#       }
#       procd.counter <- procd.counter + 1
#       procd[[i]] <- deps[[i]]
#     }
#   }
# }
# 
# print("procd")
# print(procd)


# nc <- NULL
# cc <- unlist(t(cpts[[nodes.cl[1]]]))
# processed <- c(1)
# for (i in 2:length(nodes.cl))
# {
#   print("---------------------------------------------------")
#   print(i)
#   print("cc")
#   print(cc)
#   print(nc)
#   print("...")
#   print(c(unlist(t(cpts[[nodes.cl[i]]]))))
#   print("...")
#   print(length(intersect(deps[[i]],processed)))
#   if(length(intersect(deps[[i]],processed)) == 0)
#   {
#     for (j in cc)
#     {
#       for (k in c(unlist(t(cpts[[nodes.cl[i]]]))))
#       {
#         nc <- c(nc, j * k)
#         print(paste(j,k,j*k))
#       }
#     }
#     processed <- c(processed, i)
#   }
#   else
#   {
#     for (j in cc)
#     {
#       for (k in c(unlist(t(cpts[[nodes.cl[i]]]))))
#       {
#         nc <- c(nc, j * k)
#         print(paste(j,k,j*k))
#       }
#     }
#     processed <- c(processed, i)
# #     for (h in 1:length(intersect(deps[[i]],processed)))
# #     {
# #       div.by <- c(rep(unlist(t(cpts)), length(nc)/length(unlist(cpts[[i]]))))
# #       nc <- nc / div.by
# #     }
#   }
#   cc <- nc
#   nc <- NULL
#   print("cc")
#   print(cc)
#   print("nc")
#   print(nc)
# }
# 
# new.cpt <- array(cc, dims)
# print(new.cpt)

# print("cpts")
# print(cpts)
# print("end cpts")

#print(example.am)

#junction.tree(example.am, cpts)
#junction.tree(res.single)
