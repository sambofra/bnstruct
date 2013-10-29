library("bnstruct")
library("igraph")

# read data and set parameters
B <- 10
a <- read.delim("Child_data_na_5000.txt",na.strings="?",header=FALSE,sep="") + 1

node.sizes <- rep(0,ncol(a))
for( i in 1:ncol(a) )
  node.sizes[i] <- length(unique(a[,i]))

node.sizes <- c(2,6,3,2,3,4,3,3,2,2,3,3,5,2,2,3,3,2,5,2)
names(a) <- list("BirthAsphyxia","Disease","Age","LVH","DuctFlow","CardiacMixing",
                 "LungParench","LungFlow","Sick","HypDistrib","HypoxiaInO2","CO2",
                 "ChestXray","Grunting","LVHReport","LowerBodyO2","RUQO2","CO2Report",
                 "XrayReport","GruntingReport")
cont.nodes <- c()
k.impute <- 10
layering   <- c(1,2,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5)
max.fanin.layers <- as.matrix(read.table(header=F,text="
  0  1  1  1  1
  0  1  1  1  1
  0  0  8  7  7
  0  0  0 14  6
  0  0  0  0 19"))
max.fanin  <- 3 # threshold for the values of max.fanin.layers

# impute data 
impa <- knn.impute(as.matrix(a),k.impute,setdiff(1:length(node.sizes),cont.nodes))

# test SM alone
res.single <- sm(impa,node.sizes,cont.nodes,max.fanin,layering,max.fanin.layers)

#print(res.single)

# test SM with bootstrap
# res.boot <- boot.bn(as.matrix(a), node.sizes, B, cont.nodes, max.fanin, layering,
#                    max.fanin.layers,iss=1, verbose=TRUE, k.impute=10)

# print(res.boot)

junction.tree(res.single)

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
