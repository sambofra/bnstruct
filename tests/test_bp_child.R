library("bnstruct")

# child_NA_5000 <- BNDataset(name = "Child")
# child_NA_5000 <- read.dataset(child_NA_5000, header.file="../bnstruct/inst/extdata/Child_data_na_5000.header",
#                               data.file="../bnstruct/inst/extdata/Child_data_na_5000.data",
#                               imputation = TRUE, bootstrap=FALSE, num.boots=3)
# save(child_NA_5000, file="data/child_NA_5000.rda")
# break
# print(mydata)

mydata <- child()

net <- BN()
slot(net, "name") <- "Child"
slot(net, "num.nodes") <- 20
slot(net, "node.sizes") <- mydata@node.sizes
slot(net, "cpts") <- list(NULL)
slot(net, "variables") <- mydata@variables

# net <- learn.structure(net, mydata, algo="sm", layering= c(1,2,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5), max.fanin.layers=as.matrix(read.table(header=F,text="
#    0  1  1  1  1
#    0  1  1  1  1
#    0  0  8  7  7
#    0  0  0 14  6
#    0  0  0  0 19")), max.fanin=3)
print(net)
net <- learn.structure(net, mydata, "sm", scoring.func = "BDeu",
layering= c(1,2,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5), max.fanin.layers=as.matrix(read.table(header=F,text="
   0  1  1  1  1
   0  1  1  1  1
   0  0  8  7  7
   0  0  0 14  6
   0  0  0  0 19")), max.fanin=3, bootstrap = FALSE)
net <- learn.params(net, mydata)
# 
print(net)
#print(wpdag(net))
# lapply(net@cpts, sum)
# # readLines(file("stdin"),1)
# 
# print(net)

# readLines(file("stdin"),1)

inf.eng <- InferenceEngine(net)
# inf.eng <- build.junction.tree(inf.eng, net@dag)

print(inf.eng)
# readLines(file("stdin"),1)

observations(inf.eng) <- list(c(3,5,9,19), rep(2,4))
inf.eng <- belief.propagation(inf.eng)

marginals(inf.eng)

plot(net)
readLines(file("stdin"),1)
plot(updated.bn(inf.eng))

# em(inf.eng, child_NA_5000)

# print(bn(inf.eng))
# readLines(file("stdin"),1)
# print(updated.bn(inf.eng))
# readLines(file("stdin"),1)
