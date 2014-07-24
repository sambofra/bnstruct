library("bnstruct")

mydata <- BNDataset(name = "Child")
mydata <- read.dataset(mydata, "inst/extdata/Child_data_na_5000.header", "inst/extdata/Child_data_na_5000.data", imputation = TRUE, bootstrap=FALSE)
print(mydata)

net <- BN()
slot(net, "name") <- "Child"
slot(net, "num.nodes") <- 20
slot(net, "node.sizes") <- mydata@node.sizes
slot(net, "cpts") <- list(NULL)
slot(net, "variables") <- mydata@variables

net <- learn.structure(net, mydata, algo="sm", layering= c(1,2,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5), max.fanin.layers=as.matrix(read.table(header=F,text="
   0  1  1  1  1
   0  1  1  1  1
   0  0  8  7  7
   0  0  0 14  6
   0  0  0  0 19")), max.fanin=3)
# print(net)
net <- learn.structure(net, mydata, algo="mmhc")
net <- learn.params(net, mydata)
# 
print(net)
lapply(net@cpts, sum)
readLines(file("stdin"),1)

print(net)

readLines(file("stdin"),1)

inf.eng <- InferenceEngine()
inf.eng <- build.junction.tree(inf.eng, net@dag)

print(inf.eng)
readLines(file("stdin"),1)

jpts <- belief.propagation(inf.eng, net, c(3:20), rep(1,18))

print("-------------------------------------------------------------------------------------------")

print(jpts)
bak <- jpts

print(sum(c(unlist(jpts))))
unlist(lapply(jpts, sum))
readLines(file("stdin"),1)
