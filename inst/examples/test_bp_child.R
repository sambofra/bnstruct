library("bnstruct")

# child_NA_5000 <- BNDataset(name = "Child")
# child_NA_5000 <- read.dataset(child_NA_5000, header.file="../bnstruct/inst/extdata/Child_data_na_5000.header",
#                               data.file="../bnstruct/inst/extdata/Child_data_na_5000.data",
#                               imputation = TRUE, bootstrap=FALSE, num.boots=3)
# save(child_NA_5000, file="data/child_NA_5000.rda")
# break
# print(mydata)

mydata <- child()

net <- BN(mydata)
print(net)
print(dag(net))
out <- learn.network(net, mydata, algo = "sem", scoring.func = "BDeu", struct.threshold = 0)
