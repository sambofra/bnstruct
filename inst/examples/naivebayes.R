library(bnstruct)

spam <- sample(c(0,1), 1000, prob=c(0.5, 0.5), replace=T)
buy <- sapply(spam, function(x) {if (x == 0) {sample(c(0,1),1,prob=c(0.8,0.2),replace=T)} else {sample(c(0,1),1,prob=c(0.2,0.8))}})
med <- sapply(spam, function(x) {if (x == 0) {sample(c(0,1),1,prob=c(0.95,0.05),replace=T)} else {sample(c(0,1),1,prob=c(0.05,0.95))}})
bns <- sapply(spam, function(x) {if (x == 0) {sample(c(0,1),1,prob=c(0.01,0.99),replace=T)} else {sample(c(0,1),1,prob=c(1,0))}})
lea <- sapply(spam, function(x) {if (x == 0) {sample(c(0,1),1,prob=c(0.1,0.9),replace=T)} else {sample(c(0,1),1,prob=c(0.9,0.1))}})

d <- as.matrix(cbind(spam,buy,med,bns,lea))
colnames(d) <- c("spam","buy","med","bnstruct","learn")

bd <- BNDataset(d+1, c(T,T,T,T,T), c("spam","buy","med","bnstruct","learn"),c(2,2,2,2,2))

n <- learn.network(bd, algo="mmhc", layering=c(1,2,2,2,2), layer.struct=matrix(c(0,0,1,0),c(2,2)))
plot(n)
