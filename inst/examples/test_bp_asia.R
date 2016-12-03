library("bnstruct")

# asia_10000 <- BNDataset(name = "Asia")
# asia_10000 <- read.dataset(asia_10000, "../bnstruct/inst/extdata/asia_10000.header", "../bnstruct/inst/extdata/asia_10000.data", imputation = FALSE, bootstrap=FALSE)
# save(asia_10000, file="data/asia_10000.rda")
# break
mydata <- asia()
net <- learn.network(mydata)
print(net)

inf.eng <- InferenceEngine(net)

plot(net)

print(inf.eng)

observations(inf.eng) <- list(c("Asia", "X-ray", "Dyspnea"), c(1,1,1))
inf.eng <- belief.propagation(inf.eng)

marginals(inf.eng)
get.most.probable.values(inf.eng)
