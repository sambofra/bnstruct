library("bnstruct")

# toy example to check the method works
mydata <- BNDataset("../extdata/asia_2_layers.data", "../extdata/asia_2_layers.header", starts.from=0)
net <- learn.dynamic.network(mydata, num.time.steps=2, layering=c(1,1,1,1,2,2,2,2))
plot(net)
