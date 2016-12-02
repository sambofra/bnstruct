library("bnstruct")

# toy example to check the method works
mydata <- BNDataset("../extdata/asia_2_layers.data", "../extdata/asia_2_layers.header", starts.from=0, num.time.steps=2)
net <- learn.dynamic.network(mydata, num.time.steps=2, layering=c(1,1,1,1,2,2,2,2))
d1 <- dag(net)

net <- learn.network(mydata, layering=c(1,1,1,1,2,2,2,2))
d2 <- dag(net)

print(d1 - d2)

plot(net)
