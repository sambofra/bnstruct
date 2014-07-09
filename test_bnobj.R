library("bnstruct")

# net <- BN(name = "Asia", num.nodes = 8, node.sizes = c(2,2,2,2,2,2,2,2),
#            cpts = list(NULL), variables = c("1", "2", "3", "4", "5", "6", "7", "8"),
#            dag = matrix(c(0, 1, 0, 0, 0, 0, 0, 0,
#                           0, 0, 0, 0, 0, 1, 0, 0,
#                           0, 0, 0, 1, 1, 0, 0, 0,
#                           0, 0, 0, 0, 0, 1, 0, 0,
#                           0, 0, 0, 0, 0, 0, 0, 1,
#                           0, 0, 0, 0, 0, 0, 1, 1,
#                           0, 0, 0, 0, 0, 0, 0, 0,
#                           0, 0, 0, 0, 0, 0, 0, 0), nrow = 8, ncol = 8, byrow=TRUE))

net <- BN()
slot(net, "name") <- "Asia"
slot(net, "num.nodes") <- 8
slot(net, "node.sizes") <- c(2,2,2,2,2,2,2,2)
slot(net, "cpts") <- list(NULL)
slot(net, "variables") <- c("1", "2", "3", "4", "5", "6", "7", "8")
slot(net, "dag") <- matrix(c(0, 1, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 1, 0, 0,
                           0, 0, 0, 1, 1, 0, 0, 0,
                           0, 0, 0, 0, 0, 1, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 1,
                           0, 0, 0, 0, 0, 0, 1, 1,
                           0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0), nrow = 8, ncol = 8, byrow=TRUE)

data <- read.dataset(filename = "networks/asia/asia_10000_1.data", imputation = FALSE)

num.nodes  <- data$num.nodes
node.sizes <- data$node.sizes
num.items  <- data$num.items
impa       <- data$dataset

mydata <- BNDataset(name = "boh", num.variables = data$num.nodes, num.items = data$num.items, raw.data = data$dataset, has.rawdata = TRUE, variables = net@variables)

print(mydata)
break

#cpcs.table <- mmpc(impa, node.sizes, chi.th=0.05)
#res.mmhc <- hc( impa, node.sizes, cpcs.table, c() )

jt <- JunctionTree(bn = net)
jt <- build.junction.tree(jt, net@dag)


print("---")
print(jt)


#jt <- junction.tree(res.mmhc)

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
cpts[[1]] <- array(c(0.01, 0.99), dim=c(2), dimnames=list("1" = c(1,2)))
cpts[[2]] <- array(c(0.05, 0.95, 0.01, 0.99), dim=c(2,2), dimnames=list("2" = c(1,2), "1" = c(1,2)))
cpts[[3]] <- array(c(0.5, 0.5), dim=c(2), dimnames=list("3" = c(1,2)))
cpts[[4]] <- array(c(0.1, 0.9, 0.01, 0.99), dim=c(2,2), dimnames=list("4" = c(1,2), "3" = c(1,2)))
cpts[[5]] <- array(c(0.6, 0.4, 0.3, 0.7), dim=c(2,2), dimnames=list("5" = c(1,2), "3" = c(1,2)))
cpts[[6]] <- array(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), dim=c(2,2,2), dimnames=list("6" = c(1,2), "2" = c(1,2), "4" = c(1,2)))
cpts[[7]] <- array(c(0.98, 0.02, 0.05, 0.95), dim=c(2,2), dimnames=list("7" = c(1,2), "6" = c(1,2)))
cpts[[8]] <- array(c(0.9, 0.1, 0.8, 0.2, 0.7, 0.3, 0.1, 0.9), dim=c(2,2,2), dimnames=list("8" = c(1,2), "5" = c(1,2), "6" = c(1,2)))

slot(net, "cpts") <- cpts

print(net)
#break


jpts <- belief.propagation(net, jt, c(), c())

print(jpts)