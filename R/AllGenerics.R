## BN generics
#setGeneric("BN<-", function(name, num.nodes, node.sizes, cpts, variables, dag) standardGeneric("BN<-"))
setGeneric("belief.propagation", function(bn, jt, observed.vars, observed.vals) standardGeneric("belief.propagation"))
setGeneric("learn.params", function(bn, dataset, ess) standardGeneric("learn.params"))

## Junction Tree generics
setGeneric("build.junction.tree", function(object, dgraph) standardGeneric("build.junction.tree"))

## BNDataset generics
setGeneric("has.data", function(object) standardGeneric("has.data"))
setGeneric("has.raw.data", function(object) standardGeneric("has.raw.data"))
setGeneric("has.imputed.data", function(object) standardGeneric("has.imputed.data"))
setGeneric("get.data", function(object) standardGeneric("get.data"))
setGeneric("get.raw.data", function(object) standardGeneric("get.raw.data"))
setGeneric("get.imputed.data", function(object) standardGeneric("get.imputed.data"))
setGeneric("print.BNDataset", function(object, ...) standardGeneric("print.BNDataset"))