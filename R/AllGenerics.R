## BN generics
setGeneric("belief.propagation", function(bn, jt, ...) standardGeneric("belief.propagation"))
setGeneric("learn.params", function(bn, dataset, ...) standardGeneric("learn.params"))
setGeneric("learn.structure", function(bn, dataset, ...) standardGeneric("learn.structure"))
setGeneric("print.BN", function(object, ...) standardGeneric("print.BN"))
setGeneric("plot.BN", function(object, ...) standardGeneric("plot.BN"))
setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps"))

## Junction Tree generics
setGeneric("build.junction.tree", function(object, dgraph) standardGeneric("build.junction.tree"))
setGeneric("print.JunctionTree", function(object, ...) standardGeneric("print.JunctionTree"))
setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps")) # to implement (?)

## BNDataset generics
setGeneric("has.data", function(object) standardGeneric("has.data"))
setGeneric("has.raw.data", function(object) standardGeneric("has.raw.data"))
setGeneric("has.imputed.data", function(object) standardGeneric("has.imputed.data"))
setGeneric("get.data", function(object) standardGeneric("get.data"))
setGeneric("get.raw.data", function(object) standardGeneric("get.raw.data"))
setGeneric("get.imputed.data", function(object) standardGeneric("get.imputed.data"))
setGeneric("print.BNDataset", function(object, ...) standardGeneric("print.BNDataset"))
setGeneric("read.dataset", function(object, header, dataset, ...) standardGeneric("read.dataset"))
#setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps")) # to implement (?)
setGeneric("impute", function(object, ...) standardGeneric("impute"))
setGeneric("bootstrap", function(object, ...) standardGeneric("bootstrap"))

## BootstrapIterator generics
