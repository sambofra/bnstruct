###############################################################################
###############################################################################

## BN generics

###############################################################################

#' Perform belief propagation.
#' 
#' @param bn a \link{BN} object
#' @param jt a \link{JunctionTree} object
#' @return modified \link{BN} object containing computed probabilities
#' @exportMethod belief.propagation
setGeneric("belief.propagation", function(bn, jt, ...) standardGeneric("belief.propagation"))

#' Learn the parameters of a \link{BN} object according to a \link{BNDataset}.
#' 
#' @param bn a \link{BN} object
#' @param dataset a \link{BNDataset} object
#' @return modified \link{BN} object with conditional probabilities
#' @exportMethod learn.params
setGeneric("learn.params", function(bn, dataset, ...) standardGeneric("learn.params"))

#' Learn the structure of a \link{BN} object according to a \link{BNDataset}
#' 
#' @param bn a \link{BN} object
#' @param dataset a \link{BNDataset}
#' @return modified \link{BN} object with DAG
#' @exportMethod learn.structure
setGeneric("learn.structure", function(bn, dataset, ...) standardGeneric("learn.structure"))

#' Print a \link{BN} object to stdout.
#' 
#' By default shows only generic infos like name an variables, not the 
#' adjacency matrix.
#' 
#' @param object a \link{BN} object
#' @exportMethod print.BN
setGeneric("print.BN", function(object, ...) standardGeneric("print.BN"))

#' Plot a \link{BN} as a picture.
#' 
#' @param object a \link{BN} object
#' @exportMethod plot.BN
setGeneric("plot.BN", function(object, ...) standardGeneric("plot.BN"))

#' Save a \link{BN} picture as \code{.eps} file
#' 
#' @param object a \link{BN} object
#' @param filename name (with path, if needed) of the file to be created
#' @exportMethod save.to.eps
setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps"))


###############################################################################
###############################################################################

## BNDataset generics

###############################################################################

setGeneric("has.data", function(object) standardGeneric("has.data"))
setGeneric("has.raw.data", function(object) standardGeneric("has.raw.data"))
setGeneric("has.imputed.data", function(object) standardGeneric("has.imputed.data"))
setGeneric("get.data", function(object) standardGeneric("get.data"))
setGeneric("get.raw.data", function(object) standardGeneric("get.raw.data"))
setGeneric("get.imputed.data", function(object) standardGeneric("get.imputed.data"))

#' Print a \link{BNDadaset} object to stdout.
#' 
#' By default shows only generic infos like name an variables, not the 
#' entire raw and imputed datasets.
#' 
#' @param object a \link{BNdataset} object
#' @param show.raw.data \code{TRUE} if entire raw dataset should be printed too
#' @param show.imputed.data \code{TRUE} if entire imputed dataset should be printed too, if available
#' @exportMethod print.BNDataset
setGeneric("print.BNDataset", function(object, ...) standardGeneric("print.BNDataset"))

#' Read a dataset from file.
#' 
#' File has to be in format (describe...)
#' 
#' @param object the \code{\link{BNDataset}} object
#' @param header the \code{header} file
#' @param dataset the \code{data} file
#' @param na.string.symbol character that denotes \code{NA} in the dataset
#' @param sep.symbol separator among values in the dataset
#' @param imputation \code{TRUE} if imputation has to be performed
#' @param k.impute radius for imputation (useful only if imputation == TRUE)
#' @param bootstrap \code{TRUE} if bootstrap has to be performed; prepares a list of datasets sampled from the original one
#' @param num.boots number of sampled datasets for bootstrap (useful only if bootstrap == TRUE)
#' @param seed random seed (useful only if bootstrap == TRUE)
#' @exportMethod read.dataset
setGeneric("read.dataset", function(object, header, dataset, ...) standardGeneric("read.dataset"))

# setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps")) # to implement (?)

#' Impute a dataset with missing values.
#'
#' @param object the \code{\link{BNDataset}} object
#' @param k.impute radius for imputation
#' @exportMethod impute
setGeneric("impute", function(object, ...) standardGeneric("impute"))

#' Perform bootstrap.
#' 
#' Create a list of \code{num.boots} samples of the original dataset.
#' 
#' @param object the \code{\link{BNDataset}} object
#' @param num.boots number of sampled datasets for bootstrap
#' @param seed random seed
#' @param imputation \code{TRUE} if imputation has to be performed
#' @param na.string.symbol character that denotes NA in the dataset (useful only if imputation == TRUE)
#' @param k.impute radius for imputation (useful only if imputation == TRUE)
#' @exportMethod bootstrap
setGeneric("bootstrap", function(object, ...) standardGeneric("bootstrap"))


###############################################################################
###############################################################################

## Junction Tree generics

###############################################################################

#' build a \link{JunctionTree} object
#' 
#' @param object a \link{JunctionTree} object
#' @param dgraph adjacency matrix of a directed graph
#' @exportMethod build.junction.tree
setGeneric("build.junction.tree", function(object, dgraph) standardGeneric("build.junction.tree"))

#' Print a \link{JunctionTree} object to stdout
#' 
#' @param object a \link{JunctionTree} object
#' @exportMethod print.JunctionTree
setGeneric("print.JunctionTree", function(object, ...) standardGeneric("print.JunctionTree"))

# setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps")) # to implement (?)


## BootstrapIterator generics
