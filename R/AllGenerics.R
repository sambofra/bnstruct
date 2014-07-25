###############################################################################
###############################################################################

## BN generics

###############################################################################

#' Learn the parameters of a \link{BN} object according to a \link{BNDataset}
#' using MAP (Maximum A Posteriori) estimation.
#' 
#' @name learn.params
#' @rdname learn.params-methods
#' 
#' @param bn a \code{\link{BN}} object.
#' @param dataset a \code{\link{BNDataset}} object.
#' @param ess Equivalent Sample Size value.
#' @param ... potential further arguments of methods.
#' @return modified \code{\link{BN}} object with conditional probabilities.
#' 
#' @exportMethod learn.params
setGeneric("learn.params", function(bn, dataset, ...) standardGeneric("learn.params"))

#' Learn the structure of a \code{\link{BN}} object according to a \code{\link{BNDataset}}.
#' 
#' @name learn.structure
#' @rdname learn.structure-methods
#' 
#' @param bn a \code{\link{BN}} object.
#' @param dataset a \code{\link{BNDataset}}.
#' @param algo the algorithm to use. Currently, one among \code{sm} (Silander-Myllymaki) and \code{mmhc} (Max-Min Hill Climbing, default).
#' @param alpha confidence threshold (only for \code{mmhc}).
#' @param ess Equivalent Sample Size value.
#' @param bootstrap \code{TRUE} to use bootstrap samples. 
#' @param layering vector containing the layers each node belongs to (only for \code{sm}).
#' @param max.fanin.layers matrix of available parents in each layer (only for \code{sm}).
#' @param max.fanin maximum number of parents for each node (only for \code{sm}).
#' @param cont.nodes a \code{c(s)}.
#' @param raw.data \code{TRUE} to learn the structure from the raw dataset. Default is to use imputed dataset
#'     (if available, otherwise the raw dataset will be used anyway).
#' @param ... potential further arguments of methods.
#' @return modified \code{\link{BN}} object with DAG.
#' 
#' @exportMethod learn.structure
setGeneric("learn.structure", function(bn, dataset, ...) standardGeneric("learn.structure"))

#' Return an array containing the most probable values for each variable, according to the CPTS.
#' In case of ties take the first value.
#' 
#' @name get.most.probable.values
#' @rdname get.most.probable.values-methods
#' 
#' @param bn a \code{\link{BN}} object.
#' @param ... potential further arguments of methods.
#' 
#' @return array containing, in each position, the most probable value for the corresponding variable.
#' 
#' @exportMethod get.most.probable.values
setGeneric("get.most.probable.values", function(bn,...) standardGeneric("get.most.probable.values"))

#' Print a \code{\link{BN}} object to stdout.
#' 
#' By default shows only generic infos like name an variables, not the 
#' adjacency matrix.
#' 
#' @name print.BN
#' @rdname print-methods
#' 
#' @param x a \code{\link{BN}} object.
#' @param ... potential further arguments of methods.
#' 
#' @exportMethod print.BN
setGeneric("print.BN", function(x, ...) standardGeneric("print.BN"))

#' Plot a \code{\link{BN}} as a picture.
#' 
#' @name plot.BN
#' @rdname plot-methods
#' 
#' @param x a \code{\link{BN}} object.
#' @param use.node.names \code{TRUE} if node names have to be printed. If \code{FALSE}, number are used instead.
#' @param frac fraction
#' @param max.weight max.weight
#' @param node.col list of (\code{R}) colors for the nodes.
#' @param plot.wpdag if \code{TRUE} plot the network according to the WPDAG computed using bootstrap instead of the DAG.
#' @param ... potential further arguments of methods.
#' 
#' @exportMethod plot.BN
setGeneric("plot.BN", function(x, ...) standardGeneric("plot.BN"))

#' Save a \code{\link{BN}} picture as \code{.eps} file
#' 
#' @name save.to.eps
#' @rdname save.to.eps-methods
#' 
#' @param object a \code{\link{BN}} object
#' @param filename name (with path, if needed) of the file to be created
#' 
#' @exportMethod save.to.eps
setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps"))


###############################################################################
###############################################################################

## BNDataset generics

###############################################################################

#' Check whether a \code{\link{BNDataset}} object actually contains raw or imputed data.
#' 
#' @name has.data
#' @rdname has.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' 
#' @exportMethod has.data
setGeneric("has.data", function(object) standardGeneric("has.data"))

#' Check whether a \code{\link{BNDataset}} object actually contains raw data.
#' 
#' @name has.raw.data
#' @rdname has.raw.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' 
#' @exportMethod has.raw.data
setGeneric("has.raw.data", function(object) standardGeneric("has.raw.data"))

#' Check whether a \code{\link{BNDataset}} object actually contains imputed data.
#' 
#' @name has.imputed.data
#' @rdname has.imputed.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' 
#' @exportMethod has.imputed.data
setGeneric("has.imputed.data", function(object) standardGeneric("has.imputed.data"))

#' Return data contained in a \code{\link{BNDataset}} object, if any.
#' 
#' Preference is given to imputed data, if available, because the imputed dataset
#' is (supposed to be), in general, more useful.
#' 
#' @name get.data
#' @rdname get.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' 
#' @exportMethod get.data
setGeneric("get.data", function(object) standardGeneric("get.data"))

#' Return raw data contained in a \code{\link{BNDataset}} object, if any.
#' 
#' @name get.raw.data
#' @rdname get.raw.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' 
#' @exportMethod get.raw.data
setGeneric("get.raw.data", function(object) standardGeneric("get.raw.data"))

#' Return imputed data contained in a \code{\link{BNDataset}} object, if any.
#' 
#' @name get.imputed.data
#' @rdname get.imputed.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' 
#' @exportMethod get.imputed.data
setGeneric("get.imputed.data", function(object) standardGeneric("get.imputed.data"))

#' Print a \code{\link{BNDataset}} object to stdout.
#' 
#' By default shows only generic infos like name an variables, not the 
#' entire raw and imputed datasets.
#' 
#' @method print.BNDataset
#' @name print.BNDataset
#' @rdname print.BNDataset-methods
#' 
# @usage print(x, ...)
#' @usage print(x, show.raw.data = FALSE, show.imputed.data = FALSE, ...)
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param show.raw.data \code{TRUE} if entire raw dataset should be printed too.
#' @param show.imputed.data \code{TRUE} if entire imputed dataset should be printed too, if available.
#' @param ... potential further arguments of methods.
#' 
#' @aliases printBNDataset,BNDataset-methods
#' 
#' @exportMethod print.BNDataset
setGeneric("print.BNDataset", function(x, ...) standardGeneric("print.BNDataset"))

#' Read a dataset from file.
#' 
#' File has to be in format (describe...)
#' 
#' @name read.dataset
#' @rdname read.dataset-methods
#' 
#' @param object the \code{\link{BNDataset}} object.
#' @param header the \code{header} file.
#' @param dataset the \code{data} file.
#' @param na.string.symbol character that denotes \code{NA} in the dataset.
#' @param sep.symbol separator among values in the dataset.
#' @param header.flag \code{TRUE} if the first row of \code{dataset} file is an header (e.g. it contains the variable names).
#' @param imputation \code{TRUE} if imputation has to be performed.
#' @param k.impute radius for imputation (useful only if imputation == TRUE).
#' @param bootstrap \code{TRUE} if bootstrap has to be performed; prepares a list of datasets sampled from the original one.
#' @param num.boots number of sampled datasets for bootstrap (useful only if bootstrap == TRUE).
#' @param seed random seed (useful only if bootstrap == TRUE).
#' @param ... potential further arguments of methods.
#' 
#' @exportMethod read.dataset
setGeneric("read.dataset", function(object, header, dataset, ...) standardGeneric("read.dataset"))

# setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps")) # to implement (?)

#' Impute a \code{\link{BNDataset}} raw data with missing values.
#'
#' @name impute
#' @rdname impute-methods
#' 
#' @param object the \code{\link{BNDataset}} object.
#' @param k.impute radius for imputation.
#' @param ... potential further arguments of methods.
#' 
#' @exportMethod impute
setGeneric("impute", function(object, ...) standardGeneric("impute"))

#' Perform bootstrap.
#' 
#' Create a list of \code{num.boots} samples of the original dataset.
#' 
#' @name bootstrap
#' @rdname bootstrap-methods
#' 
#' @param object the \code{\link{BNDataset}} object.
#' @param num.boots number of sampled datasets for bootstrap.
#' @param seed random seed.
#' @param imputation \code{TRUE} if imputation has to be performed.
#' @param na.string.symbol character that denotes NA in the dataset (useful only if imputation == TRUE).
#' @param k.impute radius for imputation (useful only if imputation == TRUE).
#' @param ... potential further arguments of methods.
#' 
#' @exportMethod bootstrap
setGeneric("bootstrap", function(object, ...) standardGeneric("bootstrap"))

#' Get selected element of bootstrap list.
#' 
#' @name get.boot
#' @rdname get.boot-methods
#' 
#' @param dataset a \code{\link{BNDataset}} object.
#' @param index the index of the requested sample.
#' @param imputed \code{TRUE} if samples from imputed dataset are to be used.
#' @param ... potential further arguments of methods (ignored).
#' 
#' @exportMethod get.boot
setGeneric("get.boot", function(dataset, index, ...) standardGeneric("get.boot"))


###############################################################################
###############################################################################

## Junction Tree generics

###############################################################################

#' build a JunctionTree object
#' 
#' @name build.junction.tree
#' @rdname build.junction.tree-methods
#' 
#' @param object an InferenceEngine object
#' @param dgraph adjacency matrix of a directed graph
#' 
#' @exportMethod build.junction.tree
setGeneric("build.junction.tree", function(object, dgraph) standardGeneric("build.junction.tree"))

#' Print a \link{InferenceEngine} object to stdout
#' 
#' @name print.InferenceEngine
#' @rdname print.InferenceEngine-methods
#' 
#' @param x a \link{InferenceEngine} object
#' @param container the data structure to be printed. Currently, only the \code{jt} option
#'     is supported, to show the status of the Junction Tree computed.
#' @param ... potential further arguments of methods.
#' 
#' @exportMethod print.InferenceEngine
setGeneric("print.InferenceEngine", function(x, ...) standardGeneric("print.InferenceEngine"))


#' Perform belief propagation using a Junction Tree.
#' 
#' @name belief.propagation
#' @rdname belief.propagation-methods
#' 
#' @param ie an \code{\link{InferenceEngine}} object.
#' @param bn a \code{\link{BN}} object.
#' @param observed.vars list of observed variables.
#' @param observed.vals values taken by variables listed in \code{observed.vars}.
#' @param return.potentials if TRUE only the potentials are returned, instead of the default \code{\link{BN}}.
#' @param ... potential further arguments of methods.
#' 
#' @return a new \code{\link{BN}} object with updated probabilities.
#' 
#' @exportMethod belief.propagation
setGeneric("belief.propagation", function(ie, bn, ...) standardGeneric("belief.propagation"))

# setGeneric("save.to.eps", function(object, filename) standardGeneric("save.to.eps")) # to implement (?)


## BootstrapIterator generics
