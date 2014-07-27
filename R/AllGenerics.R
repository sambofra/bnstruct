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
#' @param bn a \code{\link{BN}} or \code{\link{InferenceEngine}} object.
#' @param ... potential further arguments of methods.
#' 
#' @return array containing, in each position, the most probable value for the corresponding variable.
#' 
#' @exportMethod get.most.probable.values
setGeneric("get.most.probable.values", function(x,...) standardGeneric("get.most.probable.values"))


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


#' Insert raw data in a \code{\link{BNDataset}} object.
#' 
#' @name raw.data
#' @rdname raw.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' @param value a matrix of integers containing a dataset.
#' 
#' @exportMethod raw.data<-
setGeneric("raw.data<-", function(object, value) standardGeneric("raw.data<-"))


#' Insert imputed data in a \code{\link{BNDataset}} object.
#' 
#' @name imputed.data
#' @rdname imputed.data-methods
#' 
#' @param object a \code{\link{BNDataset}}.
#' @param value a matrix of integers containing a dataset.
#' 
#' @exportMethod imputed.data<-
setGeneric("imputed.data<-", function(object, value) standardGeneric("imputed.data<-"))


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

## InferenceEngine generics

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
#' @return updated \code{\link{InferenceEngine}} object.
#' 
#' @exportMethod belief.propagation
setGeneric("belief.propagation", function(ie, ...) standardGeneric("belief.propagation"))


#' Test if an updated \code{\link{BN}} is present in an \code{\link{InferenceEngine}}.
#' 
#' @name test.updated.bn
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return \code{TRUE} if an updated network is contained in the InferenceEngine, \code{FALSE} otherwise.
#' 
#' @export test.updated.bn
setGeneric("test.updated.bn", function(x) standardGeneric("test.updated.bn"))


###############################################################################
###############################################################################

## Accessors and mutators

###############################################################################

#' get name of an  object.
#' 
#' @name name
#' @aliases name
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return name of the desired object.
#' 
#' @exportMethod name
setGeneric("name", function(x) standardGeneric("name"))


#' get number of nodes of an object.
#' 
#' @name num.nodes
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return number of nodes of the desired object.
#' 
#' @exportMethod num.nodes
setGeneric("num.nodes", function(x) standardGeneric("num.nodes"))


#' get variables of an object.
#' 
#' @name variables
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return vector of the variables names of the desired object.
#' 
#' @exportMethod variables
setGeneric("variables", function(x) standardGeneric("variables"))


#' get status (discrete or continuous) of the variables of an object.
#' 
#' @name discreteness
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return vector contaning, for each variable of the desired object,
#'         \code{c} if the variable is continue, and \code{d} if the variable is discrete.
#' 
#' @exportMethod discreteness
setGeneric("discreteness", function(x) standardGeneric("discreteness"))


#' get size of the variables of an object. It is the actual cardinality
#' of discrete variables, and the cardinality of the discretized variable for continuous variables.
#' 
#' @name node.sizes
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return vector contaning the size of each variable of the desired object.
#' 
#' @exportMethod node.sizes
setGeneric("node.sizes", function(x) standardGeneric("node.sizes"))


#' get the list of conditional probability tables of an object.
#' 
#' @name cpts
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return list of the conditional probability tables of the desired object.
#' 
#' @exportMethod cpts
setGeneric("cpts", function(x) standardGeneric("cpts"))


#' get adjacency matrix of an object.
#' 
#' @name dag
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return matrix containing the adjacency matrix of the directed acyclic graph representing
#'         the structure of the object.
#' 
#' @exportMethod dag
setGeneric("dag", function(x) standardGeneric("dag"))


#' get WPDAG of an object, when available (e.g. when bootstrap on dataset is performed).
#' 
#' @name wpdag
#' @rdname accessors-methods
#' 
#' @param x an object.
#' 
#' @return matrix contaning the WPDAG of the desired object.
#' 
#' @exportMethod wpdag
setGeneric("wpdag", function(x) standardGeneric("wpdag"))


#' get header file of a \code{\link{BNDataset}}.
#' 
#' @name header.file
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @return header filename of the dataset.
#' 
#' @exportMethod header.file
setGeneric("header.file", function(x) standardGeneric("header.file"))


#' get data file of a \code{\link{BNDataset}}.
#' 
#' @name data.file
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @return data filename of the dataset.
#' 
#' @exportMethod data.file
setGeneric("data.file", function(x) standardGeneric("data.file"))


#' get number of variables of a \code{\link{BNDataset}}.
#' 
#' @name num.variables
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return number of variables of the desired dataset.
#' 
#' @exportMethod num.variables
setGeneric("num.variables", function(x) standardGeneric("num.variables"))


#' get number of items of a \code{\link{BNDataset}}.
#' 
#' @name num.items
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return number of items of the desired dataset.
#' 
#' @exportMethod num.items
setGeneric("num.items", function(x) standardGeneric("num.items"))

# has.rawdata
# has.impdata
# raw.data
# imputation
# imputed.data
# already have methods (see above)


#' check whether a \code{\link{BNDataset}} has bootstrap samples or not.
#' 
#' @name has.boots
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return \code{TRUE} if dataset has bootstrap samples.
#' 
#' @exportMethod has.boots
setGeneric("has.boots", function(x) standardGeneric("has.boots"))


#' check whether a \code{\link{BNDataset}} has bootstrap samples from imputed data or not.
#' 
#' @name has.imp.boots
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return \code{TRUE} if dataset has bootstrap samples from imputed data.
#' 
#' @exportMethod has.imp.boots
setGeneric("has.imp.boots", function(x) standardGeneric("has.imp.boots"))


#' get list of bootstrap samples of a \code{\link{BNDataset}}.
#' 
#' @name boots
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return the list of bootstrap samples.
#' 
#' @exportMethod boots
setGeneric("boots", function(x) standardGeneric("boots"))


#' get list of bootstrap samples from imputed data of a \code{\link{BNDataset}}.
#' 
#' @name imp.boots
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return the list of bootstrap samples from imputed data.
#' 
#' @exportMethod imp.boots
setGeneric("imp.boots", function(x) standardGeneric("imp.boots"))


#' get number of bootstrap samples of a \code{\link{BNDataset}}.
#' 
#' @name num.boots
#' @rdname accessors-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return the number of bootstrap samples.
#' 
#' @exportMethod num.boots
setGeneric("num.boots", function(x) standardGeneric("num.boots"))


#' get the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' @name junction.tree
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the junction tree contained in the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod junction.tree
setGeneric("junction.tree", function(x) standardGeneric("junction.tree"))


#' get the list of cliques of the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' @name jt.cliques
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the list of cliques of the junction tree contained in the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod jt.cliques
setGeneric("jt.cliques", function(x) standardGeneric("jt.cliques"))


#' get the list of joint probability tables compiled by an \code{\link{InferenceEngine}}.
#' 
#' @name jpts
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the list of joint probability tables compiled by the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod jpts
setGeneric("jpts", function(x) standardGeneric("jpts"))


#' get the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' @name bn
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param updated.bn \code{TRUE} if the network to be returned is the updated one,
#'        \code{FALSE} to obtain the original one.
#' 
#' @return the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         Updated network is the default choice.
#' 
#' @exportMethod bn
setGeneric("bn", function(x, ...) standardGeneric("bn"))


#' get the list of observations of an \code{\link{InferenceEngine}}.
#' 
#' @name observations
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the list of observations of the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod observations
setGeneric("observations", function(x) standardGeneric("observations"))


###############################################################################


#' set name of an object.
#' 
#' @name name
#' @rdname mutators-methods
#' @docType methods
#' 
#' @param x an object.
#' @param value the new name of the object.
#' 
#' @exportMethod name<-
setGeneric("name<-", function(x, value) standardGeneric("name<-"))


#' set number of nodes of an object.
#' 
#' @name num.nodes
#' @rdname mutators-methods
#' 
#' @param x an object.
#' @param value the number of nodes in the object.
#' 
#' @exportMethod num.nodes<-
setGeneric("num.nodes<-", function(x, value) standardGeneric("num.nodes<-"))


#' set variables of an object.
#' 
#' @name variables
#' @rdname mutators-methods
#' 
#' @param x an object.
#' @param value vector containing the variable names of the object.
#'        Overwrites \code{num.nodes} slot if non-matching.
#' 
#' @exportMethod variables<-
setGeneric("variables<-", function(x, value) standardGeneric("variables<-"))


#' set status (discrete or continuous) of the variables of an object.
#' 
#' @name discreteness
#' @rdname mutators-methods
#' 
#' @param x an object.
#' @param value a vector of elements in \{\code{c},\code{d}\} for continuous and discrete variables (respectively).
#' 
#' @exportMethod discreteness<-
setGeneric("discreteness<-", function(x, value) standardGeneric("discreteness<-"))


#' set size of the variables of an object. It represents the actual cardinality
#' of discrete variables, and the cardinality of the discretized variable for continuous variables.
#' 
#' @name node.sizes
#' @rdname mutators-methods
#' 
#' @param x an object.
#' @param value vector contaning the size of each variable of the object.
#' 
#' @exportMethod node.sizes<-
setGeneric("node.sizes<-", function(x, value) standardGeneric("node.sizes<-"))


#' set the list of conditional probability tables of an object.
#' 
#' @name cpts
#' @rdname mutators-methods
#' 
#' @param x an object.
#' @param value list of the conditional probability tables of the object.
#' 
#' @exportMethod cpts<-
setGeneric("cpts<-", function(x, value) standardGeneric("cpts<-"))


#' set adjacency matrix of an object.
#' 
#' @name dag
#' @rdname mutators-methods
#' 
#' @param x an object.
#' @param value matrix containing the adjacency matrix of the directed acyclic graph representing
#'         the structure of the object.
#' 
#' @exportMethod dag<-
setGeneric("dag<-", function(x, value) standardGeneric("dag<-"))


#' set WPDAG of the object.
#' 
#' @name wpdag
#' @rdname mutators-methods
#' 
#' @param x an object.
#' @param value matrix contaning the WPDAG of the object.
#' 
#' @exportMethod wpdag<-
setGeneric("wpdag<-", function(x, value) standardGeneric("wpdag<-"))


#' set header file of a \code{\link{BNDataset}}.
#' 
#' @name header.file
#' @rdname mutators-methods
#' 
#' @param x a \code{\link{BNDataset}}.
#' @param value header filename.
#' 
#' @exportMethod header.file<-
setGeneric("header.file<-", function(x, value) standardGeneric("header.file<-"))


#' set data file of a \code{\link{BNDataset}}.
#' 
#' @name data.file
#' @rdname mutators-methods
#' 
#' @param x a \code{\link{BNDataset}}.
#' @param value data filename.
#' 
#' @exportMethod data.file<-
setGeneric("data.file<-", function(x, value) standardGeneric("data.file<-"))


#' set number of variables of a \code{\link{BNDataset}}.
#' 
#' @name num.variables
#' @rdname mutators-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value number of variables of the dataset.
#' 
#' @exportMethod num.variables<-
setGeneric("num.variables<-", function(x, value) standardGeneric("num.variables<-"))


#' set number of items of a \code{\link{BNDataset}}.
#' 
#' @name num.items
#' @rdname mutators-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value number of items of the desired dataset.
#' 
#' @exportMethod num.items<-
setGeneric("num.items<-", function(x, value) standardGeneric("num.items<-"))


#' set list of bootstrap samples of a \code{\link{BNDataset}}.
#' 
#' @name boots
#' @rdname mutators-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value the list of bootstrap samples.
#' 
#' @exportMethod boots<-
setGeneric("boots<-", function(x, value) standardGeneric("boots<-"))


#' set list of bootstrap samples from imputed data of a \code{\link{BNDataset}}.
#' 
#' @name imp.boots
#' @rdname mutators-methods
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value the list of bootstrap samples from imputed data.
#' 
#' @exportMethod imp.boots<-
setGeneric("imp.boots<-", function(x, value) standardGeneric("imp.boots<-"))


#' set the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' @name junction.tree
#' @rdname mutators-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the junction tree to be inserted in the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod junction.tree<-
setGeneric("junction.tree<-", function(x, value) standardGeneric("junction.tree<-"))


#' set the list of cliques of the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' @name jt.cliques
#' @rdname mutators-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of cliques of the junction tree contained in the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod jt.cliques<-
setGeneric("jt.cliques<-", function(x, value) standardGeneric("jt.cliques<-"))


#' set the list of joint probability tables compiled by an \code{\link{InferenceEngine}}.
#' 
#' @name jpts
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of joint probability tables compiled by the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod jpts<-
setGeneric("jpts<-", function(x, value) standardGeneric("jpts<-"))


#' set the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' @name bn
#' @rdname mutators-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param updated.bn \code{TRUE} if the network to be returned is the updated one,
#'        \code{FALSE} to obtain the original one.
#' @param value the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         Updated network is the default choice.
#' 
#' @exportMethod bn<-
setGeneric("bn<-", function(x, ..., value) standardGeneric("bn<-"))


#' set the list of observations of an \code{\link{InferenceEngine}}.
#' Replace previous list of observations, if present.
#' 
#' @name observations
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of observations of the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod observations<-
setGeneric("observations<-", function(x, value) standardGeneric("observations<-"))


#' add evidence to the list of observations of an \code{\link{InferenceEngine}}.
#' 
#' @name add.observations
#' @rdname accessors-methods
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of observations of the \code{\link{InferenceEngine}}.
#' 
#' @exportMethod add.observations<-
setGeneric("add.observations<-", function(x, value) standardGeneric("add.observations<-"))

