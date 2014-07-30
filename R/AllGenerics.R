# generic initialization
# ' @name initialize
# ' @rdname initialize
# ' 
# ' @param ... ignored.
#setGeneric("initialize", function(x, ...) standardGeneric("initialize"))

###############################################################################
###############################################################################

## BN generics

###############################################################################


#' learn the parameters of a \link{BN}.
#' 
#' Learn the parameters of a \link{BN} object according to a \link{BNDataset}
#' using MAP (Maximum A Posteriori) estimation.
#' 
#' @name learn.params
#' @rdname learn.params
#' 
#' @param bn a \code{\link{BN}} object.
#' @param dataset a \code{\link{BNDataset}} object.
#' @param ess Equivalent Sample Size value.
#' @param ... potential further arguments of methods.
#' 
#' @return new \code{\link{BN}} object with conditional probabilities.
#' 
#' @usage learn.params(bn, dataset, ess = 1)
#' 
#' @examples
#' \dontrun{
#' ## first create a BN and learn its structure from a dataset
#' dataset <- BNDataset(name = "MyDataset")
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' bn <- BN()
#' bn <- learn.structure(bn, dataset)
#' bn <- learn.params(bn, dataset, ess=1)
#' }
#' 
#' @exportMethod learn.params
setGeneric("learn.params", function(bn, dataset, ...) standardGeneric("learn.params"))


#' learn the structure of a network.
#' 
#' Learn the structure (the directed acyclic graph) of a \code{\link{BN}} object according to a \code{\link{BNDataset}}.
#' Currently, two algorithms are supported (can be specified using the \code{algo} option): \code{'sm'}, the Silander-Myllymaki exact algorithm,
#' and \code{'mmhc'}, the Max-Min Hill-Climbing heuristic algorithm (default).
#' 
#' The Silander-Myllymaki algorithm can take a very long time, and it is not feasible for networks of more than 20-30 nodes.
#' It is strongly recommended that valid \code{layering}, \code{max.fanin.layers} and \code{max.fanin} parameters are passed
#' to the method if \code{algo = 'sm'} is given as parameter to the method.
#' 
#' @name learn.structure
#' @rdname learn.structure
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
#' @param cont.nodes use an empty vector.
#' @param raw.data \code{TRUE} to learn the structure from the raw dataset. Default is to use imputed dataset
#'     (if available, otherwise the raw dataset will be used anyway).
#' @param ... potential further arguments of methods.
#' 
#' @return new \code{\link{BN}} object with DAG.
#' 
#' @usage learn.structure(bn, dataset, algo="mmhc", alpha=0.05, ess=1, bootstrap=FALSE,
#'           layering=c(), max.fanin.layers=NULL,
#'           max.fanin=num.variables(dataset), cont.nodes=c(), raw.data=FALSE, ...)
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset(name = "MyDataset")
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' bn <- BN()
#' # use MMHC
#' bn <- learn.structure(bn, dataset, alpha=0.05, ess=1, bootstrap=FALSE)
#' 
#' # now use Silander-Myllymaki
#' layers <- layering(bn)
#' mfl <- as.matrix(read.table(header=F,
#' text='0 1 1 1 1 0 1 1 1 1 0 0 8 7 7 0 0 0 14 6 0 0 0 0 19'))
#' bn <- learn.structure(bn, dataset, algo='sm', max.fanin=3, cont.nodes=c(),
#'                       layering=layers, max.fanin.layers=mfl, raw.data=FALSE)
#' }
#' 
#' @exportMethod learn.structure
setGeneric("learn.structure", function(bn, dataset, ...) standardGeneric("learn.structure"))


#' return the layering of the nodes.
#' 
#' Compute the topological ordering of the nodes of a network, in order to divide the network in layers.
#' 
#' @name layering
#' @rdname layering
#' 
#' @param x a \code{\link{BN}} or \code{\link{InferenceEngine}} object.
#' @param updated.bn \code{TRUE} if \code{x} is an InferenceEngine and the updated network is chosen (kept only for compatibility with other methods).
#' @param ... potential further arguments for methods.
#' 
#' @usage
#' layering(x) # x is a BN
#' layering(x, ...)
#' layering(x, updated.bn=TRUE, ...) # x is an InferenceEngine
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset(name="MyDataset")
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' x <- BN(dataset)
#' layering(x)
#' eng <- InferenceEngine(x)
#' layering(x, updated.bn=TRUE)
#' }
#' 
#' @return a vector containing layers the nodes can be divided into.
setGeneric("layering", function(x, ...) standardGeneric("layering"))


#' compute the most probable values to be observed.
#' 
#' Return an array containing thevalues that each variable of the network is morel ikely to take, according to the CPTS.
#' In case of ties take the first value.
#' 
#' @name get.most.probable.values
#' @rdname get.most.probable.values
#' 
#' @param x a \code{\link{BN}} or \code{\link{InferenceEngine}} object.
#' @param ... potential further arguments of methods.
#' 
#' @return array containing, in each position, the most probable value for the corresponding variable.
#' 
#' @usage
#' get.most.probable.values(x)
#' get.most.probable.values(x, ...)
#' 
#' @examples
#' \dontrun{
#' # try with a BN object x
#' get.most.probable.values(x)
#' 
#' # now build an InferenceEngine object
#' eng <- InferenceEngine(x)
#' get.most.probable.values(eng)
#' }
#'  
#' @exportMethod get.most.probable.values
setGeneric("get.most.probable.values", function(x, ...) standardGeneric("get.most.probable.values"))


#' plot a \code{\link{BN}} as a picture.
#' 
#' Plot the network as a picture to default output.
#' 
#' @name plot
#' @rdname plot
#' 
#' @param x a \code{\link{BN}} object.
#' @param use.node.names \code{TRUE} if node names have to be printed. If \code{FALSE}, number are used instead.
#' @param frac fraction
#' @param max.weight max.weight
#' @param node.col list of (\code{R}) colors for the nodes.
#' @param plot.wpdag if \code{TRUE} plot the network according to the WPDAG computed using bootstrap instead of the DAG.
#' @param ... potential further arguments of methods.
#' 
#' @usage plot(x, use.node.names, frac, max.weight,node.col, plot.wpdag, ...)
#' 
#' @examples
#' \dontrun{
#' plot(x, use.node.names=TRUE, frac=0.2, max.weight=1,
#'      node.col=c("cyan"), plot.wpdag=FALSE)
#' }
#' 
#' @exportMethod plot
setGeneric("plot", function(x, ...) standardGeneric("plot"))


#' save a \code{\link{BN}} picture as \code{.eps} file.
#' 
#' Save an image of a Bayesian Network as an \code{.eps} file.
#' 
#' @name save.to.eps
#' @rdname save.to.eps
#' 
#' @param x a \code{\link{BN}} object
#' @param filename name (with path, if needed) of the file to be created
#' 
#' @usage
#' save.to.eps(x, filename)
#' 
#' @examples
#' \dontrun{
#' save.to.eps(x, "out.eps")
#' }
#' 
#' @seealso \code{\link{plot}}
#' 
#' @exportMethod save.to.eps
setGeneric("save.to.eps", function(x, filename) standardGeneric("save.to.eps"))


###############################################################################
###############################################################################

## BNDataset generics

###############################################################################


#' check if a BNDataset contains any data.
#' 
#' Check whether a \code{\link{BNDataset}} object actually contains raw or imputed data.
#' 
#' @name has.data
#' @rdname has.data
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @usage has.data(x)
#' 
#' @examples
#' \dontrun{
#' x <- BNDataset()
#' has.data(x) # FALSE
#' 
#' x <- read.dataset(x, "file.header", "file.data")
#' has.data(x) # TRUE
#' }
#' 
#' @seealso \code{\link{has.raw.data}}, \code{\link{has.imputed.data}}, \code{\link{get.data}}, \code{\link{get.raw.data}}, \code{\link{get.imputed.data}}
#' 
#' @exportMethod has.data
setGeneric("has.data", function(x) standardGeneric("has.data"))

#' check if a BNDataset contains raw data.
#' 
#' Check whether a \code{\link{BNDataset}} object actually contains raw data.
#' 
#' @name has.raw.data
#' @rdname has.raw.data
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @usage has.raw.data(x)
#' 
#' @examples
#' \dontrun{
#' x <- BNDataset()
#' has.raw.data(x) # FALSE
#' 
#' x <- read.dataset(x, "file.header", "file.data")
#' has.raw.data(x) # TRUE, since read.dataset() actually reads raw data.
#' }
#' 
#' @seealso \code{\link{has.data}}, \code{\link{has.imputed.data}}, \code{\link{get.data}}, \code{\link{get.raw.data}}, \code{\link{get.imputed.data}}
#' 
#' @exportMethod has.raw.data
setGeneric("has.raw.data", function(x) standardGeneric("has.raw.data"))

#' check if a BNDataset contains impited data.
#' 
#' Check whether a \code{\link{BNDataset}} object actually contains imputed data.
#' 
#' @name has.imputed.data
#' @rdname has.imputed.data
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @usage has.imputed.data(x)
#' 
#' @examples
#' \dontrun{
#' x <- BNDataset()
#' has.imputed.data(x) # FALSE
#' 
#' x <- read.dataset(x, "file.header", "file.data")
#' has.imputed.data(x) # FALSE, since read.dataset() actually reads raw data.
#' 
#' x <- impute(x)
#' has.imputed.data(x) # TRUE
#' }
#' 
#' @seealso \code{\link{has.data}}, \code{\link{has.raw.data}}, \code{\link{get.data}}, \code{\link{get.raw.data}}, \code{\link{get.imputed.data}}
#' 
#' @exportMethod has.imputed.data
setGeneric("has.imputed.data", function(x) standardGeneric("has.imputed.data"))

#' get data of a BNDataset.
#' 
#' Return data contained in a \code{\link{BNDataset}} object, if any.
#' Preference is given to imputed data, if available, because the imputed dataset
#' is (supposed to be), in general, more useful. To obtain specifically raw or imputed data,
#' one must revert to \code{\link{get.raw.data}()} and \code{\link{get.imputed.data}()}, respectively.
#' 
#' @name get.data
#' @rdname get.data
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @usage get.data(x)
#' 
#' @examples
#' \dontrun{
#' x <- BNDataset()
#' x <- read.dataset(x, "file.header", "file.data")
#' get.data(x) # returns raw dataset, the only one present in dataset
#' 
#' x <- impute(x)
#' get.data(x) # returns imputed dataset, since it is present now
#' }
#' 
#' @seealso \code{\link{has.data}}, \code{\link{has.raw.data}}, \code{\link{has.imputed.data}}, \code{\link{get.raw.data}}, \code{\link{get.imputed.data}}
#' 
#' @exportMethod get.data
setGeneric("get.data", function(x) standardGeneric("get.data"))

#' get raw data of a BNDataset.
#' 
#' Return raw data contained in a \code{\link{BNDataset}} object, if any.
#' 
#' @name get.raw.data
#' @rdname get.raw.data
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @usage get.raw.data(x)
#' 
#' @seealso \code{\link{has.data}}, \code{\link{has.raw.data}}, \code{\link{has.imputed.data}}, \code{\link{get.data}}, \code{\link{get.imputed.data}}
#' 
#' @exportMethod get.raw.data
setGeneric("get.raw.data", function(x) standardGeneric("get.raw.data"))


#' get imputed data of a BNDataset.
#' 
#' Return imputed data contained in a \code{\link{BNDataset}} object, if any.
#' 
#' @name get.imputed.data
#' @rdname get.imputed.data
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @usage get.imputed.data(x)
#' 
#' @seealso \code{\link{has.data}}, \code{\link{has.raw.data}}, \code{\link{has.imputed.data}}, \code{\link{get.data}}, \code{\link{get.raw.data}}
#' 
#' @exportMethod get.imputed.data
setGeneric("get.imputed.data", function(x) standardGeneric("get.imputed.data"))


#' add raw data.
#' 
#' Insert raw data in a \code{\link{BNDataset}} object.
#' 
#' Users are encouraged to not use this method whenever possible, in favour of \code{\link{read.dataset}}.
#' 
#' @name raw.data<-
#' @rdname raw.data-set
#' 
#' @param x a \code{\link{BNDataset}}.
#' @param value a matrix of integers containing a dataset.
#' 
#' @usage
#' raw.data(x) <- value
#' 
#' @seealso \code{\link{has.data}}, \code{\link{has.raw.data}}, \code{\link{get.data}}, \code{\link{read.dataset}}
#' 
#' @exportMethod raw.data<-
setGeneric("raw.data<-", function(x, value) standardGeneric("raw.data<-"))


#' add imputed data.
#' 
#' Insert imputed data in a \code{\link{BNDataset}} object.
#' 
#' Users are encouraged to not use this method whenever possible, in favour of \code{\link{read.dataset}} with flag \code{imputation = TRUE}.
#' 
#' @name imputed.data<-
#' @rdname imputed.data-set
#' 
#' @param x a \code{\link{BNDataset}}.
#' @param value a matrix of integers containing a dataset.
#' 
#' @usage
#' imputed.data(x) <- value
#' 
#' @seealso \code{\link{has.data}}, \code{\link{has.imputed.data}}, \code{\link{get.data}}, \code{\link{read.dataset}}
#' 
#' @exportMethod imputed.data<-
setGeneric("imputed.data<-", function(x, value) standardGeneric("imputed.data<-"))


#' Read a dataset from file.
#' 
#' File has to be in format (describe...)
#' 
#' @name read.dataset
#' @rdname read.dataset
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
#' @usage
#' # object is a BNDataset object
#' read.dataset(object, header = "file.header", dataset = "file.data",
#'          imputation=FALSE, header.flag=FALSE, na.string.symbol='?', sep.symbol='',
#'          k.impute = 10, bootstrap = FALSE, num.boots = 100, seed = 0, ...)
#'          
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' }
#' 
#' @exportMethod read.dataset
setGeneric("read.dataset", function(object, header, dataset, ...) standardGeneric("read.dataset"))


#' Impute a \code{\link{BNDataset}} raw data with missing values.
#'
#' @name impute
#' @rdname impute
#' 
#' @param object the \code{\link{BNDataset}} object.
#' @param k.impute radius for imputation.
#' @param ... potential further arguments of methods.
#' 
#' @usage
#' # object is a BNDataset object
#' impute(object, k.impute = 10, ...)
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' dataset <- impute(dataset)
#' }
#' 
#' @exportMethod impute
setGeneric("impute", function(object, ...) standardGeneric("impute"))


#' Perform bootstrap.
#' 
#' Create a list of \code{num.boots} samples of the original dataset.
#' 
#' @name bootstrap
#' @rdname bootstrap
#' 
#' @param object the \code{\link{BNDataset}} object.
#' @param num.boots number of sampled datasets for bootstrap.
#' @param seed random seed.
#' @param imputation \code{TRUE} if imputation has to be performed.
#' @param na.string.symbol character that denotes NA in the dataset (useful only if imputation == TRUE).
#' @param k.impute radius for imputation (useful only if imputation == TRUE).
#' @param ... potential further arguments of methods.
#' 
#' @usage
#' bootstrap(object, num.boots = 100, seed = 0, imputation = FALSE,
#'           k.impute = 10, na.string.symbol = '?', ...)
#'           
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' dataset <- bootstrap(dataset, num.boots = 1000)
#' }
#' 
#' @exportMethod bootstrap
setGeneric("bootstrap", function(object, ...) standardGeneric("bootstrap"))


#' get selected element of bootstrap list.
#' 
#' Given a \code{\link{BNDataset}}, return the sample corresponding to given index.
#' 
#' @name get.boot
#' @rdname get.boot
#' 
#' @param dataset a \code{\link{BNDataset}} object.
#' @param index the index of the requested sample.
#' @param imputed \code{TRUE} if samples from imputed dataset are to be used.
#' @param ... potential further arguments of methods (ignored).
#' 
#' @usage
#' get.boot(dataset, index = 1, imputed = TRUE, ...)
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' dataset <- bootstrap(dataset, num.boots = 1000)
#' 
#' for (i in 1:num.boots(dataset))
#'    print(get.boot(dataset, i))
#' }
#' 
#' @seealso \code{\link{bootstrap}}
#' 
#' @exportMethod get.boot
setGeneric("get.boot", function(dataset, index, imputed, ...) standardGeneric("get.boot"))


###############################################################################
###############################################################################

## InferenceEngine generics

###############################################################################

#' build a JunctionTree.
#' 
#' Starting from the adjacency matrix of the directed acyclic graph of the network
#' contained in an InferenceEngine, build a JunctionTree for the network
#' and store it into an InferenceEngine.
#' 
#' @name build.junction.tree
#' @rdname build.junction.tree
#' 
#' @param object an \code{\link{InferenceEngine}} object.
#' @param ... potential further arguments for methods.
#' 
#' @usage
#' build.junction.tree(object, ...)
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' net <- BN(dataset)
#' eng <- InferenceEngine()
#' eng <- build.junction.tree(eng)
#' }
#' 
#' @exportMethod build.junction.tree
setGeneric("build.junction.tree", function(object, ...) standardGeneric("build.junction.tree"))


#' perform belief propagation.
#' 
#' Perform belief propagation for the network of an InferenceEngine, given a set of observations when present.
#' In the current version of \code{bnstruct}, belief propagation can be computed only over a junction tree.
#' 
#' @name belief.propagation
#' @rdname belief.propagation
#' 
#' @param ie an \code{\link{InferenceEngine}} object.
#' @param net a \code{\link{BN}} object.
#' @param observed.vars list of observed variables.
#' @param observed.vals values taken by variables listed in \code{observed.vars}.
#' @param return.potentials if TRUE only the potentials are returned, instead of the default \code{\link{BN}}.
#' @param ... potential further arguments of methods.
#' 
#' @return updated \code{\link{InferenceEngine}} object.
#' 
#' @usage
#' belief.propagation(ie)
#' belief.propagation(ie, net=bn, return.potentials=TRUE)
#' belief.propagation(ie, net=bn, observed.vars=c("A","G","X"), observed.vals=c(1,2,1))
#' belief.propagation(ie, observed.vars=c("A", "G", "X"), observed.vals=c(1,2,1))
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' bn <- BN(dataset)
#' ie <- InferenceEngine(bn)
#' ie <- belief.propagation(ie)
#' 
#' observations(ie) <- list("observed.vars"=("A","G","X"), "observed.vals"=c(1,2,1))
#' belief.propagation(ie)
#' }
#' 
#' @exportMethod belief.propagation
setGeneric("belief.propagation", function(ie, ...) standardGeneric("belief.propagation"))


#' check if an updated \code{\link{BN}} is present in an \code{\link{InferenceEngine}}.
#' 
#' Check if an InferenceEngine actually contains an updated network, in order to provide the chance of
#' a fallback and use the original network if no belief propagation has been performed.
#' 
#' @name test.updated.bn
#' @rdname test.updated.bn
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return \code{TRUE} if an updated network is contained in the InferenceEngine, \code{FALSE} otherwise.
#' 
#' @usage
#' test.updated.bn(x)
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' bn <- BN(dataset)
#' ie <- InferenceEngine(bn)
#' test.updated.bn(ie) # FALSE
#' 
#' observations(ie) <- list("observed.vars"=("A","G","X"), "observed.vals"=c(1,2,1))
#' ie <- belief.propagation(ie)
#' test.updated.bn(ie) # TRUE
#' }
#' 
#' @export test.updated.bn
setGeneric("test.updated.bn", function(x) standardGeneric("test.updated.bn"))


###############################################################################
###############################################################################

## Accessors and mutators

###############################################################################

#' get name of an object.
#' 
#' Return the name of an object, of class \code{\link{BN}} or \code{\link{BNDataset}}.
#' 
#' @name name
#' @rdname name
#' 
#' @param x an object.
#' 
#' @return name of the object.
#' 
#' @usage
#' name(x)
#' 
#' @exportMethod name
setGeneric("name", function(x) standardGeneric("name"))


#' get number of nodes of an object.
#' 
#' Return the name of an object, of class \code{\link{BN}} or \code{\link{InferenceEngine}}.
#' 
#' @name num.nodes
#' @rdname num.nodes
#' 
#' @param x an object.
#' 
#' @return number of nodes of the desired object.
#' 
#' @usage num.nodes(x)
#' 
#' @exportMethod num.nodes
setGeneric("num.nodes", function(x) standardGeneric("num.nodes"))


#' get variables of an object.
#' 
#' Get the list of variables (with their names) of a \code{\link{BN}} or \code{\link{BNDataset}}.
#' 
#' @name variables
#' @rdname variables
#' 
#' @param x an object.
#' 
#' @return vector of the variables names of the desired object.
#' 
#' @usage variables(x)
#' 
#' @exportMethod variables
setGeneric("variables", function(x) standardGeneric("variables"))


#' get status (discrete or continuous) of the variables of an object.
#' 
#' Get a vector representing the status of the variables (with their names) of a \code{\link{BN}} or \code{\link{BNDataset}}.
#' Elements of the vector are \code{c} if the variable is continue, and \code{d} if the variable is discrete.
#' 
#' @name discreteness
#' @rdname discreteness
#'
#' @param x an object.
#' 
#' @return vector contaning, for each variable of the desired object,
#'         \code{c} if the variable is continue, and \code{d} if the variable is discrete.
#' 
#' @usage discreteness(x)
#' 
#' @exportMethod discreteness
setGeneric("discreteness", function(x) standardGeneric("discreteness"))


#' get size of the variables of an object.
#' 
#' Return a list containing the size of the variables of an object. It is the actual cardinality
#' of discrete variables, and the cardinality of the discretized variable for continuous variables.
#' 
#' @name node.sizes
#' @rdname node.sizes
#' 
#' @param x an object.
#' 
#' @return vector contaning the size of each variable of the desired object.
#' 
#' @usage node.sizes(x)
#' 
#' @exportMethod node.sizes
setGeneric("node.sizes", function(x) standardGeneric("node.sizes"))


#' get the list of conditional probability tables of a \code{\link{BN}}.
#' 
#' Return the list of conditional probability tables of the variables of a \code{\link{BN}} object.
#' Each probability table is associated to the corresponding variable, and its dimensions are named according
#' to the variable they represent.
#' 
#' Each conditional probability table is represented as a multidimensional array. 
#' The ordering of the dimensions of each variable is not guaranteed to follow the actual conditional distribution.
#' E.g. dimensions for conditional probability \code{P(C|A,B)} can be either \code{(C,A,B)} or \code{(A,B,C)}, depending on
#' if some operations have been performed, or how the probability table has been computed.
#' Users should not rely on dimension numbers, but should instead select the dimensions using their names.
#' 
#' @name cpts
#' @rdname cpts
#' 
#' @param x an object.
#' 
#' @return list of the conditional probability tables of the desired object.
#' 
#' @usage cpts(x)
#' 
#' @exportMethod cpts
setGeneric("cpts", function(x) standardGeneric("cpts"))


#' get adjacency matrix of a network.
#' 
#' Return the adjacency matrix of the directed acyclic graph representing the structure of a network.
#' 
#' @name dag
#' @rdname dag
#'
#' @param x an object.
#' 
#' @return matrix containing the adjacency matrix of the directed acyclic graph representing
#'         the structure of the object.
#'         
#' @usage dag(x)
#' 
#' @exportMethod dag
setGeneric("dag", function(x) standardGeneric("dag"))


#' get the WPDAG of an object.
#' 
#' Return the weighted partially directed acyclic graph of a network, when available (e.g. when bootstrap on dataset is performed).
#' 
#' @name wpdag
#' @rdname wpdag
#'
#' @param x an object.
#' 
#' @return matrix contaning the WPDAG of the object.
#' 
#' @usage wpdag(x)
#' 
#' @exportMethod wpdag
setGeneric("wpdag", function(x) standardGeneric("wpdag"))


#' get header file of a \code{\link{BNDataset}}.
#' 
#' Return the header filename of a dataset (with the path to its position, as given by the user),
#' present if the dataset has been read from a file and not manually inserted.
#' The header file contains three rows:
#' \enumerate{
#' \item list of names of the variables, in the same order as in the data file;
#' \item list of cardinalities of the variables, if discrete, or levels for quantization if continuous;
#' \item list of status of the variables: \code{c} for continuous variables, \code{d} for discrete ones.
#' }
#' 
#' @name header.file
#' @rdname header.file
#' 
#' @param x a \code{\link{BNDataset}}.
#' 
#' @return header filename of the dataset.
#' 
#' @seealso \code{\link{data.file}}
#' 
#' @usage header.file(x)
#' 
#' @exportMethod header.file
setGeneric("header.file", function(x) standardGeneric("header.file"))


#' get data file of a \code{\link{BNDataset}}.
#' 
#' Return the data filename of a dataset (with the path to its position, as given by the user).
#' The data filename may contain a header in the first row, containing the list of names of the variables,
#' in the same order as in the header file.
#' After the header, if present, the file contains a data.frame with the observations, one item per row.
#' 
#' @name data.file
#' @rdname data.file
#'
#' @param x a \code{\link{BNDataset}}.
#' 
#' @return data filename of the dataset.
#' 
#' @seealso \code{\link{data.file}}
#' 
#' @usage data.file(x)
#' 
#' @exportMethod data.file
setGeneric("data.file", function(x) standardGeneric("data.file"))


#' get number of variables of a \code{\link{BNDataset}}.
#' 
#' Return the number of the variables contained in a dataset. This value corresponds to the value
#' of \code{\link{num.nodes}} of a network built upon the same dataset.
#' 
#' @name num.variables
#' @rdname num.variables
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return number of variables of the desired dataset.
#' 
#' @seealso \code{\link{num.nodes}}
#' 
#' @usage num.variables(x)
#' 
#' @exportMethod num.variables
setGeneric("num.variables", function(x) standardGeneric("num.variables"))


#' get number of items of a \code{\link{BNDataset}}.
#' 
#' Return the number of items in a dataset, that is, the number of rows in its data slot.
#' 
#' @name num.items
#' @rdname num.items
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return number of items of the desired dataset.
#' 
#' @usage num.items(x)
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
#' Return \code{TRUE} if the given dataset contains samples for bootstrap, \code{FALSE} otherwise.
#' 
#' @name has.boots
#' @rdname has.boots
#'
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return \code{TRUE} if dataset has bootstrap samples.
#' 
#' @usage has.boots(x)
#' 
#' @seealso \code{\link{has.imp.boots}}, \code{\link{boots}}, \code{\link{imp.boots}}
#' 
#' @exportMethod has.boots
setGeneric("has.boots", function(x) standardGeneric("has.boots"))


#' check whether a \code{\link{BNDataset}} has bootstrap samples from imputed data or not.
#' 
#' Return \code{TRUE} if the given dataset contains samples for bootstrap from inputed dataset, \code{FALSE} otherwise.
#' 
#' @name has.imp.boots
#' @name has.imp.boots
#'
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return \code{TRUE} if dataset has bootstrap samples from imputed data.
#' 
#' @usage has.imp.boots(x)
#' 
#' @seealso \code{\link{has.boots}}, \code{\link{boots}}, \code{\link{imp.boots}}
#' 
#' @exportMethod has.imp.boots
setGeneric("has.imp.boots", function(x) standardGeneric("has.imp.boots"))


#' get list of bootstrap samples of a \code{\link{BNDataset}}.
#' 
#' Return the list of samples computed from raw data of a dataset.
#' 
#' @name boots
#' @rdname boots
#'
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return the list of bootstrap samples.
#' 
#' @usage boots(x)
#' 
#' @seealso \code{\link{has.boots}}, \code{\link{has.imp.boots}}, \code{\link{imp.boots}}
#' 
#' @exportMethod boots
setGeneric("boots", function(x) standardGeneric("boots"))


#' get list of bootstrap samples from imputed data of a \code{\link{BNDataset}}.
#' 
#' Return the list of samples computed from raw data of a dataset.
#' 
#' @name imp.boots
#' @rdname imp.boots
#'
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return the list of bootstrap samples from imputed data.
#' 
#' @seealso \code{\link{has.boots}}, \code{\link{has.imp.boots}}, \code{\link{boots}}
#' 
#' @usage imp.boots(x)
#' 
#' @exportMethod imp.boots
setGeneric("imp.boots", function(x) standardGeneric("imp.boots"))


#' get number of bootstrap samples of a \code{\link{BNDataset}}.
#' 
#' Return the number of bootstrap samples computed from a dataset.
#' 
#' @name num.boots
#' @rdname num.boots
#' 
#' @param x a \code{\link{BNDataset}} object.
#' 
#' @return the number of bootstrap samples.
#' 
#' @usage num.boots(x)
#' 
#' @exportMethod num.boots
setGeneric("num.boots", function(x) standardGeneric("num.boots"))


#' get the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' Return the adjacency matrix representing the junction tree computed for a network.
#' 
#' Rows and columns are named after the (variables in the) cliques that each node of the junction tree represent.
#' 
#' @name junction.tree
#' @rdname junction.tree
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the junction tree contained in the \code{\link{InferenceEngine}}.
#' 
#' @usage junction.tree(x)
#' 
#' @seealso \code{\link{build.junction.tree}}
#' 
#' @exportMethod junction.tree
setGeneric("junction.tree", function(x) standardGeneric("junction.tree"))


#' get the list of cliques of the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' Return the list of cliques containing the variables associated to each node of a junction tree.
#' 
#' @name jt.cliques
#' @rdname jt.cliques
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the list of cliques of the junction tree contained in the \code{\link{InferenceEngine}}.
#' 
#' @usage jt.cliques(x)
#' 
#' @exportMethod jt.cliques
setGeneric("jt.cliques", function(x) standardGeneric("jt.cliques"))


#' get the list of joint probability tables compiled by an \code{\link{InferenceEngine}}.
#' 
#' Return the list of joint probability tables for the cliques of the junction tree 
#' obtained after belief propagation has been performed.
#' 
#' Each joint probability table is represented as a multidimensional array. 
#' To retrieve single dimensions (e.g. to compute marginals), users should not rely on dimension numbers,
#' but should instead select the dimensions using their names.
#' 
#' @name jpts
#' @rdname jpts
#'
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the list of joint probability tables compiled by the \code{\link{InferenceEngine}}.
#' 
#' @usage jpts(x)
#' 
#' @exportMethod jpts
setGeneric("jpts", function(x) standardGeneric("jpts"))


#' get the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' Return a network contained in an InferenceEngine. The boolean \code{updated.bn} parameter can be used to choose
#' the updated network or the original one; default returned network is the updated one,
#' when available.
#' 
#' It is suggested to always use the \code{updated.bn} parameter.
#' 
#' @name bn
#' @rdname bn-method
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param updated.bn \code{TRUE} if the network to be returned is the updated one,
#'        \code{FALSE} to obtain the original one.
#' 
#' @return the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         
#' @usage bn(x, updated.bn = TRUE)
#' 
#' @exportMethod bn
setGeneric("bn", function(x, ...) standardGeneric("bn"))


#' get the list of observations of an \code{\link{InferenceEngine}}.
#' 
#' Return the list of observations added to an InferenceEngine.
#' 
#' Output is a list in the following format:
#' \itemize{
#' \item{\code{observed.vars}}{vector of observed variables;}
#' \item{\code{observed.vals}}{vector of values observed for the variables in \code{observed.vars} in the corresponding position.}
#' }
#' 
#' @name observations
#' @rdname observations
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the list of observations of the \code{\link{InferenceEngine}}.
#' 
#' @usage observations(x)
#' 
#' @exportMethod observations
setGeneric("observations", function(x) standardGeneric("observations"))


###############################################################################


#' set name of an object.
#' 
#' Set the \code{name} slot of an object of type \code{\link{BN}} or \code{\link{BNDataset}}.
#' 
#' @name name<-
#' @rdname name-set
#' 
#' @param x an object.
#' @param value the new name of the object.
#' 
#' @usage name(x) <- value
#' 
#' @exportMethod name<-
setGeneric("name<-", function(x, value) standardGeneric("name<-"))


#' set number of nodes of an object.
#' 
#' Set the number of nodes of an object of type \code{\link{BN}} (number of nodes of the network)
#' or \code{\link{InferenceEngine}} (where parameter contains the number of nodes of the junction tree).
#' 
#' @name num.nodes<-
#' @rdname num.nodes-set
#' 
#' @param x an object.
#' @param value the number of nodes in the object.
#' 
#' @usage num.nodes(x) <- value
#' 
#' @exportMethod num.nodes<-
setGeneric("num.nodes<-", function(x, value) standardGeneric("num.nodes<-"))


#' set variables of an object.
#' 
#' Set the list of variable names in a \code{\link{BN}} or \code{\link{BNDataset}} object.
#' 
#' @name variables<-
#' @rdname variables-set
#' 
#' @param x an object.
#' @param value vector containing the variable names of the object.
#'        Overwrites \code{num.nodes} slot if non-matching.
#' 
#' @usage variables(x) <- value
#' 
#' @exportMethod variables<-
setGeneric("variables<-", function(x, value) standardGeneric("variables<-"))


#' set status (discrete or continuous) of the variables of an object.
#' 
#' Set the list of variable status for the variables in a network or a dataset.
#' 
#' @name discreteness<-
#' @rdname discreteness-set
#' 
#' @param x an object.
#' @param value a vector of elements in \{\code{c},\code{d}\} for continuous and discrete variables (respectively).
#' 
#' @usage discreteness(x) <- value
#' 
#' @exportMethod discreteness<-
setGeneric("discreteness<-", function(x, value) standardGeneric("discreteness<-"))


#' set the size of variables of an object.
#' 
#' Set the size of the variables of a BN or BNDataset object. It represents the actual cardinality
#' of discrete variables, and the cardinality of the discretized variable for continuous variables.
#' 
#' @name node.sizes<-
#' @rdname node.sizes-set
#' 
#' @param x an object.
#' @param value vector contaning the size of each variable of the object.
#' 
#' @usage node.sizes(x) <- value
#' 
#' @exportMethod node.sizes<-
setGeneric("node.sizes<-", function(x, value) standardGeneric("node.sizes<-"))


#' set the list of conditional probability tables of a network.
#' 
#' Set the list of conditional probability tables of a \code{\link{BN}} object.
#' 
#' Each conditional probability table is represented as a multidimensional array. 
#' To retrieve single dimensions (e.g. to compute marginals), users should provide dimensions names.
#' 
#' @name cpts<-
#' @rdname cpts-set
#' 
#' @param x an object.
#' @param value list of the conditional probability tables of the object.
#' 
#' @usage cpts(x) <- value
#' 
#' @exportMethod cpts<-
setGeneric("cpts<-", function(x, value) standardGeneric("cpts<-"))


#' set adjacency matrix of an object.
#' 
#' Set the adjacency matrix of the directed acyclic graph representing the structure of a network.
#' 
#' @name dag<-
#' @rdname dag-set
#' 
#' @param x an object.
#' @param value matrix containing the adjacency matrix of the directed acyclic graph representing
#'        the structure of the object.
#'         
#' @usage dag(x) <- value
#' 
#' @exportMethod dag<-
setGeneric("dag<-", function(x, value) standardGeneric("dag<-"))


#' set WPDAG of the object.
#' 
#' Set the weighted partially directed acyclic graph of a network (e.g. in case bootstrap on dataset is performed).
#' 
#' @name wpdag<-
#' @rdname wpdag-set
#' 
#' @param x an object.
#' @param value matrix contaning the WPDAG of the object.
#' 
#' @usage wpdag(x) <- value
#' 
#' @exportMethod wpdag<-
setGeneric("wpdag<-", function(x, value) standardGeneric("wpdag<-"))


#' set header file of a \code{\link{BNDataset}}.
#' 
#' Set the header filename of a dataset (with the path to its position, as given by the user).
#' The header file has to contain three rows:
#' \enumerate{
#' \item list of names of the variables, in the same order as in the data file;
#' \item list of cardinalities of the variables, if discrete, or levels for quantization if continuous;
#' \item list of status of the variables: \code{c} for continuous variables, \code{d} for discrete ones.
#' }
#' Further rows are ignored.
#' 
#' @name header.file<-
#' @rdname header.file-set
#' 
#' @param x a \code{\link{BNDataset}}.
#' @param value header filename.
#' 
#' @usage header.file(x) <- value
#' 
#' @seealso \code{\link{data.file<-}}
#' 
#' @exportMethod header.file<-
setGeneric("header.file<-", function(x, value) standardGeneric("header.file<-"))


#' set data file of a \code{\link{BNDataset}}.
#' 
#' Set the data filename of a dataset (with the path to its position, as given by the user).
#' The data filename may contain a header in the first row, containing the list of names of the variables,
#' in the same order as in the header file.
#' After the header, if present, the file contains a data.frame with the observations, one item per row.
#' 
#' @name data.file<-
#' @rdname data.file-set
#' 
#' @param x a \code{\link{BNDataset}}.
#' @param value data filename.
#' 
#' @usage data.file(x) <- value
#' 
#' @seealso \code{\link{header.file<-}}
#' 
#' @exportMethod data.file<-
setGeneric("data.file<-", function(x, value) standardGeneric("data.file<-"))


#' set number of variables of a \code{\link{BNDataset}}.
#' 
#' Set the number of variables observed in a dataset.
#' 
#' @name num.variables<-
#' @rdname num.variables-set
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value number of variables of the dataset.
#' 
#' @usage num.variables(x) <- value
#' 
#' @exportMethod num.variables<-
setGeneric("num.variables<-", function(x, value) standardGeneric("num.variables<-"))


#' set number of items of a \code{\link{BNDataset}}.
#' 
#' Set the number of observed items (rows) in a dataset.
#' 
#' @name num.items<-
#' @rdname num.items-set
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value number of items of the desired dataset.
#' 
#' @usage num.items(x) <- value
#' 
#' @exportMethod num.items<-
setGeneric("num.items<-", function(x, value) standardGeneric("num.items<-"))


#' set list of bootstrap samples of a \code{\link{BNDataset}}.
#' 
#' Add to a dataset a list of samples from raw data computed using bootstrap.
#' 
#' @name boots<-
#' @rdname boots-set
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value the list of bootstrap samples.
#' 
#' @usage boots(x) <- value
#' 
#' @exportMethod boots<-
setGeneric("boots<-", function(x, value) standardGeneric("boots<-"))


#' set list of bootstrap samples from imputed data of a \code{\link{BNDataset}}.
#' 
#' Add to a dataset a list of samples from imputed data computed using bootstrap.
#' 
#' @name imp.boots<-
#' @rdname imp.boots-set
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value the list of bootstrap samples from imputed data.
#' 
#' @usage imp.boots(x) <- value
#' 
#' @exportMethod imp.boots<-
setGeneric("imp.boots<-", function(x, value) standardGeneric("imp.boots<-"))


#' set the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' Set the adjacency matrix of the junction tree computed for a network.
#' 
#' @name junction.tree<-
#' @rdname junction.tree-set
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the junction tree to be inserted in the \code{\link{InferenceEngine}}.
#' 
#' @usage junction.tree(x) <- value
#' 
#' @exportMethod junction.tree<-
setGeneric("junction.tree<-", function(x, value) standardGeneric("junction.tree<-"))


#' set the list of cliques of the junction tree of an \code{\link{InferenceEngine}}.
#' 
#' Add to the InferenceEngine a list containing the cliques of variables composing the nodes of the junction tree.
#' 
#' @name jt.cliques<-
#' @rdname jt.cliques-set
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of cliques of the junction tree contained in the \code{\link{InferenceEngine}}.
#'
#' @usage jt.cliques(x) <- value
#'   
#' @exportMethod jt.cliques<-
setGeneric("jt.cliques<-", function(x, value) standardGeneric("jt.cliques<-"))


#' set the list of joint probability tables compiled by an \code{\link{InferenceEngine}}.
#' 
#' Add a list of joint probability tables for the cliques of the junction tree.
#' 
#' Each joint probability table is represented as a multidimensional array. 
#' To retrieve single dimensions (e.g. to compute marginals), users should provide dimension names.
#' 
#' @name jpts<-
#' @rdname jpts-set
#'
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of joint probability tables compiled by the \code{\link{InferenceEngine}}.
#' 
#' @usage jpts(x) <- value
#' 
#' @exportMethod jpts<-
setGeneric("jpts<-", function(x, value) standardGeneric("jpts<-"))


#' set the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' Add a network to an InferenceEngine. The boolean \code{updated.bn} parameter can be used to choose
#' if to insert the updated network or the original one; default inserted network is the updated one.
#' 
#' It is suggested to always use the \code{updated.bn} parameter.
#' 
#' @name bn<-
#' @rdname bn-set
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param updated.bn \code{TRUE} if the network to be returned is the updated one,
#'        \code{FALSE} to obtain the original one.
#' @param value the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         Updated network is the default choice.
#'         
#' @usage bn(x, updated.bn = TRUE) <- value
#' 
#' @exportMethod bn<-
setGeneric("bn<-", function(x, ..., value) standardGeneric("bn<-"))


#' set the list of observations of an \code{\link{InferenceEngine}}.
#' 
#' Add a list of observations to an InferenceEngine, using a list of observations composed by the two following vectors:
#' \itemize{
#' \item{\code{observed.vars}}{vector of observed variables;}
#' \item{\code{observed.vals}}{vector of values observed for the variables in \code{observed.vars} in the corresponding position.}
#' }
#' 
#' Replace previous list of observations, if present. In order to add evidence, and not just replace it,
#' one must use the \code{\link{add.observations<-}} method.
#' 
#' In case of multiple observations of the same variable, the last observation is the one used, as the most recent.
#' 
#' @name observations<-
#' @rdname observations-set
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of observations of the \code{\link{InferenceEngine}}.
#' 
#' @usage observations(x) <- value
#' 
#' @seealso \code{\link{add.observations<-}}
#' 
#' @exportMethod observations<-
setGeneric("observations<-", function(x, value) standardGeneric("observations<-"))


#' add further evidence to an existing list of observations of an \code{\link{InferenceEngine}}.
#' 
#' Add a list of observations to an InferenceEngine that already has observations,
#' using a list composed by the two following vectors:
#' \itemize{
#' \item{\code{observed.vars}}{vector of observed variables;}
#' \item{\code{observed.vals}}{vector of values observed for the variables in \code{observed.vars} in the corresponding position.}
#' }
#' 
#' In case of multiple observations of the same variable, the last observation is the one used, as the most recent.
#' 
#' @name add.observations<-
#' @rdname add.observations-set
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the list of observations of the \code{\link{InferenceEngine}}.
#' 
#' @seealso \code{\link{observations<-}}
#' 
#' @usage add.observations(x) <- value
#' 
#' @exportMethod add.observations<-
setGeneric("add.observations<-", function(x, value) standardGeneric("add.observations<-"))


###############################################################################
###############################################################################

# other common generics

###############################################################################


#' print an object to \code{stdout}.
#' 
#' @name print
#' 
#' @param x an object.
#' @param show.raw.data when \code{x} is a \code{\link{BNDataset}}, print also raw dataset, if available.
#' @param show.imputed.data when \code{x} is a \code{\link{BNDataset}}, print also imputed dataset, if available.
#' @param engine when \code{x} is an \code{\link{InferenceEngine}}, specify the inference engine to be shown.
#'        Currently only \code{engine = 'jt'} is supported.
#' @param ... potential other arguments.
#' 
#' @usage
#' print(x) # BN
#' print(x, show.raw.data = FALSE, show.imputed.data = FALSE) # BNDataset
#' print(x, engine = 'jt') # InferenceEngine
#' 
#' @exportMethod print
setGeneric("print", function(x, ...) standardGeneric("print"))
