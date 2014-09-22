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
setGeneric("learn.params", function(bn, dataset, ess=1, ...) standardGeneric("learn.params"))


#' learn the structure of a network.
#' 
#' Learn the structure (the directed acyclic graph) of a \code{\link{BN}} object according to a \code{\link{BNDataset}}.
#' Currently, two algorithms are supported (can be specified using the \code{algo} option): \code{'sm'}, the Silander-Myllymaki exact algorithm,
#' and \code{'mmhc'}, the Max-Min Hill-Climbing heuristic algorithm (default).
#' Three scoring functions are also provided: \code{'BDeu'}, the Bayesian-Dirichlet equivalent uniform score, \code{'AIC'},
#' the Akaike Information criterion, and \code{'BIC'}, the Bayesian Information criterion.
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
#' @param scoring.func the scoring function to use. Currently, one among \code{BDeu}, \code{AIC}, \code{BIC}.
#' @param alpha confidence threshold (only for \code{mmhc}).
#' @param ess Equivalent Sample Size value.
#' @param bootstrap \code{TRUE} to use bootstrap samples. 
#' @param num.boots number of bootstrap samples to generate, if needed.
#' @param layering vector containing the layers each node belongs to (only for \code{sm}).
#' @param max.fanin.layers matrix of available parents in each layer (only for \code{sm}).
#' @param max.fanin maximum number of parents for each node (only for \code{sm}).
#' @param cont.nodes vector containing the index of continuous variables.
#' @param raw.data \code{TRUE} to learn the structure from the raw dataset. Default is to use imputed dataset
#'     (if available, otherwise the raw dataset will be used anyway).
#' @param imputation \code{TRUE} if imputation is needed; if \code{bootstrap=TRUE}, imputed samples will be also used.
#' @param na.string.symbol symbol for \code{NA} values (missing data).
#' @param k.impute number of neighbours to be used; for discrete variables we use mode, for continuous variables the median value is instead taken.
#' @param seed random seed.
#' @param ... potential further arguments for method.
#' 
#' @return new \code{\link{BN}} object with DAG.
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
setGeneric("learn.structure", function(bn, dataset, algo="mmhc", scoring.func="BDeu", alpha=0.05, ess=1, bootstrap=FALSE,
                                       layering=c(), max.fanin.layers=NULL, max.fanin=num.variables(dataset),
                                       cont.nodes=c(), raw.data=FALSE, num.boots=100, imputation = TRUE, k.impute = 10,
                                       na.string.symbol='?', seed = 0, ...) standardGeneric("learn.structure"))


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
setGeneric("layering", function(x, updated.bn=TRUE, ...) standardGeneric("layering"))


#' compute the most probable values to be observed.
#' 
#' Return an array containing the values that each variable of the network is more likely to take, according to the CPTS.
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


#' sample a row vector of values for a network.
#' 
#' @name sample.row
#' @rdname sample.row
#' 
#' @param x a \code{\link{BN}} or \code{\link{InferenceEngine}} object.
#' 
#' @return a vector of values.
#' 
#' @exportMethod sample.row
setGeneric("sample.row", function(x) standardGeneric("sample.row"))


#' sample a \code{\link{BNDataset}} from a network of an inference engine.
#' 
#' @name sample.dataset
#' @rdname sample.dataset
#' 
#' @param x a \code{\link{BN}} or \code{\link{InferenceEngine}} object.
#' @param n number of items to sample.
#' 
#' @return a \code{\link{BNDataset}}
#' 
#' @exportMethod sample.dataset
setGeneric("sample.dataset", function(x, n=100) standardGeneric("sample.dataset"))


#' compute the list of inferred marginals of a BN.
#' 
#' Given an \code{\link{InferenceEngine}}, it returns a list containing the marginals for the variables
#' in the network, according to the propagated beliefs.
#' 
#' @name marginals
#' @rdname marginals
#' 
#' @param x an \code{\link{InferenceEngine}}
#' @param ... potential further arguments of methods.
#' 
#' @return a list containing the marginals of each variable, as probability tables.
#' 
#' @examples
#' \dontrun{
#' eng <- InferenceEngine(net)
#' marginals(eng)
#' }
#' 
#' @exportMethod marginals
setGeneric("marginals", function(x, ...) standardGeneric("marginals"))


#' query BN given observations
#' 
#' @name query
#' @rdname query
#' 
#' @param x a BN.
#' @param observed.vars vector of observed variables.
#' @param observed.vals vector of observed values for corresponding variables in \code{observed.vars}.
#' @param ... potential further arguments for method.
#' 
#' @return most probable values given observations
#' 
#' @exportMethod query
setGeneric("query", function(x, observed.vars=c(), observed.vals=c(), ...) standardGeneric("query"))


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
#' @examples
#' \dontrun{
#' save.to.eps(x, "out.eps")
#' }
#' 
#' @seealso \code{\link{plot}}
#' 
#' @exportMethod save.to.eps
setGeneric("save.to.eps", function(x, filename) standardGeneric("save.to.eps"))


#' Read a network from a \code{.dsc} file.
#' 
#' Read a network described in a \code{.dsc}-formatted file, and
#' build a \code{\link{BN}} object.
#' 
#' The method relies on a coherent ordering of variable values and parameters.
#' 
#' @name read.dsc
#' @rdname read.dsc
#' 
#' @param x the \code{.dsc} file, with absolute/relative position.
#' 
#' @return a \code{\link{BN}} object.
#' 
#' @exportMethod read.dsc
setGeneric("read.dsc", function(x) standardGeneric("read.dsc"))


#' Read a network from a \code{.bif} file.
#' 
#' Read a network described in a \code{.bif}-formatted file, and
#' build a \code{\link{BN}} object.
#' 
#' The method relies on a coherent ordering of variable values and parameters.
#' 
#' @name read.bif
#' @rdname read.bif
#' 
#' @param x the \code{.bif} file, with absolute/relative position.
#' 
#' @return a \code{\link{BN}} object.
#' 
#' @exportMethod read.bif
setGeneric("read.bif", function(x) standardGeneric("read.bif"))


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
#' @param header.file the \code{header} file.
#' @param data.file the \code{data} file.
#' @param na.string.symbol character that denotes \code{NA} in the dataset.
#' @param sep.symbol separator among values in the dataset.
#' @param header.flag \code{TRUE} if the first row of \code{dataset} file is an header (e.g. it contains the variable names).
#' @param imputation \code{TRUE} if imputation has to be performed.
#' @param k.impute number of neighbours to be used; for discrete variables we use mode, for continuous variables the median value is instead taken (useful only if imputation == TRUE).
#' @param bootstrap \code{TRUE} if bootstrap has to be performed; prepares a list of datasets sampled from the original one.
#' @param num.boots number of sampled datasets for bootstrap (useful only if bootstrap == TRUE).
#' @param seed random seed (useful only if bootstrap == TRUE).
#' @param starts.from starting value for entries in the dataset (observed values, default is 0).
#' @param ... potential further arguments of methods.
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, header="file.header", dataset="file.data")
#' }
#' 
#' @exportMethod read.dataset
setGeneric("read.dataset", function(object, header.file, data.file, imputation = FALSE, header.flag = FALSE,
                                    na.string.symbol = '?', sep.symbol = '', k.impute = 10,
                                    bootstrap = FALSE, num.boots = 100, seed = 0, starts.from = 0, ...)
                            standardGeneric("read.dataset"))


#' Impute a \code{\link{BNDataset}} raw data with missing values.
#'
#' @name impute
#' @rdname impute
#' 
#' @param object the \code{\link{BNDataset}} object.
#' @param k.impute number of neighbours to be used; for discrete variables we use mode, for continuous variables the median value is instead taken.
#' @param ... potential further arguments of methods.
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' dataset <- impute(dataset)
#' }
#' 
#' @exportMethod impute
setGeneric("impute", function(object, k.impute=10) standardGeneric("impute"))


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
#' @param k.impute number of neighbours to be used; for discrete variables we use mode, for continuous variables the median value is instead taken (useful only if imputation == TRUE).
#' @param ... potential further arguments of methods.
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' dataset <- bootstrap(dataset, num.boots = 1000)
#' }
#' 
#' @exportMethod bootstrap
setGeneric("bootstrap", function(object, num.boots = 100, seed = 0, imputation = FALSE, k.impute = 10,
                                 na.string.symbol = '?', ...) standardGeneric("bootstrap"))


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
setGeneric("get.boot", function(dataset, index, imputed = TRUE, ...) standardGeneric("get.boot"))


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
setGeneric("belief.propagation", function(ie, net = NULL, observed.vars = NULL, observed.vals = NULL,
                                          return.potentials = FALSE, ...) standardGeneric("belief.propagation"))


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
#' @exportMethod test.updated.bn
setGeneric("test.updated.bn", function(x) standardGeneric("test.updated.bn"))


#' expectation-maximization algorithm.
#' 
#' Learn parameters of a network using the Expectation-Maximization algorithm.
#' 
#' @name em
#' @rdname em
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param dataset observed dataset with missing values for the Bayesian Network of \code{x}.
#' @param threshold threshold for convergence, used as stopping criterion.
#' @param k.impute number of neighbours to be used; for discrete variables we use mode, for continuous variables the median value is instead taken.
#' @param ... further potential arguments for method.
#' 
#' @return a list containing: an \code{\link{InferenceEngine}} with a new updated network (\code{"InferenceEngine"}),
#'         and the imputed dataset (\code{"BNDataset"}).
#' 
#' @examples
#' \dontrun{
#' em(x, dataset)
#' }
#' 
#' @exportMethod em
setGeneric("em", function(x, dataset, threshold = 0.001, k.impute = 10, ...) standardGeneric("em"))


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
#' @exportMethod jpts
setGeneric("jpts", function(x) standardGeneric("jpts"))


#' get the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' Return a network contained in an InferenceEngine.
#' 
#' @name bn
#' @rdname bn-method
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         
#' @exportMethod bn
setGeneric("bn", function(x) standardGeneric("bn"))


#' get the updated \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' Return an updated network contained in an InferenceEngine.
#' 
#' @name updated.bn
#' @rdname updated.bn-method
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' 
#' @return the updated \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         
#' @exportMethod updated.bn
setGeneric("updated.bn", function(x) standardGeneric("updated.bn"))

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
#' @exportMethod boots<-
setGeneric("boots<-", function(x, value) standardGeneric("boots<-"))


#' set number of bootstrap samples of a \code{\link{BNDataset}}.
#' 
#' Set the length of the list of samples of a dataset computed using bootstrap.
#' 
#' @name num.boots<-
#' @rdname num.boots-set
#' 
#' @param x a \code{\link{BNDataset}} object.
#' @param value the number of bootstrap samples.
#' 
#' @exportMethod num.boots<-
setGeneric("num.boots<-", function(x, value) standardGeneric("num.boots<-"))


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
#' @exportMethod jpts<-
setGeneric("jpts<-", function(x, value) standardGeneric("jpts<-"))


#' set the original \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' Add an original network to an InferenceEngine.
#' 
#' @name bn<-
#' @rdname bn-set
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         
#' @exportMethod bn<-
setGeneric("bn<-", function(x, value) standardGeneric("bn<-"))


#' set the updated \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#' 
#' Add an updated network to an InferenceEngine.
#' 
#' @name updated.bn<-
#' @rdname updated.bn-set
#' 
#' @param x an \code{\link{InferenceEngine}}.
#' @param value the updated \code{\link{BN}} object contained in an \code{\link{InferenceEngine}}.
#'         
#' @exportMethod updated.bn<-
setGeneric("updated.bn<-", function(x, value) standardGeneric("updated.bn<-"))



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
#' @exportMethod print
setGeneric("print", function(x, ...) standardGeneric("print"))
