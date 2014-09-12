setClassUnion("missingOrNumeric", c("missing","numeric"))
setClassUnion("missingOrLogical", c("missing","logical"))
setClassUnion("missingOrInteger", c("missing","integer"))
setClassUnion("missingOrCharacter", c("missing","character"))
setClassUnion("missingOrNULL", c("missing", "NULL"))

###############################################################################
#
# Generic Bayesian Network class
#
###############################################################################

#' BN class.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{name}:}{name of the network}
#'   \item{\code{num.nodes}:}{number of nodes in the network}
#'   \item{\code{variables}:}{names of the variables in the network}
#'   \item{\code{discreteness}:}{\code{TRUE} if variable is discrete, \code{FALSE} if variable is continue}
#'   \item{\code{node.sizes}:}{if variable \code{i} is discrete, \code{node.sizes[i]} contains the cardinality of \code{i},
#'      if \code{i} is instead discrete the value is the number of states variable \code{i} takes when discretized}
#'   \item{\code{cpts}:}{list of conditional probability tables of the network}
#'   \item{\code{dag}:}{adjacency matrix of the network}
#'   \item{\code{wpdag}:}{weighted partially dag}
#' }
#' 
#' @name BN-class
#' @rdname BN-class
#' @docType class
#' @aliases BN,BN-class
#' 
#' @exportClass BN
setClass("BN",
         representation(
           name         = "character",
           num.nodes    = "numeric",
           variables    = "character",
           discreteness = "logical",
           node.sizes   = "numeric",
           cpts         = "list",
           dag          = "matrix",
           wpdag        = "matrix"
         ),
         prototype(
           name         = "",
           num.nodes    = 0,
           variables    = c(""),
           discreteness = c(TRUE),
           node.sizes   = c(0),
           cpts         = list(NULL),
           dag          = matrix(),
           wpdag        = matrix()
         )
        )


# Create new class union to allow the embedding of a BN in a slot of another class,
# allowing it to take the default value of NULL.
setClassUnion("BNOrNULL", members=c("BN", "NULL"))


###############################################################################
#
# Dataset class
#
###############################################################################

#' BNDataset class.
#' 
#' Contains the all of the data that can be extracted from a given dataset:
#' raw data, imputed data, raw and imputed data with bootstrap.
#' 
#' Dataset should be provided in the following format... (describe)
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{name}:}{name of the dataset}
#'   \item{\code{header.file}:}{name and location of the header file}
#'   \item{\code{data.file}:}{name and location of the data file}
#'   \item{\code{variables}:}{names of the variables in the network}
#'   \item{\code{node.sizes}:}{cardinality of each variable of the network}
#'   \item{\code{num.variables}:}{number of variables (columns) in the dataset}
#'   \item{\code{discreteness}:}{\code{TRUE} if variable is discrete, \code{FALSE} if variable is continue}
#'   \item{\code{num.items}:}{number of observations (rows) in the dataset}
#'   \item{\code{has.rawdata}:}{TRUE if the dataset contains data read from a file}
#'   \item{\code{has.impdata}:}{TRUE if the dataset contains imputed data (computed from raw data)}
#'   \item{\code{raw.data}:}{matrix containing raw data}
#'   \item{\code{imputation}:}{TRUE if it dataset contains imputed data}
#'   \item{\code{imputed.data}:}{matrix containing imputed data}
#'   \item{\code{has.boots}:}{dataset has bootstrap samples}
#'   \item{\code{boots}:}{list of bootstrap samples}
#'   \item{\code{has.imp.boots}:}{dataset has imputed bootstrap samples}
#'   \item{\code{imp.boots}:}{list of imputed bootstrap samples}
#'   \item{\code{num.boots}:}{number of bootstrap samples}
#' }
#' 
#' @name BNDataset-class
#' @rdname BNDataset-class
#' @docType class
#' @aliases BNDataset,BNDataset-class
#'
#' @exportClass BNDataset
setClass("BNDataset",
         representation(
           name          = "character",
           header.file   = "character",
           data.file     = "character",
           variables     = "character",
           node.sizes    = "numeric",
           num.variables = "numeric",
           discreteness  = "logical",
           num.items     = "numeric",
           has.rawdata   = "logical",
           has.impdata   = "logical",
           raw.data      = "matrix",
           imputation    = "logical",
           imputed.data  = "matrix",
           has.boots     = "logical",
           boots         = "list",
           has.imp.boots = "logical",
           imp.boots     = "list",
           num.boots     = "numeric"
         ),
         prototype(
           name          = "",
           header.file   = "",
           data.file     = "",
           variables     = c(""),
           node.sizes    = c(0),
           num.variables = 0,
           discreteness  = c(TRUE),
           num.items     = 0,
           has.rawdata   = FALSE,
           has.impdata   = FALSE,
           raw.data      = matrix(c(0)),
           imputation    = FALSE,
           imputed.data  = matrix(c(0)),
           has.boots     = FALSE,
           boots         = list(NULL),
           has.imp.boots = FALSE,
           imp.boots     = list(NULL),
           num.boots     = 0
         )
        )


###############################################################################
#
# InferenceEngine class
#
###############################################################################

setClassUnion("vectorOrNULL", members=c("vector", "NULL"))

#' InferenceEngine class.
#' 
#' @section Slots:
#' \describe{
#'   \item{\code{junction.tree}:}{junction tree adjacency matrix.}
#'   \item{\code{num.nodes}:}{number of nodes in the junction tree.}
#'   \item{\code{cliques}:}{list of cliques composing the nodes of the junction tree.}
#'   \item{\code{triangulated.graph}:}{adjacency matrix of the original triangulated graph.}
#'   \item{\code{jpts}:}{inferred joint probability tables.}
#'   \item{\code{bn}:}{original Bayesian Network (as object of class \code{\link{BN}}) as provided by the user, or learnt from a dataset.
#'          \code{NULL} if missing.}
#'   \item{\code{updated.bn}:}{Bayesian Network  (as object of class \code{\link{BN}}) as modified by a belief propagation computation. In particular,
#'          it will have different conditional probability tables with respect to its original version. \code{NULL} if missing.}
#'   \item{\code{observed.vars}:}{list of observed variables, by name or number.}
#'   \item{\code{observed.vals}:}{list of observed values for the corresponding variables in \code{observed.vars}.}
#' }
#' 
#' 
#' @name InferenceEngine-class
#' @docType class
#' @rdname InferenceEngine-class
#' @aliases InferenceEngine,InferenceEngine-class
#' 
#' @exportClass InferenceEngine
setClass("InferenceEngine",
         representation(
           junction.tree      = "matrix",
           num.nodes          = "numeric",
           cliques            = "list",
           triangulated.graph = "matrix",
           jpts               = "list",
           bn                 = "BNOrNULL",
           updated.bn         = "BNOrNULL",
           observed.vars      = "vectorOrNULL",
           observed.vals      = "vectorOrNULL"
         ),
         prototype(
           junction.tree      = matrix(),
           num.nodes          = 0,
           cliques            = list(NULL),
           triangulated.graph = matrix(),
           jpts               = list(NULL),
           bn                 = NULL,
           updated.bn         = NULL,
           observed.vars      = NULL,
           observed.vals      = NULL
         )
        )
