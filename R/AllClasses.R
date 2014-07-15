###############################################################################
# Generic Bayesian Network class
#
# name       : name of the network
# num.nodes  : how many nodes it has
# variable   : name of the variables associated to the nodes
# node.sizes : number of values each variable can take
# cpts       : list of Conditional Probability Tables
# dag        : matrix representing the DAG of the network as adjacency matrix
# wpdag      : weighted partially directed acyclic graph as adjacency matrix
###############################################################################

setClass("BN",
         representation(
           name       = "character",
           num.nodes  = "numeric",
           variables  = "character",
           node.sizes = "vector",
           cpts       = "list",
           dag        = "matrix",
           wpdag      = "matrix"
         ),
         prototype(
           name       = "empty",
           num.nodes  = 0,
           variables  = c(""),
           node.sizes = c(0),
           cpts       = list(NULL),
           dag        = matrix(c(0)),
           wpdag      = matrix(c(0))
         )
        )


###############################################################################
# Dataset class
#
# name          : name of the dataset
# file          : file from which che dataset is taken (if any)
# variables     : variable (column) names
# num.variables : number of variables contained (# columns)
# num.items     : number of items contained (# rows)
# has.rawdata   : TRUE if instance actually contains raw data
# has.impdata   : TRUE if instance actually contains imputed data
# raw.data      : dataset as matrix; no imputation has been performed, so
#                 either the dataset has no missing data, or missing data
#                 have not been imputed. Implies has.rawdata == TRUE
# imputation    : TRUE if dataset requires imputation, FALSE otherwise
# imputed data  : imputed dataset as matrix, if needed. Implies
#                 has.impdata == TRUE
###############################################################################

setClass("BNDataset",
         representation(
           name          = "character",
           header.file   = "character",
           data.file     = "character",
           variables     = "character",
           node.sizes    = "numeric",
           num.variables = "numeric",
           num.items     = "numeric",
           has.rawdata   = "logical",
           has.impdata   = "logical",
           raw.data      = "matrix",
           imputation    = "logical",
           imputed.data  = "matrix",
           has.boots     = "logical",
           boots         = "list",
           has.imp.boots = "logical",
           imp.boots     = "list"
         ),
         prototype(
           name          = "",
           header.file   = "",
           data.file     = "",
           variables     = c(""),
           node.sizes    = c(0),
           num.variables = 0,
           num.items     = 0,
           has.rawdata   = FALSE,
           has.impdata   = FALSE,
           raw.data      = matrix(c(0)),
           imputation    = FALSE,
           imputed.data  = matrix(c(0)),
           has.boots     = FALSE,
           boots         = list(NULL),
           has.imp.boots = FALSE,
           imp.boots     = list(NULL)
         )
        )


###############################################################################
# Junction Tree class
#
# junction.tree      : the junction tree as adjacency matrix of clique nodes
# num.nodes          : number of nodes of the junction tree (# cliques)
# bn                 : the network it refers to
# cliques            : list of cliques, each as list of variables
# triangulated.graph : the triangulated graph as adjacency matrix
###############################################################################

setClass("JunctionTree",
         representation(
           junction.tree      = "matrix",
           num.nodes          = "numeric",
           bn                 = "BN",
           cliques            = "list",
           triangulated.graph = "matrix"
         ),
         prototype(
           junction.tree      = matrix(c(0)),
           num.nodes          = 0,
           bn                 = NULL,
           cliques            = list(NULL),
           triangulated.graph = matrix(c(0))
         )
        )


###############################################################################
# Dataset bootstrap iterator
# 
# dataset : dataset
# current : current element returned
# imputed : use imputed data, if available
###############################################################################

# setClass("BootstrapIterator",
#          representation(
#            dataset : "BNDataset",
#            current : "numeric",
#            imputed : "logical"
#          ),
#          prototype(
#            dataset : NULL,
#            current : -1,
#            imputed : FALSE
#          )
#         )