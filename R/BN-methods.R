#' @name BN
#' @rdname BN-class
#' @aliases initialize,BN-method
#' 
#' @param .Object a BN
#' 
setMethod("initialize",
          "BN",
          function(.Object, dataset = NULL, ...)#,
#                    algo = "mmhc", scoring.func = "BDeu", alpha = 0.05, ess = 1, bootstrap = FALSE,
#                    layering = c(), max.fanin.layers = NULL,
#                    max.fanin = num.variables(dataset), cont.nodes = c(), raw.data = FALSE, ...)
          {
            x <- .Object
            
            if (!is.null(dataset))
            {
              name(x)           <- name(dataset)
              num.nodes(x)      <- num.variables(dataset)
              variables(x)      <- variables(dataset)
              node.sizes(x)     <- node.sizes(dataset)
              discreteness(x)   <- discreteness(dataset)
              dag(x)            <- matrix(rep(0, num.nodes(x)*num.nodes(x)), nrow=num.nodes(x), ncol=num.nodes(x))
              wpdag(x)          <- matrix(rep(0, num.nodes(x)*num.nodes(x)), nrow=num.nodes(x), ncol=num.nodes(x))
              num.time.steps(x) <- num.time.steps(dataset)
#               validObject(x)
# 
#               x <- learn.structure(x, dataset, algo = algo, scoring.func = scoring.func, alpha = alpha, ess = ess, bootstrap = bootstrap,
#                                    layering = layering, max.fanin.layers = max.fanin.layers,
#                                    max.fanin = max.fanin, cont.nodes = cont.nodes, raw.data = raw.data)
#               
#               validObject(x)
# 
#               x <- learn.params(x, dataset, ess = ess)
            }
            validObject(x)
            return(x)
          })


#' constructor for a Bayesian Network.
#' 
#' Instantiate a \code{\link{BN}} object.
#' 
#' The constructor may be invoked without parameters -- in this case an empty network will be created, and its slots will be filled manually by the user.
#' This is usually viable only if the user already has knowledge about the network structure.
#' 
# Often, a better choice is to build a network starting from a dataset. Currently, two algorithms are supported for the structure learning step
# (can be specified using the \code{algo} option): \code{'sm'}, the Silander-Myllymaki exact algorithm,
# and \code{'mmhc'}, the Max-Min Hill-Climbing heuristic algorithm (default).
#  The Silander-Myllymaki algorithm can take a very long time, and it is not feasible for networks of more than 20-30 nodes.
# It is strongly recommended that valid \code{layering}, \code{max.fanin.layers} and \code{max.fanin} parameters are passed
# to the method if \code{algo = 'sm'} is given as parameter to the method.
#' 
# The parameter learning step is done using a Maximum-A-Posteriori (MAP) estimation.
#' 
#' @name BN
#' @rdname BN-class
#'
#' @param dataset a \code{\link{BNDataset}} object containing the dataset the network is built upon, if any. The remaining parameters
#'        are considered only if a starting dataset is provided.
# @param algo the algorithm used to learn the structure of the network, if needed. Currently, the supported options are
#        \code{'sm'}, Silander-Myllymaki, exact algorithm, and \code{'mmhc'}, Max-Min Hill-Climbing, heuristic (the default option).
# @param scoring.func scoring function: ome among BDeu, AIC, and BIC.
# @param alpha the confidence threshold for the MMHC algorithm.
# @param ess Equivalent Sample Size value.
# @param bootstrap \code{TRUE} to use bootstrap samples. 
# @param layering vector containing the layers each node belongs to (only for \code{sm}).
# @param max.fanin.layers matrix of available parents in each layer (only for \code{sm}).
# @param max.fanin maximum number of parents for each node (only for \code{sm}).
# @param cont.nodes use an empty vector.
# @param raw.data \code{TRUE} to learn the structure from the raw dataset. Default is to use imputed dataset
#     (if available, otherwise the raw dataset will be used anyway).
#' @param ... potential further arguments of methods.
#' 
#' @return BN object.
#' 
#' @examples
#' \dontrun{
#' net.1 <- BN()
#' 
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' net.2 <- BN(dataset)
#' }
#' 
#' 
#' @export
BN <- function(dataset = NULL, ...)#, algo = "mmhc", scoring.func = 0, alpha = 0.05, ess = 1, bootstrap = FALSE,
#                layering = c(), max.fanin.layers = NULL,
#                max.fanin = num.variables(dataset), cont.nodes = c(), raw.data = FALSE, ...)
{
  object <- new("BN", dataset = dataset, ...)#, scoring.func = scoring.func, algo = algo, alpha = alpha, ess = ess, bootstrap = bootstrap,
#                 layering = layering, max.fanin.layers = max.fanin.layers,
#                 max.fanin = max.fanin, cont.nodes = cont.nodes, raw.data = raw.data, ...)
  return(object)
}

# validator
setValidity("BN",
            function(object)
            {
              retval <- NULL
              if (num.nodes(object) > 0 && length(variables(object)) > 1 && length(variables(object)) != num.nodes(object))
              {
                retval <- c(retval, "incoherent number of variable names")
              }
              if (num.nodes(object) > 0 && length(dag(object)) > 1 &&
                  (ncol(dag(object)) != num.nodes(object) ||
                   nrow(dag(object)) != num.nodes(object)   ))
              {
                retval <- c(retval, "incoherent number of variables in DAG")
              }
              if (num.nodes(object) > 0 && length(wpdag(object)) > 1 &&
                  (ncol(wpdag(object)) != num.nodes(object) ||
                   nrow(wpdag(object)) != num.nodes(object)   ))
              {
                retval <- c(retval, "incoherent number of variables in WPDAG")
              }
              if(num.nodes(object) > 0 && length(discreteness(object)) > 1 &&
                 length(discreteness(object)) != num.nodes(object))
              {
                retval <- c(retval, "incoherent number of variable statuses")
              }
              if (num.time.steps(object) < 1) {
                retval <- c(retval, "impossible number of time steps in the network")
              }
              if (length(object@quantiles) > 1 && length(object@quantiles) != length(object@variables)) {
                retval <- c(retval, "incorrect list of quantiles")
              }
              
              if (is.null(retval)) return (TRUE)
              return(retval)
            }
)

# getters and setters

#' @aliases name,BN
#' @rdname name
setMethod("name", "BN", function(x) { return(slot(x, "name")) } )

#' @aliases num.nodes,BN
#' @rdname num.nodes
setMethod("num.nodes", "BN", function(x) { return(slot(x, "num.nodes")) } )

#' @aliases variables,BN
#' @rdname variables
setMethod("variables", "BN", function(x) { return(slot(x, "variables")) } )

#' @aliases discreteness,BN
#' @rdname discreteness
setMethod("discreteness",
          "BN",
          function(x)
          {
            return(slot(x, "discreteness"))
          })

#' @aliases quantiles,BN
#' @rdname quantiles
setMethod("quantiles",
          "BN",
          function(x)
          {
            return(slot(x, "quantiles"))
          })

#' @aliases node.sizes,BN
#' @rdname node.sizes
setMethod("node.sizes", "BN", function(x) { return(slot(x, "node.sizes")) } )

#' @aliases cpts,BN
#' @rdname cpts
setMethod("cpts", "BN", function(x) { return(slot(x, "cpts")) } )

#' @aliases dag,BN
#' @rdname dag
setMethod("dag", "BN", function(x) { return(slot(x, "dag")) } )

#' @aliases wpdag,BN
#' @rdname wpdag
setMethod("wpdag", "BN", function(x) { return(slot(x, "wpdag")) } )

#' @aliases scoring.func,BN
#' @rdname scoring.func
setMethod("scoring.func", "BN", function(x) { return(slot(x, "scoring.func")) } )

#' @aliases struct.algo,BN
#' @rdname struct.algo
setMethod("struct.algo", "BN", function(x) { return(slot(x, "struct.algo") ) } )

#' @aliases num.time.steps,BN
#' @rdname num.time.steps
setMethod("num.time.steps", "BN", function(x) { return(slot(x, "num.time.steps") ) } )

#' @aliases wpdag.from.dag,BN
#' @rdname wpdag.from.dag
setMethod("wpdag.from.dag",
          "BN",
          function(x, layering=NULL, layer.struct=NULL) {
            if (!inherits(x, "BN") || !inherits(dag(x),"matrix"))
              stop("The first parameter must be a 'BN' object containing a valid adjacency matrix.")
            if (!any(!is.na(dag(x))))
              stop("The adjacency matrix of the network must have at least one non-null value.")
            net <- x
            wpdag(net) <- dag.to.cpdag(dag(x), layering, layer.struct)
            return(net)
          } )

#' @name name<-
#' @aliases name<-,BN-method
#' @docType methods
#' @rdname name-set
setReplaceMethod("name",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "name") <- value
                   validObject(x)
                   return(x)
                 })

#' @name num.nodes<-
#' @aliases num.nodes<-,BN-method
#' @docType methods
#' @rdname num.nodes-set
setReplaceMethod("num.nodes",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "num.nodes") <- value
                   validObject(x)
                   return(x)
                 })


#' @name variables<-
#' @aliases variables<-,BN-method
#' @docType methods
#' @rdname variables-set
setReplaceMethod("variables",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "variables")  <- value
                   num.nodes(x) <- length(value)
                   validObject(x)
                   return(x)
                 })


#' @name discreteness<-
#' @aliases discreteness<-,BN-method
#' @docType methods
#' @rdname discreteness-set
setReplaceMethod("discreteness",
                 "BN",
                 function(x, value)
                 {
                   if (inherits(value, "character"))
                    slot(x, "discreteness") <- sapply(1:length(value), FUN=function(i){ !is.na(match(value[i],c('d',"D"))) })
                   else # is logical
                     slot(x, "discreteness") <- value
                   validObject(x)
                   return(x)
                 })

#' @name quantiles<-
#' @aliases quantiles<-,BN-method
#' @docType methods
#' @rdname quantiles-set
setReplaceMethod("quantiles",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "quantiles") <- value
                   validObject(x)
                   return(x)
                 })

#' @name node.sizes<-
#' @aliases node.sizes<-,BN-method
#' @docType methods
#' @rdname node.sizes-set
setReplaceMethod("node.sizes",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "node.sizes") <- value
                   validObject(x)
                   return(x)
                 })


#' @name cpts<-
#' @aliases cpts<-,BN-method
#' @docType methods
#' @rdname cpts-set
setReplaceMethod("cpts",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "cpts") <- value
                   validObject(x)
                   return(x)
                 })


#' @name dag<-
#' @aliases dag<-,BN-method
#' @docType methods
#' @rdname dag-set
setReplaceMethod("dag",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "dag") <- value
                   validObject(x)
                   return(x)
                 })


#' @name wpdag<-
#' @aliases wpdag<-,BN-method
#' @docType methods
#' @rdname wpdag-set
setReplaceMethod("wpdag",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "wpdag") <- value
                   validObject(x)
                   return(x)
                 })


#' @name scoring.func<-
#' @aliases scoring.func<-,BN-method
#' @docType methods
#' @rdname scoring.func-set
setReplaceMethod("scoring.func",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "scoring.func") <- value
                   return(x)
                 })


#' @name struct.algo<-
#' @aliases struct.algo<-,BN-method
#' @docType methods
#' @rdname struct.algo-set
setReplaceMethod("struct.algo",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "struct.algo") <- value
                   return(x)
                 })

#' @name num.time.steps<-
#' @aliases num.time.steps<-,BN-method
#' @docType methods
#' @rdname num.time.steps-set
setReplaceMethod("num.time.steps",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "num.time.steps") <- value
                   return(x)
                 })


#' @aliases layering,BN
#' @rdname layering
setMethod("layering",
          "BN",
          function(x)
          {
            layers <- topological.sort(dag(x))
            layers <- array(layers, dimnames = variables(x))
            return(layers)
          })


#' @aliases get.most.probable.values,BN
#' @rdname get.most.probable.values
setMethod("get.most.probable.values",
          "BN",
          function(x, prev.values = NULL)
          {
            bn   <- x
            dag  <- dag(bn)
            cpts <- cpts(bn)
            num.nodes <- num.nodes(bn)
            variables <- variables(bn)
            node.sizes <- node.sizes(bn)
            discrete   <- discreteness(bn)
            quantiles  <- quantiles(bn)

            mpv  <- array(rep(0,num.nodes), dim=c(num.nodes), dimnames=list(variables))

            sorted.nodes <- topological.sort(dag)
            
            dim.vars   <- lapply(1:num.nodes,
                                 function(x)
                                   as.list(
                                     match(
                                       c(unlist(
                                         names(dimnames(cpts[[x]]))
                                       )),
                                       c(variables)
                                     )
                                   )
            )
            
            
            for (node in sorted.nodes)
            {
              pot  <- cpts[[node]]
              vars <- c(unlist(dim.vars[[node]]))
              if (length(prev.values) == 0 || is.na(prev.values[node]))
              {
                #pot  <- cpts[[node]]
                #vars <- c(unlist(dim.vars[[node]]))

                # sum out parent variables
                if (length(dim.vars[[node]]) > 1)
                {
                  # find the dimensions corresponding to the current variable
                  for (parent in setdiff(vars, node))
                  {
                    out  <- marginalize(pot, vars, parent)
                    pot  <- out$potential
                    vars <- out$vars
                    pot <- pot / sum(pot)
                  }
                }
                
                # print(pot)
                wm <- which(!is.na(match(c(pot),max(pot))))
                if (length(wm) == 1)
                {
                  mpv[node] <- wm # pot[wm]
                }
                else
                {
                  mpv[node] <- sample(1:node.sizes[node], 1, replace=TRUE, prob=pot)
                }
              } else {
                  mpv[node] <- prev.values[node]
              }
              
              # propagate information from parent nodes to children
              children <- which(dag[node,] > 0)
              if (length(children) > 0)
              {
                for (child in children)
                {
                  out <- mult(cpts[[child]], dim.vars[[child]],
                              pot, c(node),
                              node.sizes)
                  cpts[[child]]     <- out$potential
                  dim.vars[[child]] <- out$vars
                }
              }
            }

            # sample continuous values, if possible
            if (length(quantiles) > 0) {
              for (node in sorted.nodes) {
                if (discrete[node] == FALSE && length(quantiles[[node]]) > 1) {
                  lb <- quantiles[[node]][mpv[node]]
                  ub <- quantiles[[node]][mpv[node]+1]
                  mpv[node] <- runif(1, lb, ub)
                }
              }
            }

            return(mpv)
          })


# # ' @rdname query
# # ' @aliases query,BN
# setMethod("query",
#           "BN",
#           function(x, observed.vars, observed.vals)
#           {
# #             obs <- unique.observations(observed.vars, observed.vals)
# #             observed.vars <- obs$observed.vars
# #             observed.vals <- obs$observed.vals
# #             cpts <- cpts(x)
# #             cpts <- lapply(1:length(cpts), function(x) {
# #                               if (!match(x, observed.vars)){}
# #                             })
#           })


# redefition of print() for BN objects
#' print a \code{\link{BN}}, \code{\link{BNDataset}} or \code{\link{InferenceEngine}} to \code{stdout}.
#' 
#' @method print BN
#' @name print
#' 
#' @param x a \code{\link{BN}}, \code{\link{BNDataset}} or \code{\link{InferenceEngine}}.
# ' @param ... potential other arguments.
#'
#' @rdname print
#' @aliases print,BN print.BN,BN
#' @export
#setMethod("print.BN",
#          "BN",
print.BN <- function(x, ...)
          {
            str <- "\nBayesian Network: "
            str <- paste(str, name(x), sep = '')
            str <- paste(str, "\n", sep = '')
            cat(str)
            str <- "\nnum.nodes "
            str <- paste(str, num.nodes(x), sep = '')
            str <- paste(str, "\n", sep = '')
            cat(str)
            str <- "\nvariables\n"
            cat(str)
            cat(variables(x))
            str <- "\ndiscreteness\n"
            cat(str)
            cat(discreteness(x))
            str <- "\nnode.sizes\n"
            cat(str)
            cat(node.sizes(x))
            if (num.time.steps(x) > 1) {
              str <- "\ntime steps\n"
              cat(str)
              cat(num.time.steps(x))
            }
            
            if (num.nodes(x) > 0 && (is.element(1,dag(x)) || length(which(wpdag(x) != 0)) > 0))
            {
              
              if (is.element(1,dag(x)))
              {
                colnames(dag(x)) <- variables(x)
                rownames(dag(x)) <- variables(x)
                cat('\nAdjacency matrix:\n')
                print(dag(x))
              }
                
              if (length(which(wpdag(x) != 0)) > 0)
              {
                colnames(wpdag(x)) <- variables(x)
                rownames(wpdag(x)) <- variables(x)
                cat('\nWPDAG:\n')
                print(wpdag(x))
              }  
              
              cat("\nConditional probability tables:")
              print(cpts(x))
              
            }
            
            cat("\n")
            
          }#)


#' plot a \code{\link{BN}} as a picture.
#'
#' @param x a \code{\link{BN}} object.
#' @param method either \code{default} of \code{qgraph}. The \code{default} method requires
#'        the \code{Rgraphviz} package, while \code{qgraph} requires the \code{qgraph} package
#'        and allows for a greater customization.
#' @param use.node.names \code{TRUE} if node names have to be printed. If \code{FALSE}, numbers are used instead.
#' @param node.size.lab font size for the node labels in the default mode.
#' @param node.col list of (\code{R}) colors for the nodes.
#' @param plot.wpdag if \code{TRUE} plot the network according to the WPDAG computed using bootstrap instead of the DAG.
#' @param frac minimum fraction [0,1] of presence of an edge to be plotted (used in case of \code{plot.wpdag=TRUE}).
#' @param max.weight maximum possible weight of an edge (used in case of \code{plot.wpdag=TRUE}).
#' @param ... potential further arguments when using \code{method="qgraph"}. Please refer to the
#'        \code{qgraph} documentation for the parameters available for the \code{qgraph()} method.
#' 
#' @importFrom graphics plot
#' @importFrom grDevices colors dev.off postscript
#' 
#' @name plot
#' @aliases plot,BN plot.BN,BN
#' @rdname plot
#' @export
plot.BN <- 
  function( x, method = "default", use.node.names = TRUE, frac = 0.2, max.weight = max(dag(x)),
                    node.size.lab=14, node.col = rep('white',num.nodes(x)),
                    plot.wpdag = FALSE, ...)
          {
            
            # check for required packages
            if (!requireNamespace("graph", quietly=T))
              stop("this function requires the graph package.")
            method <- tolower(method)
            if (method == "default") {
                if (!requireNamespace("Rgraphviz", quietly=T))
                    stop("this function requires the Rgraphviz package.")
            } else if (method == "qgraph") {
                if (!requireNamespace("qgraph", quietly=T))
                    stop("this function requires the qgraph package when using 'method = \"qgraph\"'.") 
            } else {
                stop("plotting method not available in bnstruct. Please use one between 'default' and 'qgraph'.")
            }

            
            # adjacency matrix
            if (plot.wpdag || (!is.element(1,dag(x)) && length(which(wpdag(x) != 0)) > 0))
              mat <- wpdag(x)
            else
              mat <- dag(x)
            
            if (plot.wpdag || (!is.element(1,dag(x)) && length(which(wpdag(x) != 0)) > 0))
            {
              if (missing(max.weight))
                max.weight <- max(mat)
              if (missing(node.col))
                node.col <- rep('white',ncol(mat))
            }
            
            num.nodes <- num.nodes(x)
            variables <- variables(x)

            if (method == "default") {
                bngzplot(x, mat, num.nodes, variables, use.node.names,
                            frac, max.weight, node.size.lab, node.col)
            } else {
                plist <- list(...)
                bnqgplot(mat, num.nodes, variables, use.node.names,
                            frac, max.weight, node.col, plist)
            }
}            

bngzplot <- function(x, mat, num.nodes, variables, use.node.names, frac, max.weight,
                      node.size.lab, node.col) {

            mat.th <- mat
            mat.th[mat <  frac*max.weight] <- 0
            mat.th[mat >= frac*max.weight] <- 1
            
            # node names
            if (use.node.names && length(variables) > 0)
              node.names <- variables
            else
              node.names <- as.character(1:num.nodes)
            # build graph
            rownames(mat.th) <- node.names
            colnames(mat.th) <- node.names

            # node colors
            node.fill <- as.list(node.col)
            names(node.fill) <- node.names

            g <- new("graphAM", adjMat=mat.th, edgemode="directed")

            en <- Rgraphviz::edgeNames(g,recipEdges="distinct")
            if (sum(mat) == 0) {
              g <- Rgraphviz::layoutGraph(g, layoutType="neato")
            } else {
              g <- Rgraphviz::layoutGraph(g)
            }

            # set edge darkness proportional to confidence
            conf <- mat.th*pmax(mat,t(mat)) # both values to the maximum for edges with 2 directions
            col <- colors()[253-100*(t(conf)[t(conf) >= frac*max.weight]/max.weight)]
            if (sum(mat) == 0) {
              col <- rep(colors()[1], length(en))
            }
            names(col) <- en
            
            # remove arrowheads from undirected edges
            ahs <- graph::edgeRenderInfo(g)$arrowhead
            ats <- graph::edgeRenderInfo(g)$arrowtail
            dirs <- graph::edgeRenderInfo(g)$direction
            ahs[dirs=="both"] <- ats[dirs=="both"] <- "none"
            graph::edgeRenderInfo(g) <- list(col=col,lwd=2,arrowhead=ahs,arrowtail=ats)
            graph::nodeRenderInfo(g) <- list(fill=node.fill, fontsize=node.size.lab)

            Rgraphviz::renderGraph(g)
}

bnqgplot <- function(mat, num.nodes, variables, use.node.names,
                    frac, max.weight, node.col, ...) {

            # parse ... parameters
            plist <- unlist(list(...), recursive=F)
            parnames <- names(plist)

            if (use.node.names && length(variables) > 0)
              node.names <- variables
            else
              node.names <- as.character(1:num.nodes)

            # build graph
            rownames(mat) <- node.names
            colnames(mat) <- node.names

            # node colors
            node.fill <- as.list(node.col)
            names(node.fill) <- node.names
            plist[["input"]] <- mat

            if (!("color" %in% parnames)) {
                plist[["color"]] <- node.col
            }
            if (!("minumum" %in% parnames)) {
                plist[["minimum"]] <- frac * max.weight
            }
            if (!("directed" %in% parnames)) {
                plist[["directed"]] <- TRUE
            }

            if (!("labels" %in% parnames)) {
                plist[["labels"]] <- node.names
            }

            do.call(qgraph::qgraph, plist)
}


# save BN as eps file
#' @rdname save.to.eps
#' @aliases save.to.eps,BN,character
setMethod("save.to.eps",
          c("BN", "character"),
          function(x, filename, ...)
          {
            # problem: I wanted to set filename=NULL in the declaration, but I cannot manage to
            # make it work in case of missing filename...
            
            # problem 2: cannot make dag.to.cpdag work...
            postscript(filename)
            plot(x, ...)
            dev.off()
          })


#' @rdname sample.row
#' @aliases sample.row,BN
setMethod("sample.row", "BN",
          function(x, mar=0){
            
            if (mar < 0 || mar > 1) {
              bnstruct.log("warning: non-admissible value for mar, set to 0")
              mar <- 0
            }
            
            bn   <- x
            dag  <- dag(bn)
            cpts <- cpts(bn)
            num.nodes    <- num.nodes(bn)
            variables    <- variables(bn)
            node.sizes   <- node.sizes(bn)
            discreteness <- discreteness(bn)
            quantiles    <- quantiles(bn)
            
            mpv  <- array(rep(0,num.nodes), dim=c(num.nodes), dimnames=list(variables))
            
            parents      <- lapply(1:num.nodes, function(x) which(dag[,x] != 0))
            sorted.nodes <- topological.sort(dag)
            
            dim.vars   <- lapply(1:num.nodes,
                                 function(x)
                                     match(
                                       c(unlist(
                                         names(dimnames(cpts[[x]]))
                                       )),
                                       c(variables)
                                     )
            )
            
            for (node in sorted.nodes)
            {
              if (length(parents[[node]]) == 0) {
                mpv[node] <- sample(1:node.sizes[node], 1, replace=T, prob=cpts[[node]])
              } else {
                cpt  <- cpts[[node]]
                vars <- c(unlist(dim.vars[[node]]))
                for (p in parents[[node]]) {
                  sumout          <- rep(0, node.sizes[p])
                  sumout[mpv[p]] <- 1
                  out  <- mult(cpt, vars, sumout, c(p), node.sizes)
                  cpt  <- out$potential
                  vars <- out$vars
                }
                cpt <- c(cpt[which(cpt != 0)])
                mpv[node] <- sample(1:node.sizes[node], 1, replace=T, prob=cpt)
              }
            }

            # sample continuous values, if possible
            if (length(quantiles) > 0) {
              for (node in sorted.nodes) {
                if (discreteness[node] == FALSE && length(quantiles[[node]]) > 1) {
                  lb <- quantiles[[node]][mpv[node]]
                  ub <- quantiles[[node]][mpv[node]+1]
                  mpv[node] <- runif(1, lb, ub)
                }
              }
            }
            
            if (mar > 0) {            
              missing.prob <- runif(num.nodes, 0, 1)
              mpv[which(missing.prob < mar)] <- NA
            }
                
            return(mpv)
          })


#' @rdname sample.dataset
#' @aliases sample.dataset,BN
setMethod("sample.dataset",c("BN"),
          function(x, n = 100, mar=0)
          {
            if (mar < 0 || mar > 1) {
              bnstruct.log("warning: non-admissible value for mar, set to 0")
              mar <- 0
            }
            
            num.nodes <- num.nodes(x)          
            obs <- matrix(rep(0, num.nodes * n), nrow = n, ncol = num.nodes)
            
            for (i in 1:n)
              obs[i,] <- sample.row(x, mar)
            
            #storage.mode(obs) <- "integer"
            
            bnd <- BNDataset(obs, discreteness(x), variables(x), node.sizes(x))
           
            return(bnd)
          })

#' convert a DAG to a CPDAG
#' 
#' Convert the adjacency matrix representing the DAG of a \code{\link{BN}}
#' into the adjacency matrix representing a CPDAG for the network.
#' 
#' @name dag.to.cpdag
#' @rdname dag.to.cpdag
#' 
#' @param dag.adj.matrix the adjacency matrix representing the DAG of a \code{\link{BN}}.
#' @param layering vector containing the layers where each node belongs.
#' @param layer.struct layer.struct \code{0/1} matrix for indicating which layers can contain parent nodes
#'        for nodes in a layer (only for \code{mmhc}, \code{mmpc}).
#' 
#' @return the adjacency matrix representing a CPDAG for the network.
#' 
#' @seealso \code{\link{wpdag.from.dag}}
#' 
#' @examples
#' \dontrun{
#' net <- learn.network(dataset, layering=layering, layer.struct=layer.struct)
#' pdag <- dag.to.cpdag(dag(net), layering, layer.struct)
#' wpdag(net) <- pdag
#' }
#' 
#' @export
dag.to.cpdag <- function(dag.adj.matrix, layering = NULL, layer.struct = NULL)
{
  if (!inherits(dag.adj.matrix, "matrix"))
    stop("The first parameter must be a 'matrix' representing the adjacency matrix of a network.")
  if (!any(!is.na(dag.adj.matrix)))
    stop("The adjacency matrix must have at least one non-null value.")
  return(abs(label.edges(dag.adj.matrix, layering, layer.struct)))
}


#' counts the edges in a WPDAG with their directionality
#'
#' Given a \code{BN} with a \code{WPDAG}, it counts the edges, with
#' their directionality.
#'
#' @param x the \code{BN}
#' @param use.node.names use node names rather than number (\code{TRUE} by default).
#'
#' @return a matrix containing the node pairs with the count of the edges
#'         between them in the \code{WPDAG}.
#'
#' @name edge.dir.wpdag
#' @rdname edge.dir.wpdag
#'
#' @export
edge.dir.wpdag <- function (x, use.node.names = TRUE)
{
  if (!is.element(1, dag(x)) && length(which(wpdag(x) != 0)) > 0)
    mat <- wpdag(x)
  else
    mat <- dag(x)

  num.nodes <- num.nodes(x)
  variables <- variables(x)

    if (use.node.names && length(variables) > 0)
    node.names <- variables
  else node.names <- as.character(1:num.nodes)

  rownames(mat) <- node.names
  colnames(mat) <- node.names

  # Indexes of element inside WPDAG lower triangular matrix
  ltel <- which(lower.tri(mat, diag = FALSE), arr.ind=T)
  # Respective elements inside WPDAG upper triangular matrix
  utel <- cbind(ltel[,2], ltel[,1])

  # Find pairs of nodes [i,j] with an edge directed from i to j
  nodes.dir.l <- ltel[which(mat[ltel]>mat[utel]), ]
  nodes.dir.u <- utel[which(mat[utel]>mat[ltel]), ]

  ### Find number of occurrences of directed edges ###
  # Count edges occurence for each pair of nodes
  edge.occurr.l <- mat[nodes.dir.l]
  edge.occurr.u <- mat[nodes.dir.u]
  edge.occurr <- c(edge.occurr.l, edge.occurr.u)
  # Once pairs of nodes with directed edge have been found, count the occurences of that edge in the opposite direction
  nodes.rev.l <- cbind(nodes.dir.l[,2], nodes.dir.l[,1])
  nodes.rev.u <- cbind(nodes.dir.u[,2], nodes.dir.u[,1])
  edge.rev.occurr.l <- mat[nodes.rev.l]
  edge.rev.occurr.u <- mat[nodes.rev.u]
  edge.rev.occurr <- c(edge.rev.occurr.l, edge.rev.occurr.u)

  # Name of nodes with directed edges
  nodes.start.l <- rownames(mat)[nodes.dir.l[,1]]
  nodes.stop.l <- rownames(mat)[nodes.dir.l[,2]]

  nodes.start.u <- rownames(mat)[nodes.dir.u[,1]]
  nodes.stop.u <- rownames(mat)[nodes.dir.u[,2]]
  # Combine together pairs of nodes (edges from nodes.start to nodes.stop)
  nodes.start <- c(nodes.start.l, nodes.start.u)
  nodes.stop <- c(nodes.stop.l, nodes.stop.u)
  nodes.dir <- cbind(nodes.start, nodes.stop)
  # Combine pairs of nodes (names) with info about edges occurrence 
  edge.directed.wpdag <- cbind(nodes.dir, edge.occurr, edge.rev.occurr)

  return(edge.directed.wpdag)
}



label.edges <- function(dgraph, layering = NULL, layer.struct = NULL)
{
  # LABEL-EDGES produce a N*N matrix which values are
  # 	+1 if the edge is compelled or
  #	-1 if the edge is reversible.
  n.nodes <- nrow(dgraph)
  o <- order.edges(dgraph)
  order <- o$order
  xedge <- o$x
  yedge <- o$y
  
  label <- 2*dgraph
  NbEdges <- length(xedge)
  
  # edges between layers are compelled
  if( !is.null(layering) )
  {
    n.layers = length(unique(layering))
    for( l in 1:(n.layers-1) ) {
      label[ intersect(xedge,which(layering==l)), intersect(yedge,which(layering>l)) ] <- 
      dgraph[ intersect(xedge,which(layering==l)), intersect(yedge,which(layering>l)) ]
    }    
  } 
  
  for( Edge in 1:NbEdges)
  {
    xlow <- xedge[Edge]
    ylow <- yedge[Edge]
    if( label[xlow,ylow] == 2 )
    {
      fin <- 0
      wcompelled <- which(label[,xlow] == 1)
      parenty <- which(label[,ylow] != 0)
      
      for( s in seq_len(length(wcompelled)) )
      {
        w <- wcompelled[s]
        if( !(w %in% parenty) )
        {
          label[parenty,ylow] <- 1
          fin <- 1
        }
        else if( fin == 0 ) label[w,ylow] <- 1
      }
      
      if( fin == 0 )
      {
        parentx <- c(xlow,which(label[,xlow] != 0))
        
        if( length(setdiff(parenty,parentx) > 0) )
          label[which(label[,ylow] == 2), ylow] <- 1
        else
        {
          label[xlow,ylow] <- -1
          label[ylow,xlow] <- -1
          ttp <- which(label[,ylow] == 2)
          label[ttp,ylow] <- -1
          label[ylow,ttp] <- -1
        }
      }
    }
  }

  # enforce layer.struct, if present
  if (!is.null(layering) && !is.null(layer.struct)) {
    layers <- matrix(1L, n.nodes, n.nodes)
    for (i in 1:n.layers) {
        for (j in 1:n.layers) {
            layers[which(layering == i), which(layering == j)] <- layer.struct[i, j]
        }
    }
    diag(layers) <- 0
    label <- label & layers
  }

  return(label)
}

order.edges <- function(dgraph)
  # ORDER_EDGES produce a total (natural) ordering over the edges in a DAG.
{
  N <- nrow(dgraph)
  order <- matrix(c(0),N,N)
  
  node_order <- topological.sort(dgraph)
  oo <- sort(node_order,index.return=TRUE)$ix
  dgraph <- dgraph[oo,oo]
  xy <- which(dgraph == 1, arr.ind = TRUE)
  nb.edges <- nrow(xy)
  
  if( nb.edges != 0)
    order[xy] <- 1:nb.edges
  
  order <- order[node_order,node_order]
  x <- oo[xy[,1]]
  y <- oo[xy[,2]]
  
  return(list(order=order,x=x,y=y))
}

topological.sort <- function(dgraph)
  # TOPOLOGICAL_SORT Return the nodes in topological order (parents before children).
{
  n <- nrow(dgraph)
  
  # assign zero-indegree nodes to the top
  fringe <- which( colSums(dgraph)==0 )
  order <- rep(0,n)
  
  i <- 1
  while( length(fringe) > 0 )
  {
    ind <- utils::head(fringe,1) # pop
    fringe <- utils::tail(fringe,-1)
    order[i] <- ind
    i <- i + 1 
    
    for( j in which(dgraph[ind,] != 0) )
    {
      dgraph[ind,j] <- 0
      if( sum(dgraph[,j]) == 0 )
        fringe <- c(fringe,j)
    }
  }
  
  return(order)
}

