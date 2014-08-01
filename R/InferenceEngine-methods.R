#' Constructor method of \code{\link{InferenceEngine}} class.
#'
#' @name InferenceEngine
#' @rdname InferenceEngine-class
#' @aliases initialize,InferenceEngine-method
#' 
#' @param .Object an empty InferenceEngine object.
#' 
#' @return an InferenceEngine object.
setMethod("initialize",
          c("InferenceEngine"),
          function(.Object, ...)  
          {
            validObject(.Object)
            return(.Object)
          })

#' constructor for \code{\link{InferenceEngine}} object
#' 
#' @name InferenceEngine
#' @rdname InferenceEngine-class
#' @aliases InferenceEngine
#' 
#' @param bn a \code{\link{BN}} object.
#' @param observations a list of observations composed by the two following vectors:
#' \itemize{
#' \item{\code{observed.vars}:}{vector of observed variables;}
#' \item{\code{observed.vals}:}{vector of values observed for the variables in \code{observed.vars} in the corresponding position.}
#' }
#' @param ... potential further arguments of methods.
#' 
#' @return InferenceEngine object.
#' 
#' @examples
#' \dontrun{
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' bn <- BN(dataset)
#' eng <- InferenceEngine(bn)
#' 
#' obs <- list(c("A","G,"X),c(1,2,1))
#' eng.2 <- InferenceEngine(bn, obs)
#' }
#' 
#' @export 
InferenceEngine <- function(bn = NULL, observations = NULL, ...)
{
  object <- new("InferenceEngine", bn, observations, ...)
  
  if (!is.null(bn))
    bn(object, updated.bn = FALSE) <- bn
  
  if (!is.null(observations))
    observations(object) <- observations
  
  if (!is.null(bn))
  {
    object <- build.junction.tree(object, dag(bn))
  }
  return(object)
}

# validator
setValidity("InferenceEngine",
            function(object)
            {
              retval <- NULL
              if (num.nodes(object) > 0 && length(jt.cliques(object)) > 1 &&
                  length(jt.cliques(object)) != num.nodes(object))
              {
                retval <- c(retval, "incoherent number of cliques")
              }
#               if (num.nodes(object) > 0 && length(c(junction.tree(object))) > 1 && 
#                  (ncol(junction.tree(object)) != num.nodes(object) ||
#                   nrow(junction.tree(object)) != num.nodes(object)   ))
#               {
#                 retval <- c(retval, "incoherent number of variables in Junction Tree")
#               }

              obs <- observations(object)

              if (length(obs[[1]]) != length(obs[[2]]))
              {
                retval <- c(retval, "incoherent number of observed variables and values")
              }

              if (is.null(retval)) return (TRUE)
              return (retval)
            }
)


#' @rdname num.nodes
#' @aliases num.nodes,InferenceEngine
setMethod("num.nodes", "InferenceEngine", function(x) slot(x, "num.nodes"))

#' @rdname junction.tree
#' @aliases junction.tree,InferenceEngine
setMethod("junction.tree", "InferenceEngine", function(x) slot(x, "junction.tree"))

#' @rdname jt.cliques
#' @aliases jt.cliques,InferenceEngine
setMethod("jt.cliques", "InferenceEngine", function(x) slot(x, "cliques"))

#' @rdname jpts
#' @aliases jpts,InferenceEngine
setMethod("jpts", "InferenceEngine", function(x) slot(x, "jpts"))

#' @rdname bn-method
#' @aliases bn,InferenceEngine
setMethod("bn",
           "InferenceEngine",
           function(x, updated.bn = TRUE)
           {
             if (updated.bn && !is.null(slot(x, "updated.bn")))
               return(slot(x, "updated.bn"))
             
             if (updated.bn && is.null(slot(x, "updated.bn")) && !is.null(slot(x, "bn")))
             {
               message("Updated network not yet computed, returning the original one.")
               return(slot(x, "bn"))
             }
             
             if (is.null(slot(x, "bn")))
             {
               message("No network present.")
               return(NULL)
             }
             
             return(slot(x, "bn"))
           })


#' @rdname observations
#' @aliases observations,InferenceEngine
setMethod("observations",
          "InferenceEngine",
          function(x)
          {
            return(list("observed.vars" = slot(x, "observed.vars"), "observed.vals" = slot(x, "observed.vals")))
          })


#' @name num.nodes<-
#' @aliases num.nodes<-,InferenceEngine-method
#' @docType methods
#' @rdname num.nodes-set
setReplaceMethod("num.nodes",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "num.nodes") <- value
                   validObject(x)
                   x
                 })


#' @name junction.tree<-
#' @aliases junction.tree<-,InferenceEngine-method
#' @docType methods
#' @rdname junction.tree-set
setReplaceMethod("junction.tree",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "junction.tree") <- value
                   slot(x, "num.nodes")     <- length(value)
                   validObject(x)
                   x
                 })


#' @name jt.cliques<-
#' @aliases jt.cliques<-,InferenceEngine-method
#' @docType methods
#' @rdname jt.cliques-set
setReplaceMethod("jt.cliques",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "cliques")   <- value
                   slot(x, "num.nodes") <- length(value)
                   validObject(x)
                   x
                 })


#' @name jpts<-
#' @aliases jpts<-,InferenceEngine-method
#' @docType methods
#' @rdname jpts-set
setReplaceMethod("jpts",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "jpts") <- value
                   validObject(x)
                   x
                 })


#' @name bn<-
#' @aliases bn<-,InferenceEngine-method
#' @docType methods
#' @rdname bn-set
setReplaceMethod("bn",
                 c("InferenceEngine"),
                 function(x, updated.bn = TRUE, ..., value)
                 {
                   if (class(value) != "BN")
                   {
                     message("Value argument is not a BN object.\nLeaving InferenceEngine untouched.")
                     return(x)
                   }
                   
                   if (updated.bn)
                     slot(x, "updated.bn") <- value
                   else
                     slot(x, "bn") <- value

                   validObject(x)
                   x
                 })


# I assume that the user knows what he/she does...
#' @name observations<-
#' @aliases observations<-,InferenceEngine-method
#' @docType methods
#' @rdname observations-set
setReplaceMethod("observations",
                 "InferenceEngine",
                 function(x, value)
                 {
                   ovrs <- c(unlist(value[[1]]))
                   ovls <- c(unlist(value[[2]]))
                   obs  <- unique.observations(ovrs, ovls)
                   slot(x, "observed.vars") <- obs$observed.vars
                   slot(x, "observed.vals") <- obs$observed.vals
                   validObject(x)
                   x
                 })


#' @name add.observations<-
#' @aliases add.observations<-,InferenceEngine-method
#' @docType methods
#' @rdname add.observations-set
setReplaceMethod("add.observations",
                 "InferenceEngine",
                 function(x, value)
                 {
                   ovrs <- c(slot(x, "observed.vars"), c(unlist(value[[1]])))
                   ovls <- c(slot(x, "observed.vals"), c(unlist(value[[2]])))
                   obs  <- unique.observations(ovrs, ovls)
                   slot(x, "observed.vars") <- obs$observed.vars
                   slot(x, "observed.vals") <- obs$observed.vals
                   validObject(x)
                   x
                 })


#' @rdname test.updated.bn
#' @aliases test.updated.bn,InferenceEngine
setMethod("test.updated.bn",
          "InferenceEngine",
          function(x)
            !is.null(slot(x, "updated.bn"))
          )

#' @rdname layering
#' @aliases layering,InferenceEngine
setMethod("layering",
          "InferenceEngine",
          function(x, updated.bn = TRUE, ...)
          {
            layers <- topological.sort(dag(bn(x, updated.bn)))
            layers <- array(layers, dimnames = variables(bn(x, updated.bn)))
            layers
          })


# TODO replace with method based on cliques, should be much faster
#' @rdname get.most.probable.values
#' @aliases get.most.probable.values,InferenceEngine
setMethod("get.most.probable.values",
          "InferenceEngine",
          function(x, ...)
          {
            get.most.probable.values(bn(x))
          })


# redefition of print() for InferenceEngine objects
#' @rdname print
#' @aliases print,InferenceEngine
setMethod("print",
          "InferenceEngine",
          function(x, engine = "jt", ...)
          {
            if (engine == 'jt')
            {
              str <- "\nJunction Tree "
              str <- paste(str, "with ", sep = '')
              str <- paste(str, num.nodes(x), sep = '')
              str <- paste(str, " cliques", sep = '')
              cat(str)
              
              if (num.nodes(x) > 0)
              {          
                str <- ""
                clnames <- NULL
                for (i in 1:num.nodes(x))
                {
                  clname <- ''
                  clname <- paste(clname, '(', sep = '')
                  clname <- paste(clname, paste(sort(jt.cliques(x)[[i]]), sep="", collapse=','))
                  clname <- paste(clname, ')', sep = '')
                  clnames[[i]] <- clname
                  str <- paste(str, clname, sep = ' ')
                }
                colnames(x@junction.tree) <- clnames
                rownames(x@junction.tree) <- clnames
                
                cat(str, '\n\nAdjacency matrix:')
                print(junction.tree(x))
              }
            }
            
          })

# keep last (most recent) observation for each var
unique.observations <- function(observed.vars, observed.vals)
{
  ovrs <- c(unlist(observed.vars))
  ovls <- c(unlist(observed.vals))
  dup  <- which(duplicated(rev(ovrs)) == TRUE)
  ovrs <- rev(rev(ovrs)[-dup])
  ovls <- rev(rev(ovls)[-dup])
  return(list("observed.vars"=ovrs, "observed.vals"=ovls))
}
