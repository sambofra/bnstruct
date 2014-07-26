#' Constructor method of \code{\link{InferenceEngine}} class.
#'
#' @name InferenceEngine
#' @rdname InferenceEngine-class
setMethod("initialize",
          "InferenceEngine",
          function(.Object, ...)  
          {
            validObject(.Object)      
            .Object
          })

#' Wrapper for \code{\link{InferenceEngine}} object
#' 
#' @name InferenceEngine
#' @rdname InferenceEngine-class
#' @export
InferenceEngine <- function(...)
{
  object <- new("InferenceEngine", ...)
  object
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
              if (is.null(retval)) return (TRUE)
              return (retval)
            }
)


setMethod("num.nodes", "InferenceEngine", function(x) slot(x, "num.nodes"))

setMethod("junction.tree", "InferenceEngine", function(x) slot(x, "junction.tree"))

setMethod("jt.cliques", "InferenceEngine", function(x) slot(x, "cliques"))

setMethod("jpts", "InferenceEngine", function(x) slot(x, "jpts"))


setReplaceMethod("num.nodes",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "num.nodes") <- value
                   validObject(x)
                   x
                 })


# @name junction.tree
# @rdname mutators-methods
setReplaceMethod("junction.tree",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "junction.tree") <- value
                   slot(x, "num.nodes")     <- length(value)
                   validObject(x)
                   x
                 })


# @name cliques
# @rdname mutators-methods
setReplaceMethod("jt.cliques",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "cliques")   <- value
                   slot(x, "num.nodes") <- length(value)
                   validObject(x)
                   x
                 })


# @name jpts
# @rdname accessors-methods
setReplaceMethod("jpts",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "jpts") <- value
                   validObject(x)
                   x
                 })


# redefition of print() for InferenceEngine objects
#' @rdname print.InferenceEngine-methods
#' @aliases print.InferenceEngine,InferenceEngine-methods
setMethod("print.InferenceEngine",
          "InferenceEngine",
          function(x, container = "jt", ...)
          {
            if (container == 'jt')
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
