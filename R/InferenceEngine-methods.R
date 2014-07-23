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
              if (object@num.nodes > 0 && length(object@cliques) != object@num.nodes)
              {
                retval <- c(retval, "incoherent number of cliques")
              }
              if (object@num.nodes > 0 && (ncol(object@junction.tree) != object@num.nodes ||
                                             nrow(object@junction.tree) != object@num.nodes   ))
              {
                retval <- c(retval, "incoherent number of variables in Junction Tree")
              }
              if (is.null(retval)) return (TRUE)
              return (retval)
            }
)

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
              str <- paste(str, x@num.nodes, sep = '')
              str <- paste(str, " cliques", sep = '')
              message(str)
              
              if (x@num.nodes > 0)
              {          
                str <- ""
                clnames <- NULL
                for (i in 1:x@num.nodes)
                {
                  clname <- ''
                  clname <- paste(clname, '(', sep = '')
                  clname <- paste(clname, paste(sort(x@cliques[[i]]), sep="", collapse=','))
                  clname <- paste(clname, ')', sep = '')
                  clnames[[i]] <- clname
                  str <- paste(str, clname, sep = ' ')
                }
                colnames(x@junction.tree) <- clnames
                rownames(x@junction.tree) <- clnames
                
                message(str, '\n\nAdjacency matrix:')
                print(x@junction.tree)
                message("")
              }
            }
            
          })
