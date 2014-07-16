#' Constructor for \link{JunctionTree} object
#' 
#' @name JunctionTree
#' @rdname JunctionTree-class
#' @export JunctionTree
JunctionTree <- function(...) new("JunctionTree", ...)

# validator
setValidity("JunctionTree",
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

# redefition of print() for JunctionTree objects
setMethod("print.JunctionTree",
          "JunctionTree",
          function(object, ...)
          {
            str <- "\nJunction Tree "
            str <- paste(str, "with ", sep = '')
            str <- paste(str, object@num.nodes, sep = '')
            str <- paste(str, " cliques", sep = '')
            message(str)
            
            if (object@num.nodes > 0)
            {          
              str <- ""
              clnames <- NULL
              for (i in 1:object@num.nodes)
              {
                clname <- ''
                clname <- paste(clname, '(', sep = '')
                clname <- paste(clname, paste(sort(object@cliques[[i]]), sep="", collapse=','))
                clname <- paste(clname, ')', sep = '')
                clnames[[i]] <- clname
                str <- paste(str, clname, sep = ' ')
              }
              colnames(object@junction.tree) <- clnames
              rownames(object@junction.tree) <- clnames
              
              message(str, '\n\nAdjacency matrix:')
              print(object@junction.tree)
              message("")
            }
            
          })
