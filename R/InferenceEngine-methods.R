#' Constructor method of \code{\link{InferenceEngine}} class.
#'
#' @name InferenceEngine
#' @rdname InferenceEngine-class
setMethod("initialize",
          c("InferenceEngine"),
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

              obs <- observations(object)

              if (length(obs[[1]]) != length(obs[[2]]))
              {
                retval <- c(retval, "incoherent number of observed variables and values")
              }

              if (is.null(retval)) return (TRUE)
              return (retval)
            }
)


setMethod("num.nodes", "InferenceEngine", function(x) slot(x, "num.nodes"))

setMethod("junction.tree", "InferenceEngine", function(x) slot(x, "junction.tree"))

setMethod("jt.cliques", "InferenceEngine", function(x) slot(x, "cliques"))

setMethod("jpts", "InferenceEngine", function(x) slot(x, "jpts"))

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


setMethod("observations",
          "InferenceEngine",
          function(x)
          {
            return(list("observed.vars" = slot(x, "observed.vars"), "observed.vals" = slot(x, "observed.vals")))
          })


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


setReplaceMethod("bn",
                 "InferenceEngine",
                 function(x, updated.bn = TRUE, value)
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
setReplaceMethod("observations",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "observed.vars") <- c(unlist(value[[1]]))
                   slot(x, "observed.vals") <- c(unlist(value[[2]]))
                   validObject(x)
                   x
                 })


setReplaceMethod("add.observations",
                 "InferenceEngine",
                 function(x, value)
                 {
                   slot(x, "observed.vars") <- c(slot(x, "observed.vars"), c(unlist(value[[1]])))
                   slot(x, "observed.vals") <- c(slot(x, "observed.vals"), c(unlist(value[[2]])))
                   validObject(x)
                   x
                 })


setMethod("test.updated.bn",
          "InferenceEngine",
          function(x)
            !is.null(slot(x, "updated.bn"))
          )

# TODO replace with method based on cliques, should be much faster
setMethod("get.most.probable.values",
          "InferenceEngine",
          function(x, ...)
          {
            get.most.probable.values(bn(x))
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
