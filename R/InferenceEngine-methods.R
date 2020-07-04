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
#' @param interventions a list of interventions composed of the following two vectors:
#' \itemize{
#' \item{\code{intervention.vars}:}{vector of variables for which an intervention has been performed;}
#' \item{\code{intervention.vals}:}{vector of values chosen for the variables in \code{intervention.vals} in the corresponding position.}
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
InferenceEngine <- function(bn = NULL, observations = NULL, interventions = NULL, ...)
{
  object <- new("InferenceEngine", bn, observations, interventions, ...)
  
  if (!is.null(bn))
    bn(object) <- bn
  
  if (!is.null(observations))
    observations(object) <- observations

  if (!is.null(interventions))
    interventions(object) <- interventions
  
  if (!is.null(bn))
  {
    dag <- dag(bn)
    if (!is.null(interventions)) {
      intervention.vars <- interventions[[1]]
      intervention.vals <- interventions[[2]]
      intervs <- unique.observations(intervention.vars, intervention.vals)
      intervs.vars <- intervs$observed.vars
      for (i in intervs.vars) {
        dag[,i] <- 0
      }
      b <- bn(object)
      dag(b) <- dag
      bn(object) <- b
    }

    object <- build.junction.tree(object, dag)

    if (!is.null(interventions)) {
      object <- belief.propagation(object, interventions)
      u <- updated.bn(object)
      bn(object) <- u
    }
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

              ints <- interventions(object)
              if (length(ints[[1]]) != length(ints[[2]]))
              {
                retval <- c(retval, "incoherent number of variables and values for the intervention.")
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
           function(x)
           {
             if (is.null(slot(x, "bn")))
             {
               message("No network present.")
               return(NULL)
             }
             return(slot(x, "bn"))
           })

#' @rdname updated.bn-method
#' @aliases updated.bn,InferenceEngine
setMethod("updated.bn",
          "InferenceEngine",
          function(x)
          {
            if (is.null(slot(x, "updated.bn")))
            {
              message("No updated network present.")
              return(NULL)
            }
            return(slot(x, "updated.bn"))
          })


#' @rdname observations
#' @aliases observations,InferenceEngine
setMethod("observations",
          "InferenceEngine",
          function(x)
          {
            return(list("observed.vars" = slot(x, "observed.vars"), "observed.vals" = slot(x, "observed.vals")))
          })


#' @rdname interventions
#' @aliases interventions,InferenceEngine
setMethod("interventions",
          "InferenceEngine",
          function(x)
          {
            return(list("intervention.vars" = slot(x, "intervention.vars"), "intervention.vals" = slot(x, "intervention.vals")))
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
                 function(x, value)
                 {
                   slot(x, "bn") <- value
                   validObject(x)
                   x
                 })


#' @name updated.bn<-
#' @aliases updated.bn<-,InferenceEngine-method
#' @docType methods
#' @rdname updated.bn-set
setReplaceMethod("updated.bn",
                 c("InferenceEngine"),
                 function(x, value)
                 {
                   slot(x, "updated.bn") <- value
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


# Should never be used outside...
#' @name interventions<-
#' @aliases interventions<-,InferenceEngine-method
#' @docType methods
#' @rdname interventions-set
setReplaceMethod("interventions",
                 "InferenceEngine",
                 function(x, value)
                 {
                   ivrs <- c(unlist(value[[1]]))
                   ivls <- c(unlist(value[[2]]))
                   ivs  <- unique.observations(ivrs, ivls)
                   slot(x, "intervention.vars") <- ivs$observed.vars
                   slot(x, "intervention.vals") <- ivs$observed.vals
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

# # ' @rdname layering
# # ' @aliases layering,InferenceEngine
# setMethod("layering",
#           "InferenceEngine",
#           function(x, updated.bn = TRUE, ...)
#           {
#             if (updated.bn)
#               layers <- topological.sort(dag(updated.bn(x)))
#             else
#               layers <- topological.sort(dag(bn(x)))
#             layers <- array(layers, dimnames = variables(bn(x)))
#             layers
#           })


#' @rdname get.most.probable.values
#' @aliases get.most.probable.values,InferenceEngine
setMethod("get.most.probable.values",
          "InferenceEngine",
          function(x, prev.values = NULL)
          {
#             if (is.null(jpts(x))) # don't know if this works...
#               return(get.most.probable.values(bn(x)))
            jpts         <- jpts(x)
            num.nodes    <- num.nodes(bn(x))
            cliques      <- jt.cliques(x)
            num.cliqs    <- length(cliques)
            variables    <- variables(bn(x))
            node.sizes   <- node.sizes(bn(x))
            discreteness <- discreteness(bn(x))
            quantiles    <- quantiles(bn(x))
            
            
            mpv <- array(rep(NA, num.nodes), dim=c(num.nodes), dimnames=list(variables))

            dim.vars   <- lapply(1:num.cliqs,
                                 function(index)
                                   as.list(
                                     match(
                                       c(unlist(
                                         names(dimnames(jpts[[index]]))
                                       )),
                                       c(variables)
                                     )
                                   )
            )
            

            for (i in 1:num.nodes)
            {
              if (length(prev.values) == 0 || is.na(prev.values[i]))
              {
                target.clique <- which(sapply(1:num.cliqs,
                                                  function(index){
                                                      is.element(
                                                          i,
                                                          unlist(dim.vars[[index]])
                                                      )
                                                  }
                                              ) == TRUE)[1]
                pot  <- jpts[[target.clique]]
                vars <- c(unlist(dim.vars[[target.clique]]))
 
                for (v in c(unlist(setdiff(vars,i))))
                {
                  out  <- marginalize(pot, vars, v)
                  pot  <- out$potential
                  vars <- out$vars
                  pot  <- pot / sum(pot)
                }
                wm <- which(!is.na(match(c(pot),max(pot))))
                if (length(wm) == 1)
                {
                  mpv[i] <- wm # pot[wm]
                }
                else
                {
                  mpv[i] <- sample(1:node.sizes[i], 1, replace=T, prob=pot) #,replace=TRUE
                }

                # check if the variable was continuous;
                # sample a uniform value in the quantile if it's the case
                if (discreteness[i] == FALSE) {
                  if (length(quantiles) <= 0 || length(quantiles[[i]]) < 1) {
                    stop("error: no quantization available")
                  }
                  lb <- quantiles[[i]][mpv[i]]
                  ub <- quantiles[[i]][mpv[i]+1]
                  mpv[i] <- runif(1, lb, ub)
                }
              } else {
                mpv[i] <- prev.values[i]
              }
            }
           
            return(mpv)
          })

# # ' @rdname sample.row
# # ' @aliases sample.row,InferenceEngine
# setMethod("sample.row",
#           "InferenceEngine",
#           function(x)
#           {
#             jpts      <- jpts(x)
#             num.nodes <- num.nodes(bn(x))
#             cliques   <- jt.cliques(x)
#             num.cliqs <- length(cliques)
#             variables <- variables(bn(x))
#             
#             mpv <- array(rep(0, num.nodes), dim=c(num.nodes), dimnames=list(variables))
#             
#             dim.vars   <- lapply(1:num.cliqs,
#                                  function(index)
#                                    as.list(
#                                      match(
#                                        c(unlist(
#                                          names(dimnames(jpts[[index]]))
#                                        )),
#                                        c(variables)
#                                      )
#                                    )
#             )
#             
#             
#             for (i in 1:num.nodes)
#             {
#               target.clique <- which(sapply(1:num.cliqs,
#                                             function(index){
#                                               is.element(
#                                                 i,
#                                                 unlist(dim.vars[[index]])
#                                               )
#                                             }
#               ) == TRUE)[1]
#               pot  <- jpts[[target.clique]]
#               vars <- c(unlist(dim.vars[[target.clique]]))
#               
#               for (v in c(unlist(setdiff(vars,i))))
#               {
#                 out  <- marginalize(pot, vars, v)
#                 pot  <- out$potential
#                 vars <- out$vars
#                 pot  <- pot / sum(pot)
#               }
#               
#               mpv[i] <- sample(1:node.sizes[i], 1, replace=T, prob=pot) #,replace=TRUE
#             }
#             
#             return(mpv)
#           })


#' @rdname sample.dataset
#' @aliases sample.dataset,InferenceEngine
setMethod("sample.dataset",c("InferenceEngine"),
          function(x, n = 100)
          {
            if(test.updated.bn(x))
              net <- updated.bn(x)
            else
              net <- bn(x)
            
            return(sample.dataset(net, n))
          })


#' @rdname marginals
#' @aliases marginals,InferenceEngine
setMethod("marginals",
          "InferenceEngine",
          function(x, ...)
          {
            #             if (is.null(jpts(x))) # don't know if this works...
            #               return(get.most.probable.values(bn(x)))
            jpts      <- jpts(x)
            num.nodes <- num.nodes(bn(x))
            cliques   <- jt.cliques(x)
            num.cliqs <- length(cliques)
            variables <- variables(bn(x))
            
            mpv <- NULL
            
            dim.vars   <- lapply(1:num.cliqs,
                                 function(index)
                                   as.list(
                                     match(
                                       unlist(
                                         names(dimnames(jpts[[index]])),F, F
                                       ),
                                       c(variables)
                                     )
                                   )
            )
            
            for (i in 1:num.nodes)
            {
              target.clique <- which(sapply(1:num.cliqs,
                                            function(index){
                                              is.element(
                                                i,
                                                unlist(dim.vars[[index]], F, F)
                                              )
                                            }
              ) == TRUE)[1]
              pot  <- jpts[[target.clique]]
              vars <- unlist(dim.vars[[target.clique]])
              
              for (v in c(unlist(setdiff(vars,i))))
              {
                out  <- marginalize(pot, vars, v)
                pot  <- out$potential
                vars <- out$vars
                pot  <- pot / sum(pot)
              }
              
              mpv[[i]] <- pot
            }
            
            names(mpv) <- as.list(variables)
            return(mpv)
          })


# redefition of print() for InferenceEngine objects
# ' print a \code{\link{InferenceEngine}} to \code{stdout}.
#'
#' @method print InferenceEngine
#' @name print
#' 
# ' @param x an \code{\link{InferenceEngine}}.
#' @param engine if \code{x} is an \code{\link{InferenceEngine}}, specify the inference engine to be shown. Currently only \code{engine = 'jt'} is supported.
#' @param ... potential other arguments.
#'
#' @rdname print
#' @aliases print,InferenceEngine print.InferenceEngine,InferenceEngine
#' @export
#setMethod("print.InferenceEngine",
#          "InferenceEngine",
print.InferenceEngine <- function(x, engine = "jt", ...)
          {
            if (engine == 'jt')
            {
              str <- "\nJunction Tree "
              str <- paste(str, "with ", sep = '')
              str <- paste(str, num.nodes(x), sep = '')
              str <- paste(str, " cliques", sep = '')
              cat(str,"\n")
              
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
                
                cat(str, '\n\nAdjacency matrix:\n')
                print(junction.tree(x))
              }
            }
            
          }#)

# keep last (most recent) observation for each var
unique.observations <- function(observed.vars, observed.vals)
{
  ovrs <- unlist(observed.vars)
  ovls <- unlist(observed.vals)
  dup  <- which(duplicated(rev(ovrs)) == TRUE)
  if (length(dup) > 0)
  {
    ovrs <- rev(rev(ovrs)[-dup])
    ovls <- rev(rev(ovls)[-dup])
  }
  return(list("observed.vars"=ovrs, "observed.vals"=ovls))
}

