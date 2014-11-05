#' @name BNParams
#' @rdname BNParams-class
#' @aliases initialize,BNParams-method
#' 
#' @param .Object a BNParams
#' 
setMethod("initialize",
          "BN",
          function(.Object, ...)
          {
            x <- .Object
            validObject(x)
            return(x)
          })


#' constructor for a \code{\link{BNparams}} object.
#' 
#' Takes the list of command line parameters passed in input, and processes it.
#' Does not check for their correctness (it's assumed they are correct).
#' 
#' @name BNParams
#' @rdname BNParams-class
#' 
#' @param args the list of command line parameters passed to the R script.
#' @param ... potential further arguments of methods.
#' 
#' @return BNParams object.
#' 
#' @examples
#' \dontrun{
#' params <- BNParams()
#' }
#' 
#' @export
BNParams <- function(args, ...)
{
  object <- new("BNParams", ...)
  
  i        <- 1
  num.pars <- length(args)
  while (i <= num.pars)
  {
    if (args[i] == "--ess")
    {
      object@ess <- as.numeric(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--alpha")
    {
      object@alpha <- as.numeric(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--learning.algo")
    {
      object@learning.algo <- args[i+1]
      i <- i + 2
    }
    else if (args[i] == "--scoring.func")
    {
      object@scoring.func <- args[i+1]
      i <- i + 2
    }
    else if (args[i] == "--num.boots")
    {
      object@num.boots <- as.numeric(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--k.impute")
    {
      object@k.impute <- as.numeric(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--seed")
    {
      object@seed <- as.integer(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--em_convergence")
    {
      object@em_convergence <- as.numeric(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--sem_convergence")
    {
      object@sem_convergence <- as.numeric(args[i+1])
      i <- i + 2
    }
    
    else if (args[i] == "--CPX_PARAM_PRELINEAR")
    {
      object@CPX_PARAM_PRELINEAR <- as.integer(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--CPX_PARAM_SCRIND")
    {
      object@CPX_PARAM_SCRIND <- as.integer(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--CPX_PARAM_PREIND")
    {
      object@CPX_PARAM_PREIND <- as.integer(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--CPX_PARAM_MIPCBREDLP")
    {
      object@CPX_PARAM_MIPCBREDLP <- as.integer(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--CPX_PARAM_MIPSEARCH")
    {
      object@CPX_PARAM_MIPSEARCH <- as.integer(args[i+1])
      i <- i + 2
    }
    else if (args[i] == "--cplex_presolve_algo")
    {
      object@cplex_presolve_algo <- as.integer(args[i+1])
      i <- i + 2
    }
  }

  validObject(object)
  
  return(object)
}

# validator
setValidity("BNParams",
            function(object)
            {
              retval <- NULL
              # do things
              if (is.null(retval)) return (TRUE)
              return(retval)
            }
)
