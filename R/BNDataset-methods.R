#' Constructor for \link{BNDataset} object
#' 
#' @name BNDataset
#' @rdname BNDataset-class
#' @export BNDataset
BNDataset <- function(...)
{
  dataset <- new("BNDataset", ...)
  
#  WARNING: HOW DO I MANAGE THIS?
#   # just to be sure
#   if ((length(dataset@raw.data) == 1) && (dataset@raw.data == matrix(c(0))))
#     dataset@has.rawdata <- FALSE
#   else
#   {
#     dataset@has.rawdata <- TRUE
    if(length(dataset@variables) > 0 && has.raw.data(dataset))
      colnames(dataset@raw.data) <- dataset@variables
#   }
#   
#   if ((length(dataset@imputed.data) == 1) && (dataset@imputed.data == matrix(c(0))))
#   {
#     dataset@has.impdata <- FALSE
#     dataset@imputation  <- FALSE
#   }
#   else
#   {
#     dataset@has.impdata <- TRUE
#     dataset@imputation  <- TRUE
    if(length(dataset@variables) > 0 && has.imputed.data(dataset))
      colnames(dataset@imputed.data) <- dataset@variables
#   }
    
  dataset
}

# validator
setValidity("BNDataset",
            function(object)
            {
              retval <- NULL
              if (object@num.variables > 0 && length(object@variables) > 0 && length(object@variables) != object@num.variables)
              {
                retval <- c(retval, "incoherent number of variable names")
              }
              if (object@has.rawdata && ncol(object@raw.data) != object@num.variables)
              {
                retval <- c(retval, "incoherent number of variables in raw dataset")
              }
              if (object@has.impdata && ncol(object@imputed.data) != object@num.variables)
              {
                retval <- c(retval, "incoherent number of variables in imputed dataset")
              }
              
              if (is.null(retval)) return (TRUE)
              return (retval)
            }
)

# check if any data available
setMethod("has.data",
          "BNDataset",
          function(object)
          {
            return (has.raw.data(object) || object@has.imputed.data(object))
          })

setMethod("has.raw.data",
          "BNDataset",
          function(object)
          {
            object@has.rawdata
          })

setMethod("has.imputed.data",
          "BNDataset",
          function(object)
          {
            object@has.impdata
          })

# get() method for data
# imputed data, if any, is preferred over raw data because more complete
setMethod("get.data",
          "BNDataset",
          function(object)
          {
            if (has.imputed.data(object) == FALSE)
              return (get.raw.data(object))
            return (get.imputed.data(object))
          })

setMethod("get.raw.data",
          "BNDataset",
          function(object)
          {
            if (has.raw.data(object))
              return (object@raw.data)
            return (NULL)
          })

setMethod("get.imputed.data",
          "BNDataset",
          function(object)
          {
            if (has.imputed.data(object))
              return (object@imputed.data)
            return (NULL)
          })

# redefition of print() for BNDataset objects
setMethod("print.BNDataset",
          "BNDataset",
          function(object, show.raw.data = FALSE, show.imputed.data = FALSE, ...)
          {
            str <- "\nDataset"
            
            if (object@name != "")
              str <- paste(str, object@name)
            
            if (object@data.file != "")
            {
              str <- paste(str, "from file")
              str <- paste(str, object@data.file)
            }
            
            str <- paste(str, '\n')
            
            str <- paste(str, "with ", sep = '')
            str <- paste(str, object@num.variables, sep = '')
            str <- paste(str, "variables :")
            #print(paste(c(object@variables), sep=', '))
            str <- paste(str, paste(object@variables, sep=" ", collapse=', '))
            
            if (has.data(object))
            {
              str <- paste(str, "\nand ", sep='')
              str <- paste(str, object@num.items, sep='')
              str <- paste(str, " items", sep='')
            }
            else
            {
              str <- paste(str, "currently empty", sep='')
            }
            str <- paste(str, ".\n", sep = '')
            message(str)
            
            if (show.raw.data == TRUE && has.raw.data(object))
            {
              message("Raw data:\n")
              print(get.raw.data(object))
            }
            if (show.imputed.data == TRUE && has.imputed.data(object))
            {
              message("Imputed data:\n")
              print(get.imputed.data(object))
            }
            
          })

setMethod("impute",
          "BNDataset",
          function(object, k.impute = 10)
          {
            # assumes raw data is ok
            object@imputed.data <- knn.impute(object@raw.data, k.impute,
                                              setdiff(1:length(object@node.sizes), c()))
            object@has.impdata  <- TRUE
            object
          })

setMethod("bootstrap",
          "BNDataset",
          function(object, num.boots = 100, seed = 0, imputation = FALSE, k.impute = 10)
          {
            # assumes raw data is ok
            object@has.boots <- TRUE
            object@num.boots <- num.boots
            
            set.seed(seed)
            if (num.boots >= 1)
            {
              boot.sample <- matrix(sample.int(object@num.items,
                                               size = num.boots * object@num.items,
                                               replace=TRUE),
                                    object@num.items, num.boots)
              
              if (imputation)
                object@has.imp.boots <- TRUE
              
              for (i in 1:num.boots)
              {
                object@boots[[i]] <- object@raw.data[boot.sample[,i],]
                
                if (imputation)
                  object@imp.boots[[i]] <- knn.impute(object@boots[[i]],
                                                      k.impute,
                                                      setdiff(1:length(object@node.sizes),c()) )
                
              }
            }
            object
          })