#' Constructor method of \code{\link{BNDataset}} class.
#'
#' @name BNDataset
#' @rdname BNDataset-class
setMethod("initialize",
          "BNDataset", function(.Object, ...)  
          {
            validObject(.Object)      
            .Object
          })

#' Wrapper for \code{\link{BNDataset}} object
#' 
#' @name BNDataset
#' @rdname BNDataset-class
#' @export 
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
              if(object@num.variables > 0 && length(object@discreteness) > 1 &&
                   length(object@discreteness) != object@num.variables)
              {
                retval <- c(retval, "incoherent number of variable statuses")
              }
              
              if (is.null(retval)) return (TRUE)
              return (retval)
            }
)

#' @rdname accessors-methods
#' @aliases name
setMethod("name", "BNDataset", function(x) { slot(x, "name") } )


#' @rdname accessors-methods
#' @aliases num.variables
setMethod("num.variables", "BNDataset", function(x) { slot(x, "num.variables") } )


#' @rdname accessors-methods
#' @aliases variables
setMethod("variables", "BNDataset", function(x) { slot(x, "variables") } )


#' @rdname accessors-methods
#' @aliases discreteness
setMethod("discreteness",
          "BNDataset",
          function(x)
          {
            vs  <- slot(x, "discreteness")
            nvs <- rep('c', length(vs))
            nvs[which(vs == TRUE)] <- 'd'
            nvs
          })


#' @rdname accessors-methods
#' @aliases node.sizes
setMethod("node.sizes", "BNDataset", function(x) { slot(x, "node.sizes") } )


#' @aliases header.file
#' @rdname accessors-methods
setMethod("header.file", "BNDataset", function(x) slot(x, "header.file"))


#' @aliases data.file
#' @rdname accessors-methods
setMethod("data.file", "BNDataset", function(x) slot(x, "data.file"))


#' @aliases num.variables
#' @rdname accessors-methods
setMethod("num.variables","BNDataset", function(x) slot(x, "num.variables"))


#' @name num.items
#' @rdname accessors-methods
setMethod("num.items", "BNDataset", function(x) slot(x, "num.items"))


#' @aliases has.boots
#' @rdname accessors-methods
setMethod("has.boots", "BNDataset", function(x) slot(x, "has.boots"))


#' @aliases has.imp.boots
#' @rdname accessors-methods
setMethod("has.imp.boots", "BNDataset", function(x) slot(x, "has.imp.boots"))


#' @aliases boots
#' @rdname accessors-methods
setMethod("boots", "BNDataset", function(x) slot(x, "boots"))


#' @aliases imp.boots
#' @rdname accessors-methods
setMethod("imp.boots", "BNDataset", function(x) slot(x, "imp.boots"))


#' @aliases num.boots
#' @rdname accessors-methods
setMethod("num.boots", "BNDataset", function(x) slot(x, "num.boots"))


# @name name
# @rdname mutators-methods
# @aliases name
setReplaceMethod("name",
                 signature(x="BNDataset", value="character"),
                 function(x, value)
                 {
                   slot(x, "name") <- value
                   validObject(x)
                   x
                 })


# @name variables
# @rdname mutators-methods
# @aliases variables
setReplaceMethod("variables",
                 "BNDataset",
                 function(x, value)
                 {
                   slot(x, "variables")  <- value
                   num.variables(x)      <- length(value)
                   validObject(x)
                   x
                 })


# @name discreteness
# @rdname mutators-methods
# @aliases discreteness
setReplaceMethod("discreteness",
                 "BNDataset",
                 function(x, value)
                 {
                   slot(x, "discreteness") <- sapply(1:length(value), FUN=function(i){ !is.na(match(value[i],c('d',"D"))) })
                   validObject(x)
                   x
                 })


# @name node.sizes
# @aliases node.sizes
# @rdname mutators-methods
setReplaceMethod("node.sizes",
                 "BNDataset",
                 function(x, value)
                 {
                   slot(x, "node.sizes") <- value
                   validObject(x)
                   x
                 })


# check if any data available
#' @rdname has.data-methods
#' @aliases has.data
setMethod("has.data",
          "BNDataset",
          function(object)
          {
            return (has.raw.data(object) || object@has.imputed.data(object))
          })

#' @rdname has.raw.data-methods
#' @aliases has.raw.data
setMethod("has.raw.data",
          "BNDataset",
          function(object)
          {
            object@has.rawdata
          })

#' @rdname has.imputed.data-methods
#' @aliases has.imputed.data
setMethod("has.imputed.data",
          "BNDataset",
          function(object)
          {
            object@has.impdata
          })

# get() method for data
# imputed data, if any, is preferred over raw data because more complete
#' @rdname get.data-methods
#' @aliases get.data
setMethod("get.data",
          "BNDataset",
          function(object)
          {
            if (has.imputed.data(object) == FALSE)
              return (get.raw.data(object))
            return (get.imputed.data(object))
          })

#' @rdname get.raw.data-methods
#' @aliases get.raw.data
setMethod("get.raw.data",
          "BNDataset",
          function(object)
          {
            if (has.raw.data(object))
              return (object@raw.data)
            return (NULL)
          })

#' @rdname get.imputed.data-methods
#' @aliases get.imputed.data
setMethod("get.imputed.data",
          "BNDataset",
          function(object)
          {
            if (has.imputed.data(object))
              return (object@imputed.data)
            return (NULL)
          })


# @aliases header.file
# @rdname mutators-methods
setReplaceMethod("header.file",
                 "BNDataset",
                 function(x, value)
                  {
                    slot(x, "header.file") <- value
                    x
                  })


# @aliases data.file
# @rdname mutators-methods
setReplaceMethod("data.file",
                 "BNDataset",
                 function(x, value)
                 {
                   slot(x, "data.file") <- value
                   x
                 })


# @aliases num.variables
# @rdname mutators-methods
setReplaceMethod("num.variables",
                 "BNDataset",
                 function(x, value)
                  {
                    slot(x, "num.variables") <- value
                    validObject
                    x
                  })


# @aliases num.items
# @rdname mutators-methods
setReplaceMethod("num.items",
                 "BNDataset",
                 function(x, value)
                 {
                  slot(x, "num.items") <- value
                  validObject(x)
                  x
                 })


# @aliases boots
# @rdname mutators-methods
setReplaceMethod("boots",
                 "BNDataset",
                 function(x, value)
                 {
                  slot(x, "boots")     <- value
                  slot(x, "num.boots") <- length(value)
                  slot(x, "has.boots") <- TRUE
                  validObject(x)
                  x
                 })


# @aliases imp.boots
# @rdname mutators-methods
setReplaceMethod("imp.boots",
                 "BNDataset",
                 function(x, value)
                 {
                   slot(x, "imp.boots")     <- value
                   slot(x, "num.boots")     <- length(value)
                   slot(x, "has.imp.boots") <- TRUE
                   validObject(x)
                   x
                 })


setReplaceMethod("raw.data",
                 "BNDataset",
                 function(object, value)
                 {
                   slot(object, "raw.data")    <- value
                   slot(object, "has.rawdata") <- TRUE
                   validObject(object)
                   object
                 })


setReplaceMethod("imputed.data",
                 "BNDataset",
                 function(object, value)
                 {
                   slot(object, "imputed.data") <- value
                   slot(object, "has.impdata")  <- TRUE
                   slot(object, "imputation")   <- TRUE
                   validObject(object)
                   object
                 })


# redefition of print() for BNDataset objects
#' @rdname print-methods
#' @aliases print.BNDataset,BNDataset,ANY
setMethod("print.BNDataset",
          "BNDataset",
          function(x, show.raw.data = FALSE, show.imputed.data = FALSE, ...)
          {
            object <- x
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
            cat(str)
            
            if (show.raw.data == TRUE && has.raw.data(object))
            {
              cat("Raw data:\n")
              print(get.raw.data(object))
            }
            if (show.imputed.data == TRUE && has.imputed.data(object))
            {
              cat("Imputed data:\n")
              print(get.imputed.data(object))
            }
            
          })

#' @rdname impute-methods
#' @aliases impute
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

#' @rdname bootstrap-methods
#' @aliases bootstrap
setMethod("bootstrap",
          "BNDataset",
          function(object, num.boots = 100, seed = 0, imputation = FALSE, k.impute = 10, na.string.symbol = '?')
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


#' @rdname get.boot-methods
#' @aliases get.boot
setMethod("get.boot",
          c("BNDataset", "numeric"),
          function(dataset, index, imputed = TRUE)
          {
            if (!(dataset@has.boots || dataset@has.imp.boots))
            {
              message('WARNING: No bootstrap samples available for dataset.\n')
              return(NULL)
            }
            
            if (index <= 0 || index > dataset@num.boots)
            {
              message('WARNING: index out of range for dataset.\n')
              return(NULL)
            }
            
            if (imputed && dataset@has.imp.boots)
              return(dataset@imp.boots[[index]])
            
            if (dataset@has.boots)
              return(dataset@boots[[index]])
            
            # if !has.boots && !imputed && has.imp.boots - though I don't know if this will ever happen
            return(NULL)
          })
