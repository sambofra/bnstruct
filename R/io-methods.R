#' @rdname read.dataset
#' @aliases read.dataset,BNDataset,character,character
setMethod("read.dataset",
          c("BNDataset", "character", "character"),
          function(object, header.file, data.file, imputation = FALSE, header.flag = FALSE,
                   na.string.symbol = '?', sep.symbol = '', k.impute = 10,
                   bootstrap = FALSE, num.boots = 100, seed = 0, ...)
          {
            header.file(object)  <- header.file
            data.file(object)    <- data.file
            
            ls                   <- readLines(header)
            variables(object)    <- gsub('"', '', c(unlist(strsplit(ls[1], split = " "))))
            lns                  <- c(unlist(strsplit(ls[2], split = " ")))
            node.sizes(object)   <- sapply(1:length(lns), FUN=function(x){ as.numeric(lns[x]) })
            discreteness(object) <- c(unlist(strsplit(ls[3], split = " ")))
            
            a <- read.delim(dataset, na.strings = na.string.symbol,
                            header = header.flag, sep = sep.symbol) + 1
            raw.data(object)      <- as.matrix(a)
            num.variables(object) <- ncol(object@raw.data)
            num.items(object)     <- nrow(object@raw.data)
            
            if (num.variables(object) != length(variables(object)))
            {
              message(paste(c("Incoherent number of variables between files ",
                              header," and ",dataset,"\n"), sep=''))
              quit(status = 1)
            }
            
            # quantize if needed
            if (!is.na(match(FALSE,c(unlist(discreteness(object))))))
            {
              raw.data(object) <- quantize.matrix(get.raw.data(object), node.sizes(object))
            }
            
            if (imputation)
            {
              # perform imputation
              imputed.data(object) <- knn.impute(get.raw.data(object), k.impute,
                                                setdiff(1:length(node.sizes(object)), c()))
            }
            
            set.seed(seed)
            if (bootstrap && num.boots >= 1)
            {
              object@num.boots <- num.boots
              object@has.boots <- TRUE
              print(object@has.boots)
              print(object@num.boots)
              boot.sample <- matrix(sample.int(num.items(object),
                                               size = num.boots * num.items(object),
                                               replace=TRUE),
                                    num.items(object), num.boots)
              
              if (imputation)
                object@has.imp.boots <- TRUE
              
              for (i in 1:num.boots)
              {
                object@boots[[i]] <- object@raw.data[boot.sample[,i],]
                
                if (imputation)
                  object@imp.boots[[i]] <- knn.impute(object@boots[[i]],
                                                      k.impute,
                                                      setdiff(1:length(node.sizes(object)),c()) )
                
              }
            }
            
            validObject(object)
            return(object)
          })

#' load \code{Asia} dataset.
#' 
#' Wrapper for a loader for the \code{Asia} dataset, with only raw data.
#' 
#' The dataset has 10000 items, no missing data, so no imputation needs to be performed.
#' 
#' @name asia
#' @rdname asia
#' 
#' @return a BNDataset containing the \code{Child} dataset.
#' 
#' @examples
#' dataset <- asia()
#' print(dataset)
#' 
#' @seealso \code{\link{asia_10000}}
#' 
#' @export
asia <- function()
{
  data("asia_10000", envir=environment())
  return(get("asia_10000", envir=environment()))
}


#' load \code{Child} dataset.
#' 
#' Wrapper for a loader for the \code{Child} raw dataset; also perform imputation.
#' 
#' The dataset has 5000 items, with random missing values (no latent variables). BNDataset object contains the raw dataset and imputed dataset, with \code{k=10}
#' (see \code{\link{impute}} for related explanation).
#' 
#' @name child
#' @rdname child
#' 
#' @return a BNDataset containing the \code{Child} dataset.
#' 
#' @examples dataset <- child()
#' print(dataset)
#' 
#' @seealso \code{\link{child_NA_5000}}
#' 
#' @export
child <- function()
{
  data("child_NA_5000", envir=environment())
  return(get("child_NA_5000", envir=environment()))
}
