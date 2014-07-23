#' @rdname read.dataset-methods
#' @aliases read.dataset
setMethod("read.dataset",
          c("BNDataset", "character", "character"),
          function(object, header, dataset, imputation = FALSE, header.flag = FALSE,
                   na.string.symbol = '?', sep.symbol = '', k.impute = 10,
                   bootstrap = FALSE, num.boots = 100, seed = 0)
          {
            ls                  <- readLines(header)
            object@variables    <- c(unlist(strsplit(ls[1], split = " ")))
            lns                 <- c(unlist(strsplit(ls[2], split = " ")))
            object@node.sizes   <- sapply(1:length(lns), FUN=function(x){ as.numeric(lns[x]) })
            lds                 <- c(unlist(strsplit(ls[3], split = " ")))
            object@discreteness <- sapply(1:length(lds), FUN=function(x){ !is.na(match(lds[x],c('d',"D"))) })
            
            a <- read.delim(dataset, na.strings = na.string.symbol,
                            header = header.flag, sep = sep.symbol) + 1
            object@raw.data      <- as.matrix(a)
            object@num.variables <- ncol(object@raw.data)
            object@num.items     <- nrow(object@raw.data)
            object@has.rawdata   <- TRUE
            
            if (object@num.variables != length(object@variables))
            {
              message(paste(c("Incoherent number of variables between files ",
                              header," and ",dataset,"\n"), sep=''))
              quit(status = 1)
            }
            
            # quantize if needed
            if (!is.na(match(FALSE,c(unlist(object@discreteness)))))
            {
              object@raw.data <- quantize.matrix(object@raw.data, object@node.sizes)
            }
            
            object@imputation    <- imputation
            object@has.impdata   <- imputation
            
            if (imputation)
            {
              # perform imputation
              object@imputed.data <- knn.impute(object@raw.data, k.impute,
                                                setdiff(1:length(object@node.sizes), c()))
            }
            
            object@has.boots <- bootstrap
            
            set.seed(seed)
            if (bootstrap && num.boots >= 1)
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
            
            validObject(object)
            object
          })