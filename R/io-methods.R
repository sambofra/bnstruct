#' @rdname read.dataset
#' @aliases read.dataset,BNDataset,character,character
setMethod("read.dataset",
          c("BNDataset", "character", "character"),
          function(object, header, dataset, imputation = FALSE, header.flag = FALSE,
                   na.string.symbol = '?', sep.symbol = '', k.impute = 10,
                   bootstrap = FALSE, num.boots = 100, seed = 0, ...)
          {
            ls                   <- readLines(header)
            variables(object)    <- gsub('"', '', c(unlist(strsplit(ls[1], split = " "))))
            lns                  <- c(unlist(strsplit(ls[2], split = " ")))
            node.sizes(object)   <- sapply(1:length(lns), FUN=function(x){ as.numeric(lns[x]) })
            lds                  <- c(unlist(strsplit(ls[3], split = " ")))
            discreteness(object) <- sapply(1:length(lds), FUN=function(x){ !is.na(match(lds[x],c('d',"D"))) })
            
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
            object
          })
