#' @rdname read.dataset
#' @aliases read.dataset,BNDataset,character,character
setMethod("read.dataset",
          c("BNDataset", "character", "character"),
          function(object, header.file, data.file, imputation = FALSE, header.flag = FALSE,
                   na.string.symbol = '?', sep.symbol = '', k.impute = 10,
                   bootstrap = FALSE, num.boots = 100, seed = 0, starts.from = 1, ...)
          {
            header.file(object)  <- header.file
            data.file(object)    <- data.file
            
            ls                   <- readLines(header.file)
            variables(object)    <- gsub('"', '', c(unlist(strsplit(ls[1], split = " "))))
            lns                  <- c(unlist(strsplit(ls[2], split = " ")))
            node.sizes(object)   <- sapply(1:length(lns), FUN=function(x){ as.numeric(lns[x]) })
            discreteness(object) <- c(unlist(strsplit(ls[3], split = " ")))
            
            a <- read.delim(data.file, na.strings = na.string.symbol,
                            header = header.flag, sep = sep.symbol) + (1 - starts.from)
            raw.data(object)      <- as.matrix(a)
            num.variables(object) <- ncol(object@raw.data)
            num.items(object)     <- nrow(object@raw.data)
            
            if (num.variables(object) != length(variables(object)))
            {
              message(paste(c("Incoherent number of variables in dataset\n"), sep=''))
              quit(status = 1)
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

#' Show method for objects.
#'
#' The \code{show} method allows to provide a custom aspect for the output that is generated
#' when the name of an instance is gives as command in an R session.
#'
#' @name show
#' @rdname show
#' @aliases show show,AllTheClasses-method show,BN-method show,BNDataset-method show,InferenceEngine-method
#' @docType methods
#' 
#' @param object an object.
#' 
#' @usage show(object)
#' 
#' @export
setMethod("show", "AllTheClasses", function(object) print(object))


#' @rdname read.dsc
#' @aliases read.dsc,character
setMethod("read.dsc", c("character"),
          function(x)
          {
            file.name <- x
            all.file  <- readChar(file.name, file.info(file.name)$size)
            all.file  <- gsub('\n', ' ', all.file)
            all.file  <- gsub('"', '', all.file)
            
            lines        <- unlist(strsplit(all.file, "node|probability"))
            num.nodes    <- (length(lines) - 1) / 2
            network.name <- lines[1]
            network.name <- gsub(" ", "", unlist(strsplit(network.name, "belief network"), F, F)[2])
            lines        <- lines[-1]
            
            variables    <- rep("", num.nodes)
            discreteness <- rep(TRUE, num.nodes)
            node.sizes   <- rep(0, num.nodes)
            
            net <- BN()
            name(net)      <- network.name
            num.nodes(net) <- num.nodes
            
            nodes <- sapply(1:num.nodes, function(x) gsub(" ", "", lines[x]))
            probs <- sapply((num.nodes+1):(2*num.nodes), function(x) gsub(" ", "", lines[x]))
            
            for (i in 1:num.nodes)
            {
              tmp <- unlist(strsplit(nodes[i], "\\{|\\}"))
              variables[i] <- tmp[1]
              tmp2 <- unlist(strsplit(tmp[2], "\\:|\\[|\\]"))
              if (tolower(tmp2[2]) == "continuous") discreteness[i] <- FALSE
              node.sizes[i] <- as.numeric(tmp2[3])
            }
            
            variables(net)    <- variables
            discreteness(net) <- discreteness
            node.sizes(net)   <- node.sizes

            dag <- ""
            prob.list <- rep("", num.nodes)
            
            for (i in 1:num.nodes)
            {
              tmp    <- unlist(strsplit(probs[i], "\\{|\\}"))
              to.num <- unlist(strsplit(tmp[1], "\\(|\\||\\)|\\,"))[-1]
              to.num <- sapply(to.num, function(x) which(variables==x))
              
              local.node <- "("
              local.node <- paste(local.node, to.num[1], sep="")
              if (length(to.num) > 1)
              {
                local.node <- paste(local.node, "|", sep="")
                for(j in 2:length(to.num))
                {
                  local.node <- paste(local.node, to.num[j], sep="")
                  if (j < length(to.num))
                    local.node <- paste(local.node, ",", sep="")
                }
              }
              local.node <- paste(local.node, ")", sep="")
              
              dag <- paste(dag, local.node, sep="")
              prob.list[i] <- tmp[2]
            }
            
            dag(net) <- factors.to.graph(dag)
            
            cpts <- NULL
            
            for (i in 1:num.nodes)
            {
              family <- c(which(dag(net)[,i]!=0), i)
              if (length(family) > 1)
              {
                ps <- unlist(strsplit(prob.list[i], "\\(\\*\\)\\:|\\,|\\;|\\:"))
              }
              else
              {
                ps <- unlist(strsplit(prob.list[i], "\\,|\\;"))
              }
              ps <- ps[which(ps != "")]
              suppressWarnings(ps <- ps[which(!is.na(as.numeric(ps)))])
              ps <- array(as.numeric(ps), dim=node.sizes[family])
              cpts[[i]] <- ps
              
              dms <- NULL
              dns <- NULL
              for (j in 1:length(family))
              {
                dms[[j]] <- as.list(c(1:node.sizes[family[j]]))
                dns[[j]] <- c(variables[family[j]])
              }
              
              dimnames(cpts[[i]])          <- dms
              names( dimnames(cpts[[i]]) ) <- dns
            }
            names(cpts) <- variables
            
            cpts(net) <- cpts
            
            return(net)  
          })


#' @rdname read.bif
#' @aliases read.bif,character
setMethod("read.bif", c("character"),
          function(x)
          {
            file.name <- x
            all.file  <- readChar(file.name, file.info(file.name)$size)
            all.file  <- gsub('\n', ' ', all.file)
            all.file  <- gsub('"', '', all.file)
            
            lines        <- unlist(strsplit(all.file, "variable|probability"))

            num.nodes    <- (length(lines) - 1) / 2
            network.name <- lines[1]
            network.name <- gsub(" ", "", unlist(strsplit(network.name, "network"), F, F)[2])
            network.name <- gsub("\\{", "", network.name)
            network.name <- gsub("\\}", "", network.name)
            lines        <- lines[-1]
            
            variables    <- rep("", num.nodes)
            discreteness <- rep(TRUE, num.nodes)
            node.sizes   <- rep(0, num.nodes)
            
            net <- BN()
            name(net)      <- network.name
            num.nodes(net) <- num.nodes
            
            nodes <- lines[1:num.nodes]
            probs <- lines[(num.nodes+1):(2*num.nodes)]
            
            for (i in 1:num.nodes)
            {
              tmp <- unlist(strsplit(nodes[i], "\\{|\\}|type|\\[|\\]|\\;"))
              tmp <- gsub(" ", "", tmp)
              tmp <- tmp[-which(tmp == "")]
              
              variables[i] <- tmp[1]
              if (tolower(tmp[2]) == "continuous") discreteness[i] <- FALSE
              node.sizes[i] <- as.numeric(tmp[3])
            }
            
            variables(net)    <- variables
            discreteness(net) <- discreteness
            node.sizes(net)   <- node.sizes
            
            dag <- ""
            prob.list <- rep("", num.nodes)
            
            for (i in 1:num.nodes)
            {
              tmp    <- gsub(" ", "", unlist(strsplit(probs[i], "\\{|\\}")))
              to.num <- unlist(strsplit(tmp[1], "\\(|\\||\\)|\\,"))[-1]
              to.num <- sapply(to.num, function(x) which(variables==x))
              
              local.node <- "("
              local.node <- paste(local.node, to.num[1], sep="")
              if (length(to.num) > 1)
              {
                local.node <- paste(local.node, "|", sep="")
                for(j in 2:length(to.num))
                {
                  local.node <- paste(local.node, to.num[j], sep="")
                  if (j < length(to.num))
                    local.node <- paste(local.node, ",", sep="")
                }
              }
              local.node <- paste(local.node, ")", sep="")
              
              dag <- paste(dag, local.node, sep="")
              prob.list[i] <- tmp[2]
            }
            
            dag(net) <- factors.to.graph(dag)
            
            cpts <- NULL
            
            for (i in 1:num.nodes)
            {
              family <- c(rev(which(dag(net)[,i]!=0)), i)
              if (length(family) > 1)
              {
                ps <- unlist(strsplit(prob.list[i], "\\(|\\)|\\,|\\;|\\:"))
              }
              else
              {
                ps <- gsub("table","", prob.list[i])
                ps <- unlist(strsplit(ps, "\\,|\\;"))
              }
              ps <- ps[which(ps != "")]
              suppressWarnings(ps <- ps[which(!is.na(as.numeric(ps)))])
              ps <- array(as.numeric(ps), dim=c(node.sizes[family]))
              cpts[[i]] <- ps
              
              dms <- NULL
              dns <- NULL
              for (j in 1:length(family))
              {
                dms[[j]] <- as.list(c(1:node.sizes[family[j]]))
                dns[[j]] <- c(variables[family[j]])
              }
              
              dimnames(cpts[[i]])          <- dms
              names( dimnames(cpts[[i]]) ) <- dns
            }
            names(cpts) <- variables
            
            cpts(net) <- cpts
            
            return(net)  
          })


#' @rdname read.net
#' @aliases read.net,character
setMethod("read.net", c("character"),
          function(x)
          {
            file.name <- x
            all.file  <- readChar(file.name, file.info(file.name)$size)
            all.file  <- gsub('\n', ' ', all.file)
            all.file  <- gsub('"', '', all.file)
            
            lines        <- unlist(strsplit(all.file, "node|potential"))
            
            num.nodes    <- (length(lines) - 1) / 2
            network.name <- lines[1]
            network.name <- gsub(" ", "", unlist(strsplit(network.name, "network"), F, F)[2])
            network.name <- gsub("\\{", "", network.name)
            network.name <- gsub("\\}", "", network.name)
            lines        <- lines[-1]
            
            variables    <- rep("", num.nodes)
            discreteness <- rep(TRUE, num.nodes)
            node.sizes   <- rep(0, num.nodes)
            
            net <- BN()
            name(net)      <- network.name
            num.nodes(net) <- num.nodes
            
            nodes <- lines[1:num.nodes]
            probs <- lines[(num.nodes+1):(2*num.nodes)]
            
            for (i in 1:num.nodes)
            {
              tmp <- unlist(strsplit(nodes[i], "\\{|\\}|type|\\[|\\]|\\;"))
              tmp <- gsub(" ", "", tmp)
              tmp <- tmp[-which(tmp == "")]
              
              variables[i] <- tmp[1]
              if (tolower(tmp[2]) == "continuous") discreteness[i] <- FALSE
              node.sizes[i] <- as.numeric(tmp[3])
            }
            
            variables(net)    <- variables
            discreteness(net) <- discreteness
            node.sizes(net)   <- node.sizes
            
            dag <- ""
            prob.list <- rep("", num.nodes)
            
            for (i in 1:num.nodes)
            {
              tmp    <- gsub(" ", "", unlist(strsplit(probs[i], "\\{|\\}")))
              to.num <- unlist(strsplit(tmp[1], "\\(|\\||\\)|\\,"))[-1]
              to.num <- sapply(to.num, function(x) which(variables==x))
              
              local.node <- "("
              local.node <- paste(local.node, to.num[1], sep="")
              if (length(to.num) > 1)
              {
                local.node <- paste(local.node, "|", sep="")
                for(j in 2:length(to.num))
                {
                  local.node <- paste(local.node, to.num[j], sep="")
                  if (j < length(to.num))
                    local.node <- paste(local.node, ",", sep="")
                }
              }
              local.node <- paste(local.node, ")", sep="")
              
              dag <- paste(dag, local.node, sep="")
              prob.list[i] <- tmp[2]
            }
            
            dag(net) <- factors.to.graph(dag)
            
            cpts <- NULL
            
            for (i in 1:num.nodes)
            {
              family <- c(rev(which(dag(net)[,i]!=0)), i)
              if (length(family) > 1)
              {
                ps <- unlist(strsplit(prob.list[i], "\\(|\\)|\\,|\\;|\\:"))
              }
              else
              {
                ps <- gsub("table","", prob.list[i])
                ps <- unlist(strsplit(ps, "\\,|\\;"))
              }
              ps <- ps[which(ps != "")]
              suppressWarnings(ps <- ps[which(!is.na(as.numeric(ps)))])
              ps <- array(as.numeric(ps), dim=c(node.sizes[family]))
              cpts[[i]] <- ps
              
              dms <- NULL
              dns <- NULL
              for (j in 1:length(family))
              {
                dms[[j]] <- as.list(c(1:node.sizes[family[j]]))
                dns[[j]] <- c(variables[family[j]])
              }
              
              dimnames(cpts[[i]])          <- dms
              names( dimnames(cpts[[i]]) ) <- dns
            }
            names(cpts) <- variables
            
            cpts(net) <- cpts
            
            return(net)  
          })

#' @rdname write.dsc
#' @aliases write.dsc,BN
setMethod("write.dsc","BN",
          function(x, path="./")
          {
            file.name <- strcat(path, name(x), ".dsc")
            rows      <- NULL
            rows[[1]] <- strcat("belief network \"", name(x), "\"")
            k         <- 2
            
            num.nodes  <- num.nodes(x)
            node.sizes <- node.sizes(x)
            
            for (node in 1:num.nodes)
            {
              rows[[k]] <- strcat("node ", variables(x)[node], " {")
              k <- k+1
              s <- strcat("  type : ")
              if (discreteness(x)[node])
                s <- strcat(s, "discrete [ ")
              else
                s <- strcat(s, "continuous [ ")
              s <- strcat(s, node.sizes[node], " ] = { ")
              for (j in 1:(node.sizes[node]))
              {
                s <- strcat(s, '"', j, '"')
                if (j < node.sizes[node])
                  s <- strcat(s, ",")
                s <- strcat(s, " ")
              }
              s <- strcat(s, "}")
              rows[[k]]   <- s
              rows[[k+1]] <- "}"
              k           <- k+2
            }
            
            sdag <- graph.to.factors(dag(x), names=variables(x))
            sdag <- gsub("\\(", " ", sdag)
            sdag <- gsub("\\)", " ", sdag)
            sdag <- unlist(strsplit(sdag, " "), F, F)
            sdag <- sdag[-(which(sdag==""))]
            print(sdag)
            
            cpts <- cpts(x)
            print(cpts)
            print(cpts[[2]])
            print(c(cpts[[2]]))
            for (node in 1:num.nodes)
            {
              print(fast.bincombinations(prod(node.sizes[which(dag(x)[,node] > 0)])))
              snode <- sdag[node]
              snode <- gsub("\\|", " \\| ", snode)
              snode <- gsub("\\,", "\\, ", snode)
              rows[[k]] <- strcat("probability ( ", snode," ) {")
              k <- k+1
              for (j in length(cpts[[node]]))
              {
                for (k in 1:node.sizes[node])
                {
                  s <- strcat("(",k,")")
                }
                rows[[k]] <- s
                k <- k+1
              }
              rows[[k]] <- "}"
              k <- k+1
            }
            
            print(rows)
            write(rows, file=file.name)
          })
