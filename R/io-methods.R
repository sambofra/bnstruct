#' @rdname read.dataset
#' @aliases read.dataset,BNDataset,character,character
#' @importFrom utils data read.delim
setMethod("read.dataset",
          c("BNDataset", "character", "character"),
          function(object, data.file, header.file, data.with.header = FALSE,
                   na.string.symbol = '?', sep.symbol = "", starts.from = 1,
                   num.time.steps = 1)
          {
            header.file(object)  <- header.file
            data.file(object)    <- data.file

            a <- read.table(data.file, na.strings = na.string.symbol,
                            header = data.with.header, sep = sep.symbol) + (1 - starts.from)
            a <- as.matrix(a)
            
            ls                   <- readLines(header.file)

            #vars <- gsub('"', '', c(unlist(strsplit(ls[1], split = " "))))
            # modified to keep spaces in variable names
            # hat tip to https://stat.ethz.ch/pipermail/r-help/2012-November/342314.html
            res<-unlist(strsplit(ls[1],"[\"]"))
            res1<-res[res!=" "]
            res1<-res1[res1!=""]
            vars<-c(unlist(strsplit(res1[grepl("\\s+$",res1)]," ")),res1[!grepl("\\s+$",res1)])
            if (length(vars) == ncol(a)) {
              variables(object) <- vars
            } else if (num.time.steps > 1 && length(vars) * num.time.steps == ncol(a)) {
              copyvars <- c()
              for (t in 1:num.time.steps) {
                for (w in vars) {
                  copyvars <- c(copyvars, paste(w, as.character(t), sep='_t'))
                }
              }
              variables(object) <- copyvars
            } else {
              stop("Incoherent number of variable names in the dataset header (mismatch between header and data).")
            }
            
            lns                  <- c(unlist(strsplit(ls[2], split = "\\s+")))
            if (length(lns) == ncol(a)) {
              node.sizes(object)   <- sapply(1:length(lns), FUN=function(x){ as.numeric(lns[x]) })
            } else if (num.time.steps > 1 && length(lns) * num.time.steps == ncol(a)) {
              node.sizes(object)   <- rep(sapply(1:length(lns), FUN=function(x){ as.numeric(lns[x]) }), num.time.steps)
            } else {
              stop("Incoherent number of variable cardinalities in the dataset header (mismatch between header and data).")
            }
            
            disc <- c(unlist(strsplit(ls[3], split = "\\s+")))
            for (d in 1:length(disc)) {
              if (disc[d] %in% c("d","D","T","TRUE")) disc[d] <- 'D'
              else if (disc[d] %in% c("c","C","F","FALSE")) disc[d] <- 'C'
              else {
                bnstruct.log("Unrecognized status for variable ",variables(object)[d],", converting it to discrete.")
                disc[d] <- 'D'
              }
            }
            if (length(disc) == ncol(a)) {
              discreteness(object) <- disc
            } else if (num.time.steps > 1 && length(disc) * num.time.steps == ncol(a)) {
              discreteness(object) <-rep(disc, num.time.steps)
            } else {
              stop("Incoherent number of variable status in the dataset header (mismatch between header and data).")
            }
            
            
            raw.data(object)      <- as.matrix(a)
            num.variables(object) <- ncol(object@raw.data)
            num.items(object)     <- nrow(object@raw.data)
            
            if (num.variables(object) != length(variables(object)))
            {
              message(paste(c("Incoherent number of variables in dataset\n"), sep=''))
              quit(status = 1)
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

#' load a two-layers dataset derived from the \code{Asia} dataset.
#' 
#' Wrapper for a loader for a 2-layers dataset derived from the \code{Asia} dataset, with only raw data.
#' 
#' The dataset has 100 items, no missing data, so no imputation needs to be performed.
#' 
#' @name asia_2_layers
#' @rdname asia_2_layers
#' 
#' @return a BNDataset containing the \code{Child} dataset.
#' 
#' @examples
#' dataset <- asia_2_layers()
#' print(dataset)
#' 
#' @seealso \code{\link{asia_10000}}
#' 
#' @export
asia_2_layers <- function()
{
  data("asia_2_layers", envir=environment())
  return(get("asia_2_layers", envir=environment()))
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
#' @exportMethod show
setMethod("show", "AllTheClasses", function(object) print(object))


#' @rdname read.dsc
#' @aliases read.dsc,character
setMethod("read.dsc", c("character"),
          function(x)
          {
            file.name <- x
            all.file  <- readChar(file.name, file.info(file.name)$size)
            all.file  <- gsub('\n', ' ', all.file)
            all.file  <- gsub('\r', ' ', all.file)
            all.file  <- gsub('\t', ' ', all.file)
            all.file  <- gsub('"', '', all.file)
            all.file  <- gsub("\\[", " \\[ ", all.file)
            
            lines        <- unlist(strsplit(all.file, "node |probability"))
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
            
            #             print(nodes)
            #             print(probs)
            
            for (i in 1:num.nodes)
            {
              #               print(nodes[i])
              tmp <- unlist(strsplit(nodes[i], "\\{|\\}"))
              #print(tmp)
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
              family <- c(i, which(dag(net)[,i]!=0))
              if (length(family) > 1)
              {
                ps <- unlist(strsplit(prob.list[i], "\\(\\*\\)\\:|\\,|\\;|\\:"))
              }
              else
              {
                ps <- unlist(strsplit(prob.list[i], "\\,|\\;"))
              }
              ps <- ps[which(ps != "")]
              print(ps)
              # suppressWarnings(ps <- ps[which(!is.na(as.numeric(ps)))])
              nps <- c()
              to.remove <- FALSE
              for (ips in 1:length(ps))
              {
                curr.n <- suppressWarnings(as.numeric(ps[ips]))
                if (length(family) > 2 && is.na(curr.n))
                {
                  to.remove <- !to.remove
                }
                if (!to.remove && !is.na(curr.n))
                {
                  nps <- c(nps, curr.n)
                }
              }
              ps <- nps
              print(ps)
              ps <- array(as.numeric(ps), dim=node.sizes[family])
              print(ps)
              cpts[[i]] <- ps
              #             cpts[[i]] <- sort.dimensions(ps, c(length(family):1))$potential
              print(cpts[[i]])
              
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


# ' @importFrom grDevices col2rgb rgb
#' @rdname write_xgmml
#' @aliases write_xgmml,BN
setMethod("write_xgmml","BN",
          function(x, path="./network", write.wpdag=FALSE, node.col = rep('white',num.nodes(x)),
                   frac = 0.2, max.weight=max(wpdag(x)))
          {
            
            get.color.code <- function(colorname) {
              rgbcode <- col2rgb(colorname)
              return(rgb(rgbcode[1], rgbcode[2], rgbcode[3], maxColorValue=255))
            }
            
            weight.color.hex <- function(weight) {
              # weight <- 255 - weight
              return(rgb(weight, weight, weight, maxColorValue=255))
            }
            
            file.name <- strcat(path, ".xgmml")
            rows      <- NULL
            rows[[1]] <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
            rows[[2]] <- "<graph label=\"Network\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:cy=\"http://www.cytoscape.org\" xmlns=\"http://www.cs.rpi.edu/XGMML\"  directed=\"1\" Layout=\"Tree\">"
            rows[[3]] <- "<att name=\"documentVersion\" value=\"1.1\"/>"
            k         <- 4
            
            if (write.wpdag) {
              g <- wpdag(x)
              if (is.null(g))
                bnstruct.log("error: no wpdag to be saved")
              g <- g / max.weight
              g[which(g < frac)] <- 0
              g <- g * 255 # rescale for color
            } else {
              g <- dag(x)
              if (is.null(g))
                bnstruct.log("error: no dag to be saved")
            }
            
            num.nodes  <- num.nodes(x)
            
            # pass through an igraph to get the coordinates
            # otherwise cytoscape will put every node in the same place...
            ig.obj <- igraph::graph_from_adjacency_matrix(g)
            coords <- igraph::layout_nicely(ig.obj) * 3 * num.nodes # also, rescale
            # coords <- layout_with_graphopt(ig.obj) * 3 * num.nodes # also, rescale
            
            for (node in 1:num.nodes)
            {
              rows[[k]] <- strcat("<node label=\"", variables(x)[node], "\" id=\"",node,"\">")
              k <- k+1
              s <- strcat("    <att type=\"string\" name=\"NODE_TYPE\" value=\"DefaultNode\"/>")
              rows[[k]]   <- s
              s           <- strcat("    <att type=\"string\" name=\"canonicalName\" value=\"",variables(x)[node],"\"/>")
              rows[[k+1]] <- s
              k           <- k+2
              rows[[k]]   <- strcat("    <graphics type=\"ELLIPSE\" h=\"40.0\" w=\"40.0\" x=\"",coords[node,1],
                                    "\" y=\"",coords[node,2],"\" ",
                                    " fill=\"",get.color.code(node.col[node]),
                                    "\" width=\"1\" outline=\"#666666\" cy:nodeTransparency=\"1.0\" cy:nodeLabel=\"",
                                    variables(x)[node],"\" cy:borderLineType=\"solid\"/>")
              k           <- k+1
              rows[[k]]   <- "</node>"
              k           <- k+1
            }
            
            edge.counter <- 1
            for (i in 1:(num.nodes-1)) {
              for (j in (i+1):num.nodes) {
                # undirected edge
                if (g[i,j] > 0 && g[j,i] > 0) {
                  rows[[k]] <- strcat("<edge id=\"",edge.counter,"\" label=\"",edge.counter,"\" source=\"",i,"\" target=\"",j,"\" cy:directed=\"0\">")
                  k         <- k + 1
                  rows[[k]] <- "  <att name=\"shared name\" value=\"\" type=\"string\" cy:type=\"String\"/>"
                  k         <- k + 1
                  rows[[k]] <- "  <att name=\"shared interaction\" value=\"interacts with\" type=\"string\" cy:type=\"String\"/>"
                  k         <- k + 1
                  rows[[k]] <- "  <att name=\"canonicalName\" type=\"string\" cy:type=\"String\"/>"
                  k         <- k + 1
                  rows[[k]] <- "  <att name=\"edge.targetArrowShape\" type=\"string\" cy:type=\"String\"/>"
                  k         <- k + 1
                  rows[[k]] <- "  <att name=\"edge.sourceArrowShape\" type=\"string\" cy:type=\"String\"/>"
                  k         <- k + 1
                  rows[[k]] <- strcat("  <att name=\"name\" value=\"",i,"-",j,"\" type=\"string\" cy:type=\"String\"/>")
                  k         <- k + 1
                  rows[[k]] <- "<att name=\"selected\" value=\"1\" type=\"boolean\" cy:type=\"Boolean\"/>"
                  k         <- k + 1
                  rows[[k]] <- "  <att name=\"interaction\" value=\"interacts with\" type=\"string\" cy:type=\"String\"/>"
                  k         <- k + 1
                  rows[[k]] <- strcat("  <att name=\"edge.color\" value=\"",weight.color.hex(g[i,j]),"\" type=\"string\" cy:type=\"String\"/>")
                  k         <- k + 1
                  rows[[k]] <- strcat("  <att name=\"edge.targetArrowColor\" value=\"",weight.color.hex(g[i,j]),"\" type=\"string\" cy:type=\"String\"/>")
                  k         <- k + 1
                  rows[[k]] <- strcat("  <att name=\"edge.sourceArrowColor\" value=\"",weight.color.hex(g[i,j]),"\" type=\"string\" cy:type=\"String\"/>")
                  k         <- k + 1
                  rows[[k]] <- "</edge>"
                  k         <- k + 1
                  edge.counter <- edge.counter + 1
                  edge.counter <- edge.counter + 1
                } else if (g[i,j] > 0 || g[j,i] > 0) { # directed edge
                  if (g[i,j] > 0 && g[j,i] == 0) {
                    rows[[k]] <- strcat("<edge id=\"",edge.counter,"\" label=\"",edge.counter,"\" source=\"",i,"\" target=\"",j,"\" cy:directed=\"1\">")
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"shared name\" value=\"\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"shared interaction\" value=\"interacts with\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"canonicalName\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"edge.targetArrowShape\" value=\"DELTA\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"edge.sourceArrowShape\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"name\" value=\"",i,"-",j,"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"selected\" value=\"1\" type=\"boolean\" cy:type=\"Boolean\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"interaction\" value=\"interacts with\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"edge.color\" value=\"",weight.color.hex(g[i,j]),"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"edge.targetArrowColor\" value=\"",weight.color.hex(g[i,j]),"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"edge.sourceArrowColor\" value=\"",weight.color.hex(g[i,j]),"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- "</edge>"
                    k         <- k + 1
                    edge.counter <- edge.counter + 1
                  } else if (g[j,i] > 0 && g[i,j] == 0) {
                    rows[[k]] <- strcat("<edge id=\"",edge.counter,"\" label=\"",edge.counter,"\" source=\"",j,"\" target=\"",i,"\" cy:directed=\"1\">")
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"shared name\" value=\"\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"shared interaction\" value=\"interacts with\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"canonicalName\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"edge.targetArrowShape\" value=\"DELTA\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"edge.sourceArrowShape\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"name\" value=\"",j,"-",i,"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- "<att name=\"selected\" value=\"1\" type=\"boolean\" cy:type=\"Boolean\"/>"
                    k         <- k + 1
                    rows[[k]] <- "  <att name=\"interaction\" value=\"interacts with\" type=\"string\" cy:type=\"String\"/>"
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"edge.color\" value=\"",weight.color.hex(g[j,i]),"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"edge.targetArrowColor\" value=\"",weight.color.hex(g[j,i]),"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- strcat("  <att name=\"edge.sourceArrowColor\" value=\"",weight.color.hex(g[j,i]),"\" type=\"string\" cy:type=\"String\"/>")
                    k         <- k + 1
                    rows[[k]] <- "</edge>"
                    k         <- k + 1
                    edge.counter <- edge.counter + 1
                  }
                }
              }
            }
            
            rows[[k]] <- "</graph>"
            k         <- k + 1
            # print(rows)
            write(rows, file=file.name)
          })


# output log messages
bnstruct.log <- function(...)
{
  m <- ""
  
  blit <- get("bnstruct.log.indent.tracker", .bnstruct.env)
  if (blit > 0)
    for (i in seq_len(blit))
      m <- paste(m, "... ", sep='')
  
  m <- strcat(m, "bnstruct :: ")
  m <- strcat(m, ...)
  message(m)
}

# output begin-of-action log messages
bnstruct.start.log <- function(...)
{
  bnstruct.log(...)
  assign("bnstruct.log.indent.tracker", get("bnstruct.log.indent.tracker", .bnstruct.env) + 1, envir = .bnstruct.env)
}

# output begin-of-action log messages
bnstruct.end.log <- function(...)
{
  assign("bnstruct.log.indent.tracker", max(0,get("bnstruct.log.indent.tracker", .bnstruct.env) - 1), envir = .bnstruct.env)
  bnstruct.log(...)
}

bnstruct.reset.log <- function()
{
  assign("bnstruct.log.indent.tracker", 0, envir = .bnstruct.env)
}
