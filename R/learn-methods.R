#' @rdname learn.params
#' @aliases learn.params,BN,BNDataset
setMethod("learn.params",
          c("BN", "BNDataset"),
          function(bn, dataset, ess = 1, use.imputed.data = FALSE)
          #learn.params <- function(data, dag, node.sizes, ess = 1)
          {
            # Learn the CPTs of each node, given data, DAG, node sizes and equivalent sample size
            # CPTs have the parents on dimensions 1:(n-1) and the child on the last dimension,
            # so that the sum over the last dimension is always 1

            # just to play safe
            if (use.imputed.data)
              data <- as.matrix(imputed.data(dataset))
            else
              data <- as.matrix(raw.data(dataset))

            storage.mode(data) <- "integer"
            
            node.sizes <- node.sizes(bn)
            dag        <- dag(bn)
            n.nodes    <- num.nodes(bn)
            variables  <- variables(bn)
            
#             storage.mode(dag) <- "integer"
            storage.mode(node.sizes) <- "integer"

            #n.nodes <- dataset@num.items #dim(data)[2]
            cpts <- list("list",n.nodes)
            var.names <- c(unlist(variables))  # colnames(data)
            d.names <- mapply(function(name,size)(1:size),var.names,node.sizes)
            # esimate a cpt for each family from data
            for ( i in 1:n.nodes )
            {
              family <- c( which(dag[,i]!=0), i )
              counts <- .Call( "compute_counts_nas", data[,family], node.sizes[family], 
                               PACKAGE = "bnstruct" )
              cpts[[i]] <- counts.to.probs( counts + ess / prod(dim(counts)) )
              dms <- NULL
              dns <- NULL
              for (j in 1:length(family))
              {
                dms[[j]] <- as.list(c(1:node.sizes[family[j]]))
                dns[[j]] <- c(var.names[family[j]])
              }
              
              dimnames(cpts[[i]])          <- dms
              names( dimnames(cpts[[i]]) ) <- dns
                
            }
            names(cpts) <- var.names
            
            #return( cpts )
            
            cpts(bn) <- cpts
            return(bn)
          }
)

#' @rdname learn.structure
#' @aliases learn.structure,BN,BNDataset
setMethod("learn.structure",
          c("BN", "BNDataset"),
          function(bn, dataset, algo = "mmhc", scoring.func = "BDeu", alpha = 0.05, ess = 1, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL, max.fanin = num.variables(dataset),
                   layer.struct = NULL, cont.nodes = c(), use.imputed.data = FALSE, ...)
          {
            num.nodes(bn)  <- num.variables(dataset)
            node.sizes(bn) <- node.sizes(dataset)
            variables(bn)  <- variables(dataset)
            validObject(bn)
            
            node.sizes <- node.sizes(bn)
            num.nodes  <- num.nodes(bn)
            
            if (length(cont.nodes) == 0)
              cont.nodes <- setdiff(1:num.nodes,which(discreteness(dataset)))
            
            if (bootstrap)
            {
              if (!has.boots(dataset))
              {
                stop("Bootstrap samples not available. Please generate samples before learning with bootstrap.\nSee > ?bootstrap for help.")
              }
              else
              {
                num.boots <- num.boots(dataset)
              }
            }
            else
            {
              if (use.imputed.data && has.imputed.data(dataset))
                data   <- imputed.data(dataset)
              else if (use.imputed.data && !has.imputed.data(dataset))
                stop("Imputed data not available. Please impute data before learning.\nSee < ?impute for help.")
              else
                data   <- raw.data(dataset)
            }
            
            scoring.func <- match(tolower(scoring.func), c("bdeu", "aic", "bic"))
            if (is.na(scoring.func))
            {
              message("scoring function not recognized, using BDeu")
              scoring.func <- 0
            }
            else {
              scoring.func <- scoring.func - 1
            }
            scoring.func(bn) <- c("BDeu", "AIC", "BIC")[scoring.func + 1]
            
            if (algo == "sm")
            {
              if (bootstrap)
              {
                finalPDAG <- matrix(0,num.nodes,num.nodes)
                for( i in seq_len(num.boots(dataset)) )
                {
                  data <- get.boot(dataset, i, imputed=!raw.data)
                  
                  dag <- sm(data, node.sizes, scoring.func, cont.nodes, max.fanin, layering,
                            max.fanin.layers, ess)
                  
                  finalPDAG <- finalPDAG + dag.to.cpdag( dag, layering )
                }
                wpdag(bn) <- finalPDAG
              }
              else
              {     
                dag(bn)  <- sm(data, node.sizes, scoring.func, cont.nodes,
                               max.fanin, layering, max.fanin.layers, ess)
              }
            } # end if algo == sm
            
            if (algo == "sem")
            {
              other.args <- list(...)

              if ("mmpc" %in% names(other.args))
              {
                mmpc <- as.logical(other.args$mmpc)
                print(mmpc)
              }
              else
              {
                mmpc <- TRUE
              }
              
              if ("tabu.tenure" %in% names(other.args))
              {
                tabu.tenure <- as.numeric(other.args$tabu.tenure)
                print(tabu.tenure)
              }
              else
              {
                tabu.tenure <- TRUE
              }

              sem.output <- sem()
              
              readLines(file("stdin"),1)
            } # end if (algo == sem)
            
            if (algo == "mmhc") # default
            {
              if (bootstrap)
              {
                finalPDAG <- matrix(0,num.nodes,num.nodes)
                for( i in seq_len(num.boots(dataset)) )
                {
                  data <- get.boot(dataset, i, imputed=!raw.data)
                  
                  cpc <- mmpc( data, node.sizes, cont.nodes, alpha, layering, layer.struct )
                  dag <- hc( data, node.sizes, scoring.func, cpc, cont.nodes )
                  
                  finalPDAG <- finalPDAG + dag.to.cpdag( dag, layering )
                }
                wpdag(bn) <- finalPDAG
              }
              else
              {
                cpc     <- mmpc( data, node.sizes, cont.nodes, alpha, layering, layer.struct )
                dag(bn) <- hc( data, node.sizes, scoring.func, cpc, cont.nodes )
              }
            } # end if algo == mmhc
            
            struct.algo(bn) <- algo
            
            return(bn)
          })

counts.to.probs <- function( counts )
{
  d <- dim(counts)
  if( length(d) == 1 )
    return( counts / sum(counts) )
  else
  {
    # last dimension on the columns, everything else on the rows
    tmp.d <- c( prod(d[1:(length(d)-1)]), d[length(d)] )
    dim(counts) <- tmp.d
    # normalization
    nor <- rowSums( counts )
    nor <- nor + (nor == 0) # for the next division
    counts <- counts / array(nor,tmp.d)
    dim(counts) <- d
    return( counts )
  }
}