#' @rdname learn.params-methods
#' @aliases learn.params
setMethod("learn.params",
          c("BN", "BNDataset"),
          function(bn, dataset, ess = 1)
          #learn.params <- function(data, dag, node.sizes, ess = 1)
          {
            # Learn the CPTs of each node, given data, DAG, node sizes and equivalent sample size
            # CPTs have the parents on dimensions 1:(n-1) and the child on the last dimension,
            # so that the sum over the last dimension is always 1

            # just to play safe
            data <- get.data(dataset)
            
            storage.mode(data) <- "integer"
            storage.mode(bn@dag) <- "integer"
            storage.mode(bn@node.sizes) <- "integer"
            
            node.sizes <- node.sizes(bn)
            dag        <- dag(bn)
            n.nodes    <- num.nodes(bn)
            variables  <- variables(bn)
            
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
            bn
          }
)

#' @rdname learn.structure-methods
#' @aliases learn.structure
setMethod("learn.structure",
          c("BN", "BNDataset"),
          function(bn, dataset, algo = "mmhc", alpha = 0.05, ess = 1, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset), cont.nodes = c(), raw.data = FALSE)
          {
            num.nodes(bn)  <- num.variables(dataset)
            node.sizes(bn) <- node.sizes(dataset)
            variables(bn)  <- variables(dataset)
            validObject(bn)
            
            node.sizes <- node.sizes(bn)
            
            if (bootstrap)
            {
              # todo
            }
            else
            {
              if (raw.data)
                data   <- get.raw.data(dataset)
              else
                data   <- get.data(dataset)
            }

            if (algo == "sm")
            {
              if (bootstrap)
              {
#                 res.sm.boot <- boot.bn(as.matrix(a), node.sizes, B, cont.nodes, 
#                                        verbose=TRUE, k.impute=10, method = "sm",
#                                        max.fanin = max.fanin, layering = layering,
#                                        max.fanin.layers = max.fanin.layers )
              }
              else
              {                
#                 print(node.sizes)
#                 print(cont.nodes)
#                 print(max.fanin)
#                 print(layering)
#                 print(max.fanin.layers)
                dag(bn)  <- sm(data, node.sizes, cont.nodes, max.fanin, layering, max.fanin.layers)
              }
              return(bn)
            }
            
            # if (algo == "mmhc") # default
            {
              cpc    <- mmpc( data, node.sizes, cont.nodes, alpha, layering )
              dag(bn) <- hc( data, node.sizes, cpc, cont.nodes )
              return(bn)
            }
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