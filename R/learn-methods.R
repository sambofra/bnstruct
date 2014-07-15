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
            
            node.sizes <- bn@node.sizes
            dag        <- bn@dag
            n.nodes    <- bn@num.nodes
            print(dag)
            
            #n.nodes <- dataset@num.items #dim(data)[2]
            cpts <- vector("list",n.nodes)
            var.names <- c(unlist(bn@variables))  # colnames(data)
            d.names <- mapply(function(name,size)(1:size),var.names,node.sizes)
            # print(d.names)
            # esimate a cpt for each family from data
            for ( i in 1:n.nodes )
            {
              family <- c( which(dag[,i]!=0), i )
              counts <- .Call( "compute_counts_nas", data[,family], node.sizes[family], 
                               PACKAGE = "bnstruct" )
              cpts[[i]] <- counts.to.probs( counts + ess / prod(dim(counts)) )
              dimnames(cpts[[i]]) <- d.names[family]
            }
            names( cpts ) <- as.list(var.names)
            #return( cpts )
            
            bn@cpts <- cpts
            bn
          }
)

setMethod("learn.structure",
          c("BN", "BNDataset"),
          function(bn, dataset, algo = "mmhc", alpha = 0.05, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = bn@num.nodes, cont.nodes = c(), raw.data = FALSE)
          {
            bn@num.nodes  <- dataset@num.variables
            bn@node.sizes <- dataset@node.sizes
            bn@variables  <- dataset@variables
            
            node.sizes <- bn@node.sizes
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
                bn@dag  <- sm(data, node.sizes, cont.nodes, max.fanin, layering, max.fanin.layers)
              }
              return(bn)
            }
            
            # if (algo == "mmhc") default
            {
              cpc    <- mmpc( data, node.sizes, cont.nodes, alpha, layering )
              bn@dag <- hc( data, node.sizes, cpc, cont.nodes )
              return(bn)
            }
          })