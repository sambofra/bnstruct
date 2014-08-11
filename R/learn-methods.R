#' @rdname learn.params
#' @aliases learn.params,BN,BNDataset
setMethod("learn.params",
          c("BN", "BNDataset"),
          function(bn, dataset, ess = 1)
          #learn.params <- function(data, dag, node.sizes, ess = 1)
          {
            # Learn the CPTs of each node, given data, DAG, node sizes and equivalent sample size
            # CPTs have the parents on dimensions 1:(n-1) and the child on the last dimension,
            # so that the sum over the last dimension is always 1

            # just to play safe
            data <- as.matrix(get.data(dataset))

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
            print(var.names)
            d.names <- mapply(function(name,size)(1:size),var.names,node.sizes)
            # esimate a cpt for each family from data
            for ( i in 1:n.nodes )
            {
              print(i)
              family <- c( which(dag[,i]!=0), i )
              print(family)
              print(data[,family])
              counts <- .Call( "compute_counts_nas", data[,family], node.sizes[family], 
                               PACKAGE = "bnstruct" )
              #print(c(counts))
              print("counts")
              print(class(counts))
              print(counts)
              #tmp <- array(c(counts + ess / prod(dim(counts))), dim=dim(counts))
              print("tmp")
              print(ess/prod(dim(counts)))
              print(dim(counts))
              cpts[[i]] <- counts.to.probs(  counts)# + ess / prod(dim(counts)))
#               storage.mode(counts) <- "numeric"
#               print(counts)
#               print(prod(dim(counts)))
#               print(c(unlist(counts)))
#               print(c(counts) + ess / prod(dim(counts)))
#               readLines(file("stdin"),1)
              # cpts[[i]] <- counts.to.probs( array(c(counts + ess / prod(dim(counts))), dim(counts)) )
              print("cpts")
              print(cpts)
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
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset), cont.nodes = c(), raw.data = FALSE,
                   num.boots = 100, imputation = TRUE, k.impute = 10, na.string.symbol='?', seed = 0)
          {
            num.nodes(bn)  <- num.variables(dataset)
            node.sizes(bn) <- node.sizes(dataset)
            variables(bn)  <- variables(dataset)
            validObject(bn)
            
            node.sizes <- node.sizes(bn)
            num.nodes  <- num.nodes(bn)
            
            if (bootstrap)
            {
              if (!has.boots(dataset))
              {
                dataset <- bootstrap(dataset, num.boots = num.boots,
                                     seed = seed, imputation = imputation,
                                     k.impute = k.impute, na.string.symbol = na.string.symbol)
              }
              else
              {
                num.boots <- num.boots(dataset)
              }
            }
            else
            {
              if (raw.data)
                data   <- get.raw.data(dataset)
              else
                data   <- get.data(dataset)
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
                dag(bn)  <- sm(data, node.sizes, scoring.func, cont.nodes, max.fanin, layering, max.fanin.layers, ess)
              }
              return(bn)
            }
            
            # if (algo == "mmhc") # default
            {
              if (bootstrap)
              {
                finalPDAG <- matrix(0,num.nodes,num.nodes)
                for( i in seq_len(num.boots(dataset)) )
                {
                  data <- get.boot(dataset, i, imputed=!raw.data)
                  
                  cpc <- mmpc( data, node.sizes, cont.nodes, alpha, layering )
                  dag <- hc( data, node.sizes, scoring.func, cpc, cont.nodes )
                  
                  finalPDAG <- finalPDAG + dag.to.cpdag( dag, layering )
                }
                wpdag(bn) <- finalPDAG
              }
              else
              {
                cpc     <- mmpc( data, node.sizes, cont.nodes, alpha, layering )
                dag(bn) <- hc( data, node.sizes, scoring.func, cpc, cont.nodes )
              }
              return(bn)
            }
          })

counts.to.probs <- function( counts )
{
  print(class(counts))
  print("counts.to.probs")
  print(counts)
  print(class(counts))
  d <- dim(counts)
  print(d)
  if( length(d) == 1 )
    return( counts / sum(counts) )
  else
  {
    # last dimension on the columns, everything else on the rows
    print("nor")
    tmp.d <- c( prod(d[1:(length(d)-1)]), d[length(d)] )
    print("nor")
    dim(counts) <- tmp.d
    print("nor")
    # normalization
    nor <- rowSums( counts )
    print("nor")
    nor <- nor + (nor == 0) # for the next division
    print("nor")
    counts <- counts / array(nor,tmp.d)
    dim(counts) <- d
    return( counts )
  }
}