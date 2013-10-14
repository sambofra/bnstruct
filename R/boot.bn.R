boot.bn <- function(x, node.sizes, B = 100, cont.nodes = NULL, 
	max.fanin = NULL, layering = NULL, max.fanin.layers = NULL, 
	iss = 1, verbose = FALSE, seed = 0, k.impute = 0, chi.th = 0 )
{
	set.seed(seed) 
	n.nodes <- ncol(x)
	n.cases <- nrow(x) 
	boot.sample <- matrix(sample.int(n.cases,size = B*n.cases,replace=TRUE),n.cases,B)
	
	finalPDAG <- matrix(0,n.nodes,n.nodes)
	
	for( i in seq_len(B) )
	{
		if( verbose ) print(i)
    
    data <- x[boot.sample[,i],]
    print("impute")
    if( k.impute > 0 )
      data <- knn.impute( data, k.impute, setdiff(1:n.nodes,cont.nodes) )
    
    # write.table( data, paste(i,".txt",sep=""), quote=F, row.names=F, col.names=F )
    
		#if( is.integer(node.sizes) )
		print("cpc")
    if( chi.th > 0 )
      cpc.mat <- mmpc( data, node.sizes, cont.nodes, chi.th )
    else
      cpc.mat <- NULL
    
		print("sm")
    dag <- sm(data,
              node.sizes,
              cont.nodes,
              max.fanin,
              layering,
              max.fanin.layers,
              iss, cpc.mat)
    # print(dag)
		print("cpdag")
    finalPDAG <- finalPDAG + dag.to.cpdag( dag, layering )
    # print( dag.to.cpdag( dag, layering ) )
	}
	
	return(finalPDAG)	
}