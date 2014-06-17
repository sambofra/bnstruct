# Convert a CPT outputted by learn.params for a fancy print
fancy.cpt <- function( cpt )
{
  d <- dim(cpt)
  dn <- dimnames(cpt)
  if( length(dn) <= 2 )
    return( cpt )
  new.dn <- list(apply(expand.grid(dn[1:(length(dn)-1)],stringsAsFactors=T),
                  1,paste,collapse=", "),dn[[length(dn)]])
  names(new.dn) <- list(paste(names(dn[1:(length(dn)-1)]),collapse=","),
                        names(dn)[length(dn)])
  dim( cpt ) <- c(prod(d[1:(length(d)-1)]),d[length(d)])
  dimnames(cpt) <- new.dn
  return( cpt )
}

# Learn the CPTs of each node, given data, DAG, node sizes and equivalent sample size
# CPTs have the parents on dimensions 1:(n-1) and the child on the last dimension,
# so that the sum over the last dimension is always 1
learn.params <- function(data, dag, node.sizes, ess = 1)
{
	# just to play safe
	storage.mode(data) <- "integer"
	storage.mode(dag) <- "integer"
	storage.mode(node.sizes) <- "integer"
	
	n.nodes <- dim(data)[2]
	cpts <- vector("list",n.nodes)
  var.names <- colnames(data)
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
	return( cpts )
}

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

# Compute Structural Hamming Distance between graphs g1 and g2
shd <- function(g1, g2)
{
  dif <- (g1 != g2)
  sum( dif | t(dif) ) / 2
}

# Check if the (directed) graph is acyclic (recoded in c as "is_acyclic")
is.acyclic <- function(g)
{
  rem <- rep(FALSE,nrow(g))
  while( !all(rem) ) # still some edges to remove
  {
    leaves <- (rowSums(g) == 0)
    if( !any(leaves & !rem) )
      return(FALSE)
    g[,leaves] <- 0L
    rem <- rem | leaves
  }
  return(TRUE)
}

# Quantize each column i of the continuous matrix data in a number of levels 
# equal to levels[i]
# 
# levels[i] == 0 if the column is already discrete
#
quantize.matrix <- function(data, levels) 
{
  nr <- nrow(data)
  nc <- ncol(data)
  
  quant <- matrix(0,nr,nc)
  
  for( i in 1:nc )
  {
    if( levels[i] == 0 )  # already discrete
      quant[,i] <- as.matrix(data[,i],nr,1)
    else
    {
      quantiles <- quantile( data[,i], probs = (0:levels[i])/levels[i], na.rm = TRUE )
      # cut the range using the quantiles as break points.
      quant[,i] <- as.matrix( cut( data[,i], quantiles, labels=FALSE, include.lowest=TRUE),nr,1 )
    }
  }
  
  storage.mode(quant) <- "integer"
  colnames(quant) <- colnames(data)
  return(quant)
}

# Compute quantiles for each column i of the continuous matrix data, 
# given numbers of levels equal to levels[i]
# 
# levels[i] == 0 if the column is already discrete
#
quantiles.matrix <- function(data, levels) 
{
  nr <- nrow(data)
  nc <- ncol(data)
  
  quant <- vector("list",nc)
  
  for( i in 1:nc )
    if( levels[i] != 0 )
      quant[[i]] <- quantile( data[,i], probs = (0:levels[i])/levels[i], na.rm = TRUE )
  
  names(quant) <- colnames(data)
  return(quant)
}

# Plot a weighted connectivity matrix using Rgraphviz
plot.mat <- function( mat, node.names = as.character(1:ncol(mat)), frac = 0.2, 
                      max.weight = max(mat), node.col = rep('white',ncol(mat)) )
{
  # check for Rgraphviz
  if (!require(Rgraphviz))
    stop("this function requires the Rgraphviz package.")
  
  # adjacency matrix
  mat.th <- mat
  mat.th[mat <  frac*max.weight] <- 0
  mat.th[mat >= frac*max.weight] <- 1
  # build graph
  g <- graphAM( mat.th, edgemode="directed")
  nodes(g) <- node.names
  en <- edgeNames(g,recipEdges="distinct")
  g <- layoutGraph(g)
  # set edge darkness proportional to confidence
  conf <- mat.th*pmax(mat,t(mat)) # both values to the maximum for edges with 2 directions
  col <- colors()[253-100*(t(conf)[t(conf) >= frac*max.weight]/max.weight)]
  names(col) <- en
  edgeRenderInfo(g) <- list(col=col,lwd=2)
  node.fill <- as.list(node.col)
  names(node.fill) <- node.names
  nodeRenderInfo(g) <- list(fill=node.fill)
  renderGraph(g)
}

dag.to.cpdag <- function(dag, layering = NULL)
{
	return(abs(label.edges(dag, layering)))
}

label.edges <- function(dag, layering = NULL)
{
	# LABEL-EDGES produce a N*N matrix which values are
	# 	+1 if the edge is compelled or
	#	-1 if the edge is reversible.

	N<-nrow(dag)
	o <- order.edges(dag)
	order <- o$order
	xedge <- o$x
	yedge <- o$y
	
	label <- 2*dag
	NbEdges <- length(xedge)
	
	# edges between layers are compelled
	if( !is.null(layering) )
	{
		layers = length(unique(layering))
		for( l in 1:(layers-1) )
			label[ intersect(xedge,which(layering==l)), intersect(yedge,which(layering>l)) ] <- 
				dag[ intersect(xedge,which(layering==l)), intersect(yedge,which(layering>l)) ]
	} 
	
	for( Edge in 1:NbEdges)
	{
		xlow <- xedge[Edge]
		ylow <- yedge[Edge]
		if( label[xlow,ylow] == 2 )
		{
			fin <- 0
			wcompelled <- which(label[,xlow] == 1)
			parenty <- which(label[,ylow] != 0)
			
			for( s in seq_len(length(wcompelled)) )
			{
				w <- wcompelled[s]
				if( !(w %in% parenty) )
				{
					label[parenty,ylow] <- 1
					fin <- 1
				}
            else if( fin == 0 ) label[w,ylow] <- 1
			}
			
			if( fin == 0 )
			{
				parentx <- c(xlow,which(label[,xlow] != 0))
				
				if( length(setdiff(parenty,parentx) > 0) )
    				label[which(label[,ylow] == 2), ylow] <- 1
				else
				{
					label[xlow,ylow] <- -1
					label[ylow,xlow] <- -1
					ttp <- which(label[,ylow] == 2)
					label[ttp,ylow] <- -1
					label[ylow,ttp] <- -1
				}
			}
		}
	}
	return(label)
}

order.edges <- function(dag)
# ORDER_EDGES produce a total (natural) ordering over the edges in a DAG.
{
	N <- nrow(dag)
	order <- matrix(c(0),N,N)

	node_order <- topological.sort(dag)
	oo <- sort(node_order,index.return=TRUE)$ix
	dag <- dag[oo,oo]
	xy <- which(dag == 1, arr.ind = TRUE)
	nb.edges <- nrow(xy)

	if( nb.edges != 0)
		order[xy] <- 1:nb.edges

	order <- order[node_order,node_order]
	x <- oo[xy[,1]]
	y <- oo[xy[,2]]

	return(list(order=order,x=x,y=y))
}

ind2subv <- function(siz,index)
{
	# IND2SUBV   Subscript vector from linear index.
	# IND2SUBV(SIZ,IND) returns a vector of the equivalent subscript values 
	# corresponding to a single index into an array of size SIZ.
	# If IND is a vector, then the result is a matrix, with subscript vectors
	# as rows.

	n <- length(siz)
	if( n == 0 ) 
		return( index )
		
	cum.size <- cumprod(siz)
	prev.cum.size <- c(1,cum.size[seq_len(length(siz)-1)])
	index <- index - 1
	sub <- rep(index,n) %% rep(cum.size,length(index))
	sub <- sub %/% rep(prev.cum.size,length(index)) + 1
	
	return(sub)
}

topological.sort <- function(dag)
# TOPOLOGICAL_SORT Return the nodes in topological order (parents before children).
{
	n <- nrow(dag)
	
	# assign zero-indegree nodes to the top
	fringe <- which( colSums(dag)==0 )
	order <- rep(0,n)
	
	i <- 1
	while( length(fringe) > 0 )
	{
		ind <- head(fringe,1) # pop
		fringe <- tail(fringe,-1)
		order[ind] <- i
		i <- i + 1 
		
		for( j in which(dag[ind,] != 0) )
		{
			dag[ind,j] <- 0
			if( sum(dag[,j]) == 0 )
				fringe <- c(fringe,j)
		}
	}
	
	return(order)
}
