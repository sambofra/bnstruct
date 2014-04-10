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

fast.bincombinations <- function(p)
{
  # computes all the combinations of p elements
  # many many thanks to
  # http://stackoverflow.com/questions/13891604
  vapply(X = seq_len(p),
         FUN = function(i)rep(rep(0L:1L, each = 2^(p-i)), times = 2^(i-1)),
         FUN.VALUE = integer(2^(p)))
}

PriorityQueue <- function() {
  # with many thanks to RosettaCode
  # http://rosettacode.org/wiki/Priority_queue#R
  keys <<- values <<- NULL
  insert <- function(key, value) {
    temp <- c(keys, key)
    ord <- order(temp)
    keys <<- temp[ord]
    values <<- c(values, list(value))[ord]
  }
  pop <- function() {
    head <- values[[1]]
    values <<- values[-1]
    keys <<- keys[-1]
    return(head)
  }
  empty <- function() length(keys) == 0
  list(insert = insert, pop = pop, empty = empty)
}


Stack <- function()
{
  # won't work for lists or matrices or other complex structures, but for me, now, seems ok
  elements <<- NULL
  num.els <<- 0
  push <- function(element)
  {
    num.els <<- num.els + 1
    elements[[num.els]] <<- element
  }
  pop <- function()
  {
    if (num.els == 0)
    {
      ret <- NULL
    } else {      
      ret <- elements[[num.els]]
      elements <<- elements[-num.els]
      num.els <<- num.els - 1
    }
    return(ret)
  }
  empty <- function()
  {
    elements <<- NULL
    num.els <<- 0
  }
  size <- function() {
    return(num.els)
  }
  find <- function(target) {
    res <- which(c(elements) == target)
    if (length(res) == 0) res <- NULL # just because I'm not sure how to handle integer(0)
    return(list("result" = res, "count" = length(res)))
  }
  get.elements <- function(){
    return(elements)
  }
  list(push = push, pop = pop, empty = empty, size = size, find = find, get.elements = get.elements)
}

Queue <- function()
{
  # won't work for lists or matrices or other complex structures, but for me, now, seems ok
  elements <<- NULL
  num.els <<- 0
  push <- function(element)
  {
    num.els <<- num.els + 1
    elements[[num.els]] <<- element
  }
  pop <- function()
  {
    if (num.els == 0)
    {
      ret <- NULL
    } else {      
      ret <- elements[[num.els]]
      elements <<- elements[-1]
      num.els <<- num.els - 1
    }
    return(ret)
  }
  empty <- function()
  {
    elements <<- NULL
    num.els <<- 0
  }
  size <- function() {
    return(num.els)
  }
  find <- function(target) {
    res <- which(c(elements) == target)
    if (length(res) == 0) res <- NULL # just because I'm not sure how to handle integer(0)
    return(list("result" = res, "count" = length(res)))
  }
  get.elements <- function(){
    return(elements)
  }
  list(push = push, pop = pop, empty = empty, size = size, find = find, get.elements = get.elements)
}
