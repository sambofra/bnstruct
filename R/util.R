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

# Compute Structural Hamming Distance between graphs g1 and g2
#' compute the Structural Hamming Distance between two adjacency matrices.
#' 
#' Compute the Structural Hamming Distance between two adjacency matrices, that is,
#' the distance, in terms of edges, between two network structures. The lower the \code{shd},
#' the more similar are the two network structures.
#' 
#' @name shd
#' @rdname shd
#' 
#' @param g1 first adjacency matrix.
#' @param g2 second adjacency matrix.
#' 
#' @export shd
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

# sample a random chain from a dataset
sample.chain <- function( dataset )
{
  net <- BN(dataset)
  net.dag <- dag(net)
  n <- num.nodes(net)
  chain <- sample(n,n)
  for( i in 2:n )
    net.dag[chain[i-1],chain[i]] <- 1
  dag(net) <- net.dag
  return( suppressMessages(learn.params(net,dataset)) )
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
  
  quantiles.list <- list()
  # print(levels)
  
  for( i in 1:nc )
  {
    if( levels[i] == 0 )  {
      # already discrete
      quant[,i] <- as.matrix(data[,i],nr,1)
      quantiles.list[[i]] <- c() 
    }
    else
    {
      quantiles <- unique(quantile( data[,i], probs = (0:levels[i])/levels[i], na.rm = TRUE ))
      quantiles.list[[i]] <- quantiles
      # cut the range using the quantiles as break points.
      quant[,i] <- as.matrix( cut( data[,i], quantiles, labels=FALSE, include.lowest=TRUE),nr,1 )
    }
  }
  
  storage.mode(quant) <- "integer"
  # print(sapply(1:nc,function(x)max(quant[,x])))
  colnames(quant) <- colnames(data)
  return(list("quant"=quant, "quantiles"=quantiles.list))
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
      quant[[i]] <- unique(quantile( data[,i], probs = (0:levels[i])/levels[i], na.rm = TRUE ))
  
  names(quant) <- colnames(data)
  return(quant)
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

factors.to.graph <- function(factors, sep = '(')
{
  # compute adjacency matrix from factor chain
  # eg: (1)(2)(3|1,2)
  # accepts '(' or '[' as separator character
  # nodes are numbers from 1 to N
  # DOES NOT CHECK FOR CORRECTNESS OF INPUT
  l <- list()
  if (sep == '(') {
    sep1 = '('
    sep2 = ')'
  } else if (sep == '[') {
    sep1 = '['
    sep2 = ']'    
  } else {
    # error
  }
  
  for (i in unlist(strsplit(factors, sep1, TRUE)))
    for (j in unlist(strsplit(i, sep2, TRUE)))
      l[[length(l)+1]] <- list(j)
  
  num_nodes = length(l)
  am = matrix(rep(0, num_nodes*num_nodes), c(num_nodes,num_nodes))
  for (i in l)
  {
    item <- unlist(strsplit(unlist(i), "|",TRUE))
    if (length(item) > 1)
    {
      to <- as.integer(item[1])
      for (j in unlist(strsplit(unlist(item[2]), ",", TRUE)))
      {
        from <- as.integer(j)
        am[from,to] <- 1
      }    
    }
  }
  return(am)
}

graph.to.factors <- function(am, sep = '(', names = NULL)
{
  # compute factor chain from adjacency matrix
  # accepts '(' or '[' as separator character
  # nodes are numbers from 1 to N
  # names should be a vector containing the variable names
  # DOES NOT CHECK FOR CORRECTNESS OF INPUT
  l <- list()
  if (sep == '(') {
    sep1 = '('
    sep2 = ')'
  } else if (sep == '[') {
    sep1 = '['
    sep2 = ']'    
  } else {
    # error
  }
  
  if (missing(names) || is.null(names))
  {
    use.names <- FALSE
  }
  else
  {
    use.names <- TRUE
  }
  
  factors <- c()
  
  # build up string node after node
  for (i in 1:nrow(am))
  {
    if (use.names)
    {
      factor <- c(sep1, names[i])
    }
    else
    {
      factor = c(sep1,i)
    }
    parents <- which(am[,i] > 0)
    if (length(parents) > 0)
    {
      factor <- c(factor,'|')
      # build up parents
      while(length(parents) > 1)
      {
        if (use.names)
        {
          factor <- c(factor, names[parents[1]], ',')
        }
        else
        {
          factor <- c(factor, parents[1],',')
        }
        parents <- parents[-1]
      }
      if (use.names)
      {
        factor <- c(factor, names[parents[1]])
      }
      else
      {
        factor <- c(factor, parents[1])
      }
    }
    factor <- c(factor,sep2)
    factors <- c(factors, factor)
  }
  factors <- paste(unlist(factors), collapse='')
  return(factors)
}

# concatenate strings: paste an arbitrary number of strings with default sep=''
# input strings are not checked, be careful
strcat <- function(..., sep = '')
{
  s <- ""
  args <- list(...)
  for (i in unlist(args))
  {
    s <- paste(s, as.character(i), sep=sep)
  }
  return(s)
}


fast.bincombinations <- function(p)
{
  # computes all the combinations of p elements
  # many many thanks to
  # http://stackoverflow.com/questions/13891604
  return(vapply(X = seq_len(p),
         FUN = function(i)rep(rep(0L:1L, each = 2^(p-i)), times = 2^(i-1)),
         FUN.VALUE = integer(2^(p))))
}


# identifies the connected components of a network structure,
# in case the DAG is divided into disconnected subnetworks.
# Return: a list whose elements are arrays containings the subgraphs
identify.subgraphs <- function(am) {

    visited <- rep(0, nrow(am))
    subgs <- list()
    csd <- c()
    l <- 1

    while (length(which(visited == 0)) > 0) {
      curr <- which(visited == 0)[1]
      visited[curr] <- l
      csd = c(curr)
      connected <- c(which(am[,curr] > 0), which(am[curr,] > 0))
      while(length(connected) > 0) {
        i <- connected[1]
        connected <- connected[-1]
        if (visited[i] == 0) {
          connected <- unique(sort(c(connected,which(am[,i] > 0), which(am[i,] > 0))))
          visited[i] <- l
          csd <- c(csd, i)
        }
      } 
      subgs[[l]] <- sort(csd)
      csd <- c()
      l <- l+1
    }

    return(subgs)
}
