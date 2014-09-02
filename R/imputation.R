#' Perform imputation of a data frame using k-NN.
#' 
#' Perform imputation of missing data in a data frame using the k-Nearest Neighbour algorithm.
#' For discrete variables we use the mode, for continuous variables the median value is instead taken.
#' 
#' @name knn.impute
#' @rdname knn.impute
#' 
#' @param data a data frame
#' @param k number of neighbours to be used; for discrete variables we use mode, for continuous variables the median value is instead taken.
#' @param cat.var vector containing the cardinality of the variables, if discrete,
#'        or the number of levels the variables are going to be quantized into, if continuous.
#'        
#' @return imputed data frame.
#' 
#' @export knn.impute
knn.impute <- function( data, k, cat.var )
{
  n.cases <- dim(data)[1]
  n.var <- dim(data)[2]
  num.var <- setdiff(1:n.var,cat.var)
  t.data <- t(data)
  
  num.var.max <- apply(data[,num.var],2,max,na.rm=TRUE)
  num.var.min <- apply(data[,num.var],2,min,na.rm=TRUE)
  # num.var.range <- matrix(rep(num.var.max-num.var.min,n.cases),dim(data),byrow=TRUE)
  num.var.range <- num.var.max-num.var.min
  
  imp.data <- data
  
  na.cases <- which(rowSums(is.na(data)) > 0)
  neigh <- rep(0,k)
  storage.mode(num.var) <- "integer"
  
  for( i in na.cases )
  {
    # d <- heom.dist( data[i,], data, num.var, num.var.range )
    d <- .Call( "heom_dist", data[i,], data, num.var, num.var.range, PACKAGE = "bnstruct" )
    s <- sort(d, index.return=TRUE)$ix
    for( j in which(is.na(data[i,])) )
    {
      # find the k closest neighbours with nonmissing value for j
      ind.neigh <- 1
      ind.s <- 2 # the first element is the case itself
      while( ind.neigh < k + 1)
      {
        if( !is.na(data[s[ind.s],j]) )
        {
          neigh[ind.neigh] <- data[s[ind.s],j]
          ind.neigh <- ind.neigh + 1
        }
        ind.s <- ind.s + 1
      }
      
      # impute from the neighbours
      if( j %in% cat.var )
        imp.data[i,j] <- stat.mode.ord(neigh)
      else
        imp.data[i,j] <- median(neigh)      
    }
  }
  
  return(imp.data)
}

# Heterogeneous Euclidean Overlap Metric between 
# vector x and the rows of matrix Xr
# heom.dist <- function( x, Xr, num.var, num.var.range ) 
# {
#   dXr <- dim(Xr)
#   rx <- matrix(rep(x,dXr[1]),dXr[1],dXr[2],byrow=TRUE)
#   cat.var <- setdiff(1:dXr[2],num.var)
#   
#   # compute distance matrix (numeric variables)
#   num.dist <- (rx[,num.var] - Xr[,num.var]) / num.var.range
#   num.dist[is.na(num.dist)] <- 1
#   
#   # compute distance matrix (categorical variables)
#   cat.dist <- rx[,cat.var] != Xr[,cat.var]
#   cat.dist[is.na(cat.dist)] <- 1
#   
#   dist = 0
#   if ( length(cat.var) < 2 )
#     dist <- dist + sum(cat.dist^2)
#   else
#     dist <- dist + rowSums(cat.dist^2)
#   if( length(num.var) < 2 )
#     dist <- dist + sum(num.dist^2)
#   else
#     dist <- dist + rowSums(num.dist^2)
#   dist <- sqrt(dist)
#   
#   return(dist)
# }

# statistical mode, for ordered categorical vectors. 
# returns the first mode in case of multiple modes
stat.mode.ord <- function(x) 
{
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# statistical mode, for categorical vectors. 
# returns a vector in case of multiple modes
stat.mode <- function(x) 
{
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[which(tab == max(tab))]
}