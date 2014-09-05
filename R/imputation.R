#' Perform imputation of a data frame using k-NN.
#' 
#' Perform imputation of missing data in a data frame using the k-Nearest Neighbour algorithm.
#' For discrete variables we use the mode, for continuous variables the median value is instead taken.
#' 
#' @name knn.impute
#' @rdname knn.impute
#' 
#' @param data a data frame
#' @param k number of neighbours to be used; for categorical variables 
#'        the mode of the neighbours is used, for continuous variables 
#'        the median value is used instead. Default: 10.
#' @param cat.var vector containing the indices of the variables to be 
#'        considered as categorical. Default: all variables.
#' @param to.impute vector indicating which rows of the dataset are to be imputed. 
#'        Default: impute all rows.
#' @param using vector indicating which rows of the dataset are to be used to search for neighbours.
#'        Automatically set to include also the rows to be imputed. Default: use all rows.
#'      
#' @return imputed data frame.
#' 
#' @export knn.impute
knn.impute <- function( data, k = 10, cat.var = 1:ncol(data), 
	to.impute = 1:nrow(data), using = 1:nrow(data) )
{
  n.var <- dim(data)[2]
  num.var <- setdiff(1:n.var,cat.var)
  using <- union(using, to.impute) # forced to include the rows to be imputed 
  
  imp.data <- data[to.impute,,drop=FALSE]  # retain dimensions even for one row
  use.data <- data[using,]
  storage.mode(imp.data) <- "double"
  storage.mode(use.data) <- "double"
  
  num.var.max <- apply(use.data[,num.var],2,max,na.rm=TRUE)
  num.var.min <- apply(use.data[,num.var],2,min,na.rm=TRUE)
  num.var.range <- num.var.max - num.var.min
  
  na.cases <- which(rowSums(is.na(imp.data)) > 0)
   
  neigh <- rep(0,k)
  storage.mode(num.var) <- "integer"
  
  for( i in na.cases )
  {
    d <- .Call( "heom_dist", imp.data[i,], use.data, num.var, num.var.range, PACKAGE = "bnstruct" )
    s <- sort(d, index.return=TRUE)$ix
    for( j in which(is.na(imp.data[i,])) )
    {
      # find the k closest neighbours with nonmissing value for j
      ind.neigh <- 1
      ind.s <- 2 # the first element is the case itself
      while( ind.neigh < k + 1)
      {
        if( !is.na(use.data[s[ind.s],j]) )
        {
          neigh[ind.neigh] <- use.data[s[ind.s],j]
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
