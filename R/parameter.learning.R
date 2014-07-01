multinomial.mle <- function(train.data, node.sizes, graph.structure)
{
  # Parameter estimation from given data using Maximum Likelihood Estimation
  # for multinomial variables. Page 86 of my notes.
  
  # create empty lists of parameters, marginals, dimensions
  params <- NULL
  marginals <- NULL
  dimensions <- NULL
  dimnames <- NULL
  
  num.nodes <- length(node.sizes)
  db.size <- nrow(train.data)
  
  for (i in 1:num.nodes)
  {
    nvals <- node.sizes[i]
    # dimensions: 1 for values of variable i, + 1 for each parent
    parents <- which(graph.structure[,i] > 0)
    dimensions[[i]] <- 1 + length(parents)
    
    # compute total number of elements in matrix, and dimensions of matrix
    # maybe can be computed also in the recursive function, to do later (?)
    nvals <- node.sizes[i]
    dims <- c(nvals)
    dnames <- c(i)
    for (parent in parents)
    {
      nvals <- nvals * node.sizes[parent]
      dims <- c(dims, node.sizes[parent])
      dnames <- c(dnames, parent)
    }
    names(dims) <- dnames
    dimnames[[i]] <- as.list(dnames)
    
    # Call recursive method
    # Get back joint probability tables and marginals for the current variable
    # values have to be normalized in order to become probabilities from counts
    nodes.eval <- c(i, unlist(parents))
    out.mle <- rec.est(c(), nodes.eval, node.sizes, train.data)
    counts <- out.mle$jointdist / db.size
    params[[i]] <- array(counts, dim = dims, dimnames = dnames)
    params[[i]] <- aperm(params[[i]], c(length(dims):1))
    marginals[[i]] <- out.mle$marginals / db.size
    names(marginals[[i]]) <- c(1:node.sizes[i])
    
    # print(counts)
    
    # print(sum(params[[i]]) == 1)
    
    # jp.marginalization(params[[i]], dimnames[[i]], 2, node.sizes)
  }
  
  #print(params)
  #print(marginals)
  
  return(list("jpts" = params, "marginals" = marginals, "dimnames" = dimnames))
  #return(params)
}

multinomial.map <- function(train.data, node.sizes, graph.structure, ess = 1)
{
  # Parameter estimation from given data using Bayesian (Dirichlet) Estimation.
  
  # create empty lists of parameters, marginals, dimensions
  params <- NULL
  marginals <- NULL
  dimensions <- NULL
  dimnames <- NULL
  
  num.nodes <- length(node.sizes)
  db.size <- nrow(train.data)
  
  for (i in 1:num.nodes)
  {
    nvals <- node.sizes[i]
    # dimensions: 1 for values of variable i, + 1 for each parent
    parents <- which(graph.structure[,i] > 0)
    dimensions[[i]] <- 1 + length(parents)
    
    # compute total number of elements in matrix, and dimensions of matrix
    # maybe can be computed also in the recursive function, to do later (?)
    nvals <- node.sizes[i]
    dims <- c(nvals)
    dnames <- c(paste(i))
    for (parent in parents)
    {
      nvals <- nvals * node.sizes[parent]
      dims <- c(dims, node.sizes[parent])
      dnames <- c(dnames, paste(parent))
    }
    names(dims) <- dnames
    dimnames[[i]] <- as.list(unlist(dnames))
    print("dnames")
    print(dnames)
    
    # Call recursive method
    # Get back joint probability tables and marginals for the current variable
    # values have to be normalized in order to become probabilities from counts
    nodes.eval <- c(i, unlist(parents))
    out.map <- rec.est(c(), nodes.eval, node.sizes, train.data)
    counts <- (out.map$jointdist + ess/length(out.map$jointdist)) / (db.size + ess)
    p <- array(counts, dim = dims, dimnames = c(dnames))
    str(p)
    str(dnames)
    print(length(dim(p)) == length(dnames))
    print("..")
    print(dim(p))
    #dimnames(p) <- as.list(dnames)
    print(names(dim(p)))
    params[[i]] <- p
    params[[i]] <- aperm(params[[i]], c(length(dims):1))
    marginals[[i]] <- (out.map$marginals + ess/length(out.map$marginals)) / (db.size + ess)
    names(marginals[[i]]) <- c(1:node.sizes[i])
    
    print(dimnames(params[[i]]))
    
    # print(sum(params[[i]]) == 1) # TRUE, even if printing tables does not say so...
    
  }
  
  #print(params)
  #print(marginals)
  
  return(list("jpts" = params, "marginals" = marginals, "dimnames" = dimnames))
  #return(params)
}

rec.est <- function(list.so.far, nodes.eval, node.sizes, train.data, step = 1)
{
  # recursive step: takes the previously computed list, and compute joint
  # probability with respect to the next unprocessed variable. First call
  # computed the marginal distribution for the variable. In successive calls,
  # each value from list.so.far is replaced with the corresponding joint prob.
  # e.g.: in a structure like A->C<-B, first call computes P(C), second call
  # computes P(AC), and finally P(ABC).
  # Output is: a) list of joint probability values in column major,
  # b) list of marginal probabilities.
  
  #nvals <- length(list.so.far)
  new.list <- c()
  for (i in 1:node.sizes[nodes.eval[1]])
  {
    #print(paste(paste(rep(" ", step), nodes.eval[1]), "=", i))
    new.list <- c(new.list, length(which(train.data[,nodes.eval[1]] == i)))
  }
  
  # the list we got in the first step is the marginal distribution
  # for the variable
  if (step == 1)
  {
    marginals <- new.list
  }
  
  if (length(nodes.eval) > 1)
  {
    ret.list <- c()
    list.by.row <- NULL
    # recursive call on relevant subset of database,
    # i.e. the rows which match the currently evaluated node values
    for (i in 1:node.sizes[nodes.eval[1]])
    {
      td <- subset(train.data, train.data[,nodes.eval[1]] == i)
      list.by.row[[i]] <- rec.est(new.list,
                                  nodes.eval[-1],
                                  node.sizes,
                                  td,
                                  step = step + 1)
    }
    
    # merge: build a matrix and transpose it
    # e.g.: I have (1 2 3)(4 5 6)(7 8 9)
    # I want: (1 4 7 2 5 8 3 6 9)
    ret.list <- t(array(data = c(unlist(list.by.row)),
                        dim = c(length(list.by.row[[1]]),
                                node.sizes[nodes.eval[1]]
                        )
    )
    )
    
    out.list <- array(ret.list)
  }
  else
  {
    ret.list <- t(array(data = c(unlist(new.list)),
                        dim = c(length(new.list[[1]]),
                                node.sizes[nodes.eval[1]]
                        )
    )
    )
    out.list <- array(ret.list)
    #out.list <- array(c(unlist(new.list)))
  }
  
  if (step == 1)
  {
    return(list("jointdist" = out.list, "marginals" = marginals))
  }
  else
  {
    return("jointdist" = out.list)
  }
}