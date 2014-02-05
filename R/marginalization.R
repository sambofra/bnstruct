jp.marginalization <- function(jpt, dimnames, wrt, node.sizes)
{
  # Marginalization of a joint probability distribution
  # with respect to the variable wrt. Each dimension is associated to a node,
  # according to the dimnames vector.
  
  # get position of the variable to be marginalized in the array dimension list
  marg.dim <- which(dimnames == wrt)
  
  # get dimensions, compute dimensions for the soon-to-be-created prob. table
  # and number of the values that it will contain
  dims <- dim(jpt)
  new.dims <- c(dims[-marg.dim])
  new.num.vals <- prod(new.dims)
  length.of.run <- dims[marg.dim]

  # switch dimensions in the array:
  # dimension corresponding to the variable to be marginalized goes first
  new.order <- c(marg.dim, (1:length(dims))[-marg.dim])
  # remove marginalized dimension name
  new.order.names <- c(dimnames[-marg.dim])
  # switch dimensions, make jpt a linear array
  jpt <- aperm(jpt, new.order)
  marg <- array(jpt)
  # sum up #(length.of.run) contiguous values in the array
  marg <- tapply(marg, rep(1:new.num.vals, each=length.of.run), sum)  
  # apply new dimensions to resulting list (if needed)
  marg <- array(marg, new.dims, c(unlist(new.order.names)))
  
  return(marg)
}

jpt.to.cpt <- function(jpt, dimnames, wrt, marg, node.sizes)
{
  # construct CPT starting from a JPT, conditioning on variable wtr.
  # Needs marginals.
  
  # get position of the variable to be marginalized in the array dimension list
  marg.dim <- which(dimnames == wrt)
  
  dims <- dim(jpt)
  #num.runs <- prod(dims[-marg.dim])
  #length.of.run <- dims[marg.dim]
  
  # switch dimensions in the array:
  # dimension corresponding to the variable to be marginalized goes first
  new.order <- c(marg.dim, (1:length(dims))[-marg.dim])
  
  z <- rep(array(marg), 1)#num.runs)

  cpt <- jpt
  cpt <- aperm(cpt, new.order)
  cpt <- cpt / z
  cpt <- aperm(cpt, new.order)
  
  return(cpt)
}

construct.cpts <- function(jpts, marginals, graph.structure, dnames, node.sizes)
{
  # construct CPTs starting from JPTs, marginals
  
  cpts <- NULL
  for (i in 1:length(jpts))
  {
    cpt <- jpts[[i]]
    parents.indices <- which(graph.structure[,i] > 0)
    #iterate through parents, if any
    if (length(parents.indices) > 0)
    {
      # construct denominator by multiplying marginals
      parents <- unlist(parents.indices)
      dimnames <- dnames[[i]]
      den <- array(marginals[[parents[1]]], c(1, node.sizes[parents[1]]));
      print("PRINT DEN")
      print(den)
      if(length(parents) > 1)
        for (j in 2:length(parents))
        {
          m <- array(marginals[[parents[j]]], c(1, node.sizes[parents[j]]));
          print(m)
          den <- array(t(den) %*% m)
          print(den)
        }
        
      dims <- dim(cpt)
      
      print(i)
      print(str(den))
      print(str(cpt))
      den <- rep(den, length(array(cpt))/length(array(den)))
      #print(length(array(cpt))/length(array(den)) == 1)
      p.order <- c((length(dims)-1):1, length(dims))
      print(p.order)
      print(rev(p.order))
      #cpt <- array(aperm(cpt, p.order)) / den #array(aperm(array(den, dims), p.order))
      cpt <- array(cpt) / array(aperm(array(den, dims), p.order))
      dim(cpt) <- dims
      print("************************************************")
      print(rev(p.order))
      print("************************************************")
      cpt <- aperm(cpt, c(length(dims), (length(dims)-1):1))#rev(p.order))
    }
    cpts[[i]] <- cpt
  }
  return(cpts)
}

cp.marginalization <- function(cpt, dimnames, wrt, wrt.marg, node.sizes)
{
  # marginalization of a CPT with respect to the variable wrt.
  
  # get position of the variable to be marginalized in the array dimension list
  marg.dim <- which(dimnames == wrt)
  print(paste("marg.dim", marg.dim))
  
  # get dimensions, compute dimensions for the soon-to-be-created prob. table
  # and number of the values that it will contain
  dims <- dim(cpt)
  print(paste("dims", dims))
  new.dims <- c(dims[-marg.dim])
  print(paste("new.dims", new.dims))
  new.num.vals <- prod(new.dims)
  length.of.run <- dims[marg.dim]
  num.runs <- new.num.vals
  
  # switch dimensions in the array:
  # dimension corresponding to the variable to be marginalized goes first
  new.order <- c(marg.dim, (1:length(dims))[-marg.dim])
  print(paste("new.order", new.order))
  # remove marginalized dimension name
  new.order.names <- c(dimnames[-marg.dim])
  print(paste("no.names", new.order.names))
  # switch dimensions, make jpt a linear array
  jpt <- aperm(cpt, new.order)
  marg <- array(jpt)
  print("marg")
  print(marg)
  z <- rep(array(wrt.marg), num.runs)
  print("z")
  print(z)
  marg <- marg / z
  # sum up #(length.of.run) contiguous values in the array
  marg <- tapply(marg, rep(1:new.num.vals, each=length.of.run), sum)  
  print("marg")
  print(marg)
  # apply new dimensions to resulting list (if needed)
  marg <- array(marg, new.dims, c(unlist(new.order.names)))
  
  return(marg)
}

mult.cpt.marg <- function(cpt, dnames, marg.var, marg, node.sizes)
{
  marg.dim <- which(dnames == marg.var)
  
  print(cpt)
  
  # get dimensions, compute dimensions for the soon-to-be-created prob. table
  # and number of the values that it will contain
  dims <- dim(cpt)
  
  # switch dimensions in the array:
  # dimension corresponding to the variable to be marginalized goes first
  new.order <- c(marg.dim, (1:length(dims))[-marg.dim])
  jpt <- aperm(cpt, new.order)
  jpt <- array(jpt)
  z <- rep(array(marg), length(jpt)/node.sizes[marg.var])
  jpt <- jpt * z
  # sum up #(length.of.run) contiguous values in the array
  # apply new dimensions to resulting list (if needed)
  jpt <- array(jpt, dims)
  jpt <- aperm(jpt, new.order)
  
  jpt <- order.jpt.dims(jpt, dnames)
  return(jpt)
}

mult.cpt.jpt <- function(cpt, jpt, node.sizes)
{
  # compute something in the form P(C|A,B)xP(AB)
  dims.c <- dim(cpt)
  dims.j <- dim(jpt)
  rep.times <- prod(node.sizes[dims.c]) / prod(node.sizes[dims.j])
  res <- array(cpt)
  # res <- res * rep(array(jpt), rep.times)
  # lapply below here repeats each element contiguously, not the entire list
  res <- res * unlist(lapply(array(jpt), rep, rep.times))
  dim(res) <- dims.c
  return(res)
}

mult.jpt.jpt <- function(jpt1, dnames1, jpt2, dnames2, node.sizes)
{
  
}

mult.marg.marg <- function(v1, m1, v2, m2, node.sizes)
{
  jp <- m1 %*% t(m2)
  dn <- c(v1, v2)
  return(list("jpt" = jp, "dnames" = dn))
}

order.jpt.dims <- function(jpt, dnames)
{
  # sort dimensions in the jpt, according to variables in ascending order
  len <- length(dnames)
  
  if (len == 1)
    return(jpt)
  
  m <- array(c(1:len, dnames), c(len, 2))
  m <- m[order(m[,2]),]
  print(m)
  ord <- c(m[,1])
  jpt <- aperm(jpt, ord)
  return(jpt)
}
