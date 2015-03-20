#' @name BN
#' @rdname BN-class
#' @aliases initialize,BN-method
#' 
#' @param .Object a BN
#' 
setMethod("initialize",
          "BN",
          function(.Object, dataset = NULL, variables = c(), node.sizes = c(), discreteness = c())
          {
            x <- .Object
            
            if (!is.null(dataset))
            {
              name(x)         <- name(dataset)
              num.nodes(x)    <- num.variables(dataset)
              variables(x)    <- variables(dataset)
              node.sizes(x)   <- node.sizes(dataset)
              discreteness(x) <- discreteness(dataset)
              dag(x)          <- matrix(rep(0, num.nodes(x)*num.nodes(x)), nrow=num.nodes(x), ncol=num.nodes(x))
              wpdag(x)        <- matrix(rep(0, num.nodes(x)*num.nodes(x)), nrow=num.nodes(x), ncol=num.nodes(x))
              validObject(x)
              return(x)
            }
            
            if (!is.null(variables) && !is.null(node.sizes) && !is.null(discreteness))
            {
              name(x)         <- name(dataset)
              num.nodes(x)    <- num.variables(dataset)
              variables(x)    <- variables(dataset)
              node.sizes(x)   <- node.sizes(dataset)
              discreteness(x) <- discreteness(dataset)
              dag(x)          <- matrix(rep(0, num.nodes(x)*num.nodes(x)), nrow=num.nodes(x), ncol=num.nodes(x))
              wpdag(x)        <- matrix(rep(0, num.nodes(x)*num.nodes(x)), nrow=num.nodes(x), ncol=num.nodes(x))
              validObject(x)
              return(x)
            }
            
            validObject(x)
            return(x)
          })


#' constructor for a Bayesian Network.
#' 
#' Instantiate a \code{\link{BN}} object.
#' 
#' The constructor may be invoked without parameters -- in this case an empty network will be created, and its slots will be filled manually by the user.
#' This is usually viable only if the user already has knowledge about the network structure.
#' 
# Often, a better choice is to build a network starting from a dataset. Currently, two algorithms are supported for the structure learning step
# (can be specified using the \code{algo} option): \code{'sm'}, the Silander-Myllymaki exact algorithm,
# and \code{'mmhc'}, the Max-Min Hill-Climbing heuristic algorithm (default).
#  The Silander-Myllymaki algorithm can take a very long time, and it is not feasible for networks of more than 20-30 nodes.
# It is strongly recommended that valid \code{layering}, \code{max.fanin.layers} and \code{max.fanin} parameters are passed
# to the method if \code{algo = 'sm'} is given as parameter to the method.
#' 
# The parameter learning step is done using a Maximum-A-Posteriori (MAP) estimation.
#' 
#' @name BN
#' @rdname BN-class
#'
#' @param dataset a \code{\link{BNDataset}} object containing the dataset the network is built upon, if any. The remaining parameters
#'        are considered only if a starting dataset is provided.
#' 
#' @return BN object.
#' 
#' @examples
#' \dontrun{
#' net.1 <- BN()
#' 
#' dataset <- BNDataset()
#' dataset <- read.dataset(dataset, "file.header", "file.data")
#' net.2 <- BN(dataset)
#' }
#' 
#' 
#' @export
BN <- function(dataset = NULL, variables = c(), node.sizes = c(), discreteness = c())
{
  object <- new("BN", dataset = dataset, variables = c(), node.sizes = c(), discreteness = c())
  return(object)
}

# validator
setValidity("BN",
            function(object)
            {
              retval <- NULL
              if (num.nodes(object) > 0 && length(variables(object)) > 1 && length(variables(object)) != num.nodes(object))
              {
                retval <- c(retval, "incoherent number of variable names")
              }
              if (num.nodes(object) > 0 && length(dag(object)) > 1 &&
                  (ncol(dag(object)) != num.nodes(object) ||
                   nrow(dag(object)) != num.nodes(object)   ))
              {
                retval <- c(retval, "incoherent number of variables in DAG")
              }
              if (num.nodes(object) > 0 && length(wpdag(object)) > 1 &&
                  (ncol(wpdag(object)) != num.nodes(object) ||
                   nrow(wpdag(object)) != num.nodes(object)   ))
              {
                retval <- c(retval, "incoherent number of variables in WPDAG")
              }
              if(num.nodes(object) > 0 && length(discreteness(object)) > 1 &&
                 length(discreteness(object)) != num.nodes(object))
              {
                retval <- c(retval, "incoherent number of variable statuses")
              }
              
              if (is.null(retval)) return (TRUE)
              return(retval)
            }
)

# getters and setters

#' @aliases name,BN
#' @rdname name
setMethod("name", "BN", function(x) { return(slot(x, "name")) } )

#' @aliases num.nodes,BN
#' @rdname num.nodes
setMethod("num.nodes", "BN", function(x) { return(slot(x, "num.nodes")) } )

#' @aliases variables,BN
#' @rdname variables
setMethod("variables", "BN", function(x) { return(slot(x, "variables")) } )

#' @aliases discreteness,BN
#' @rdname discreteness
setMethod("discreteness",
          "BN",
          function(x)
          {
            return(slot(x, "discreteness"))
          })

#' @aliases node.sizes,BN
#' @rdname node.sizes
setMethod("node.sizes", "BN", function(x) { return(slot(x, "node.sizes")) } )

#' @aliases cpts,BN
#' @rdname cpts
setMethod("cpts", "BN", function(x) { return(slot(x, "cpts")) } )

#' @aliases dag,BN
#' @rdname dag
setMethod("dag", "BN", function(x) { return(slot(x, "dag")) } )

#' @aliases wpdag,BN
#' @rdname wpdag
setMethod("wpdag", "BN", function(x) { return(slot(x, "wpdag")) } )

#' @aliases scoring.func,BN
#' @rdname scoring.func
setMethod("scoring.func", "BN", function(x) { return(slot(x, "scoring.func")) } )

#' @aliases struct.algo,BN
#' @rdname struct.algo
setMethod("struct.algo", "BN", function(x) { return(slot(x, "struct.algo") ) } )

#' @name name<-
#' @aliases name<-,BN-method
#' @docType methods
#' @rdname name-set
setReplaceMethod("name",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "name") <- value
                   validObject(x)
                   return(x)
                 })

#' @name num.nodes<-
#' @aliases num.nodes<-,BN-method
#' @docType methods
#' @rdname num.nodes-set
setReplaceMethod("num.nodes",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "num.nodes") <- value
                   validObject(x)
                   return(x)
                 })


#' @name variables<-
#' @aliases variables<-,BN-method
#' @docType methods
#' @rdname variables-set
setReplaceMethod("variables",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "variables")  <- value
                   num.nodes(x) <- length(value)
                   validObject(x)
                   return(x)
                 })


#' @name discreteness<-
#' @aliases discreteness<-,BN-method
#' @docType methods
#' @rdname discreteness-set
setReplaceMethod("discreteness",
                 "BN",
                 function(x, value)
                 {
                   if (class(value) == "character")
                    slot(x, "discreteness") <- sapply(1:length(value), FUN=function(i){ !is.na(match(value[i],c('d',"D"))) })
                   else # is logical
                     slot(x, "discreteness") <- value
                   validObject(x)
                   return(x)
                 })


#' @name node.sizes<-
#' @aliases node.sizes<-,BN-method
#' @docType methods
#' @rdname node.sizes-set
setReplaceMethod("node.sizes",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "node.sizes") <- value
                   validObject(x)
                   return(x)
                 })


#' @name cpts<-
#' @aliases cpts<-,BN-method
#' @docType methods
#' @rdname cpts-set
setReplaceMethod("cpts",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "cpts") <- value
                   validObject(x)
                   return(x)
                 })


#' @name dag<-
#' @aliases dag<-,BN-method
#' @docType methods
#' @rdname dag-set
setReplaceMethod("dag",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "dag") <- value
                   validObject(x)
                   return(x)
                 })


#' @name wpdag<-
#' @aliases wpdag<-,BN-method
#' @docType methods
#' @rdname wpdag-set
setReplaceMethod("wpdag",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "wpdag") <- value
                   validObject(x)
                   return(x)
                 })


#' @name scoring.func<-
#' @aliases scoring.func<-,BN-method
#' @docType methods
#' @rdname scoring.func-set
setReplaceMethod("scoring.func",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "scoring.func") <- value
                   return(x)
                 })


#' @name struct.algo<-
#' @aliases struct.algo<-,BN-method
#' @docType methods
#' @rdname struct.algo-set
setReplaceMethod("struct.algo",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "struct.algo") <- value
                   return(x)
                 })


#' @aliases layering,BN
#' @rdname layering
setMethod("layering",
          "BN",
          function(x)
          {
            layers <- topological.sort(dag(x))
            layers <- array(layers, dimnames = variables(x))
            return(layers)
          })


#' @aliases get.most.probable.values,BN
#' @rdname get.most.probable.values
setMethod("get.most.probable.values",
          "BN",
          function(x)
          {
            bn   <- x
            dag  <- dag(bn)
            cpts <- cpts(bn)
            num.nodes <- num.nodes(bn)
            variables <- variables(bn)
            node.sizes <- node.sizes(bn)

            mpv  <- array(rep(0,num.nodes), dim=c(num.nodes), dimnames=list(variables))

            sorted.nodes <- topological.sort(dag)
            
            dim.vars   <- lapply(1:num.nodes,
                                 function(x)
                                   as.list(
                                     match(
                                       c(unlist(
                                         names(dimnames(cpts[[x]]))
                                       )),
                                       c(variables)
                                     )
                                   )
            )
            
            
            for (node in sorted.nodes)
            {
              pot  <- cpts[[node]]
              vars <- c(unlist(dim.vars[[node]]))

              # sum out parent variables
              if (length(dim.vars[[node]]) > 1)
              {
                # find the dimensions corresponding to the current variable
                for (parent in setdiff(vars, node))
                {
                  out  <- marginalize(pot, vars, parent)
                  pot  <- out$potential
                  vars <- out$vars
                  pot <- pot / sum(pot)
                }
              }
              
              # print(pot)
              wm <- which(!is.na(match(c(pot),max(pot))))
              if (length(wm) == 1)
              {
                mpv[node] <- wm # pot[wm]
              }
              else
              {
                mpv[node] <- sample(1:node.sizes[node], 1, replace=TRUE, prob=pot)
              }
              
              # propagate information from parent nodes to children
              children <- which(dag[node,] > 0)
              if (length(children) > 0)
              {
                for (child in children)
                {
                  out <- mult(cpts[[child]], dim.vars[[child]],
                              pot, c(node),
                              node.sizes)
                  cpts[[child]]     <- out$potential
                  dim.vars[[child]] <- out$vars
                }
              }
            }
            return(mpv)
          })


# # ' @rdname query
# # ' @aliases query,BN
# setMethod("query",
#           "BN",
#           function(x, observed.vars, observed.vals)
#           {
# #             obs <- unique.observations(observed.vars, observed.vals)
# #             observed.vars <- obs$observed.vars
# #             observed.vals <- obs$observed.vals
# #             cpts <- cpts(x)
# #             cpts <- lapply(1:length(cpts), function(x) {
# #                               if (!match(x, observed.vars)){}
# #                             })
#           })


# redefition of print() for BN objects
#' @rdname print
#' @aliases print,BN
setMethod("print",
          "BN",
          function(x, ...)
          {
            str <- "\nBayesian Network: "
            str <- paste(str, name(x), sep = '')
            str <- paste(str, "\n", sep = '')
            cat(str)
            str <- "\nnum.nodes "
            str <- paste(str, num.nodes(x), sep = '')
            str <- paste(str, "\n", sep = '')
            cat(str)
            str <- "\nvariables\n"
            cat(str)
            cat(variables(x))
            str <- "\ndiscreteness\n"
            cat(str)
            cat(discreteness(x))
            str <- "\nnode.sizes\n"
            cat(str)
            cat(node.sizes(x))
            
            if (num.nodes(x) > 0 && (is.element(1,dag(x)) || length(which(wpdag(x) != 0)) > 0))
            {
              
              if (is.element(1,dag(x)))
              {
                colnames(dag(x)) <- variables(x)
                rownames(dag(x)) <- variables(x)
                cat('\nAdjacency matrix:\n')
                print(dag(x))
              }
                
              if (length(which(wpdag(x) != 0)) > 0)
              {
                colnames(wpdag(x)) <- variables(x)
                rownames(wpdag(x)) <- variables(x)
                cat('\nWPDAG:\n')
                print(wpdag(x))
              }  
              
              cat("\nConditional probability tables:")
              print(cpts(x))
              
            }
            
            cat("\n")
            
          })


#' plot a \code{\link{BN}} as a picture.
#'
#' @param x a \code{\link{BN}} object.
#' @param ... potential further arguments for methods.
#' @param use.node.names \code{TRUE} if node names have to be printed. If \code{FALSE}, numbers are used instead.
#' @param frac fraction
#' @param max.weight max.weight
#' @param node.col list of (\code{R}) colors for the nodes.
#' @param plot.wpdag if \code{TRUE} plot the network according to the WPDAG computed using bootstrap instead of the DAG.
#' 
#' 
#' @name plot
#' @aliases plot,BN plot.BN,BN
#' @rdname plot
#' @export
plot.BN <- 
  function( x, ..., use.node.names = TRUE, frac = 0.2, 
                    max.weight = max(dag(x)), node.col = rep('white',ncol(dag(x))),
                    plot.wpdag = FALSE)
          {
            
            # check for Rgraphviz
            if (!require(Rgraphviz))
              stop("this function requires the Rgraphviz package.")
            
            # adjacency matrix
            if (plot.wpdag || (!is.element(1,dag(x)) && length(which(wpdag(x) != 0)) > 0))
              mat <- wpdag(x)
            else
              mat <- dag(x)
            
            if (plot.wpdag || (!is.element(1,dag(x)) && length(which(wpdag(x) != 0)) > 0))
            {
              if (missing(max.weight))
                max.weight <- max(mat)
              if (missing(node.col))
                node.col <- rep('white',ncol(mat))
            }
            
            num.nodes <- num.nodes(x)
            variables <- variables(x)
            
            mat.th <- mat
            if (is.element(1,dag(x)) || length(which(wpdag(x) != 0)) > 0)
            {
              mat.th[mat <  frac*max.weight] <- 0
              mat.th[mat >= frac*max.weight] <- 1
            }
            
            # node names
            if (use.node.names && length(variables) > 0)
              node.names <- variables
            else
              node.names <- as.character(1:num.nodes)
            # build graph
            g <- graphAM( mat.th, edgemode="directed")
            nodes(g) <- node.names
            en <- edgeNames(g,recipEdges="distinct")
            g <- layoutGraph(g)
            
            # set edge darkness proportional to confidence
            conf <- mat.th*pmax(mat,t(mat)) # both values to the maximum for edges with 2 directions
            col <- colors()[253-100*(t(conf)[t(conf) >= frac*max.weight]/max.weight)]
            names(col) <- en
            
            # remove arrowheads from undirected edges
            ahs <- edgeRenderInfo(g)$arrowhead
            ats <- edgeRenderInfo(g)$arrowtail
            dirs <- edgeRenderInfo(g)$direction
            ahs[dirs=="both"] <- ats[dirs=="both"] <- "none"
            edgeRenderInfo(g) <- list(col=col,lwd=2,arrowhead=ahs,arrowtail=ats)
            
            # node colors
            node.fill <- as.list(node.col)
            names(node.fill) <- node.names
            nodeRenderInfo(g) <- list(fill=node.fill)
            
            renderGraph(g)
          }

# save BN as eps file
#' @rdname save.to.eps
#' @aliases save.to.eps,BN,character
setMethod("save.to.eps",
          c("BN", "character"),
          function(x, filename)
          {
            # problem: I wanted to set filename=NULL in the declaration, but I cannot manage to
            # make it work in case of missing filename...
            
            # problem 2: cannot make dag.to.cpdag work...
            postscript(filename)
            plot(x)
            dev.off()
          })


#' @rdname sample.row
#' @aliases sample.row,BN
setMethod("sample.row", "BN",
          function(x){
            bn   <- x
            dag  <- dag(bn)
            cpts <- cpts(bn)
            num.nodes <- num.nodes(bn)
            variables <- variables(bn)
            node.sizes <- node.sizes(bn)
            
            copy <- cpts
            
            mpv  <- array(rep(0,num.nodes), dim=c(num.nodes), dimnames=list(variables))
            
            sorted.nodes <- topological.sort(dag)
            
            dim.vars   <- lapply(1:num.nodes,
                                 function(x)
                                   as.list(
                                     match(
                                       c(unlist(
                                         names(dimnames(cpts[[x]]))
                                       )),
                                       c(variables)
                                     )
                                   )
            )

            print("sorted.nodes")
            print(variables[sorted.nodes])
#             readLines(file("stdin"),1)
            
            for (node in sorted.nodes)
            {
              print("node")
              print(node)
              pot  <- cpts[[node]]
              vars <- c(unlist(dim.vars[[node]]))
              print(pot)
              print("((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((")
              print(copy[[node]])
              print("))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))")
              print(vars)
              # sum out parent variables
              if (length(c(unlist(dim.vars[[node]]))) > 1)
              {
                print("parents")
                print(setdiff(vars, node))
                parents <- setdiff(vars, node)
                for (parent in parents)
                  print(cpts[[parent]])
                print("-")
#                 readLines(file("stdin"),1)
                # find the dimensions corresponding to the current variable
#                 for (parent in parents)
#                 {
#                   print(parent)
#                   npot              <- rep(0, node.sizes[parent])
#                   npot[mpv[parent]] <- 1
#                   print("***************")
#                   print(pot)
#                   print("°°°°°°°°°°°°°°°")
#                   out <- mult(pot, vars,
#                               npot, c(parent),
#                               node.sizes)
#                   pot  <- out$potential
#                   vars <- out$vars
#                   print(pot)
#                   print(vars)
#                   print("###############")
#                   #               npot[mpv[node]] <- 1
# #                   out  <- marginalize(pot, vars, parent)
# #                   pot  <- out$potential
# #                   vars <- out$vars
#                   # pot <- pot / sum(pot)
#                 }
                remaining <- match(node, vars)
                pot  <- apply(pot, remaining, sum)
                vars <- vars[remaining]
              }
              else
              {
                print("no parents")
                print(pot)
              }
              
              # pot  <- pot / sum(pot)
              print(pot)
              
              mpv[node] <- sample(1:node.sizes[node], 1, replace=T, prob=pot)
              print(mpv[node])
              # propagate information from parent nodes to children
              children        <- which(dag[node,] > 0)
              npot            <- rep(0, node.sizes[node])
              npot[c(mpv[node])] <- 1
              cpts[[node]] <- npot
              dim.vars[[node]] <- c(node)
              print("node")
              print(variables[node])
              print("children")
              print(variables[children])
#               readLines(file("stdin"),1)
              if (length(children) > 0)
              {
                for (child in children)
                {
                  print("and now")
                  print(variables[child])
#                   readLines(file("stdin"),1)
                  print("prima")
                  print(cpts[[child]])
                  print(dim.vars[[child]])
                  out <- mult(cpts[[child]], dim.vars[[child]],
                              npot, c(node),
                              node.sizes)
                  cpts[[child]]     <- out$potential
                  dim.vars[[child]] <- out$vars
                  remaining <- c(1:length(dim.vars[[child]]))[-which(dim.vars[[child]] == node)]
                  cpts[[child]]  <- apply(cpts[[child]], remaining, sum)
                  dim.vars[[child]] <- dim.vars[[child]][remaining]
                  dag[node,child] <- 0
                  print("dopo")
                  print(cpts[[child]])
                  print(dim.vars[[child]])
                }
              }
            }
            return(mpv)
          })


#' @rdname sample.dataset
#' @aliases sample.dataset,BN
setMethod("sample.dataset",c("BN"),
          function(x, n = 100)
          {
            bnd <- BNDataset("")
            name(bnd)          <- name(x)
            variables(bnd)     <- variables(x)
            num.variables(bnd) <- num.nodes(x)
            discreteness(bnd)  <- discreteness(x)
            node.sizes(bnd)    <- node.sizes(x)
            num.items(bnd)     <- n
            
            obs <- matrix(rep(0, num.variables(bnd) * n), nrow = n, ncol = num.variables(bnd))
            
            for (i in 1:n)
            {
              obs[i,] <- sample.row(x)
            }
            
            storage.mode(obs) <- "integer"
            raw.data(bnd)     <- obs
            
            return(bnd)
          })


dag.to.cpdag <- function(object, layering = NULL)
{
  return(abs(label.edges(object, layering)))
}


label.edges <- function(dgraph, layering = NULL)
{
  # LABEL-EDGES produce a N*N matrix which values are
  # 	+1 if the edge is compelled or
  #	-1 if the edge is reversible.
  N<-nrow(dgraph)
  o <- order.edges(dgraph)
  order <- o$order
  xedge <- o$x
  yedge <- o$y
  
  label <- 2*dgraph
  NbEdges <- length(xedge)
  
  # edges between layers are compelled
  if( !is.null(layering) )
  {
    layers = length(unique(layering))
    for( l in 1:(layers-1) )
      label[ intersect(xedge,which(layering==l)), intersect(yedge,which(layering>l)) ] <- 
      dgraph[ intersect(xedge,which(layering==l)), intersect(yedge,which(layering>l)) ]
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

order.edges <- function(dgraph)
  # ORDER_EDGES produce a total (natural) ordering over the edges in a DAG.
{
  N <- nrow(dgraph)
  order <- matrix(c(0),N,N)
  
  node_order <- topological.sort(dgraph)
  oo <- sort(node_order,index.return=TRUE)$ix
  dgraph <- dgraph[oo,oo]
  xy <- which(dgraph == 1, arr.ind = TRUE)
  nb.edges <- nrow(xy)
  
  if( nb.edges != 0)
    order[xy] <- 1:nb.edges
  
  order <- order[node_order,node_order]
  x <- oo[xy[,1]]
  y <- oo[xy[,2]]
  
  return(list(order=order,x=x,y=y))
}

topological.sort <- function(dgraph)
  # TOPOLOGICAL_SORT Return the nodes in topological order (parents before children).
{
  n <- nrow(dgraph)
  
  # assign zero-indegree nodes to the top
  fringe <- which( colSums(dgraph)==0 )
  order <- rep(0,n)
  
  i <- 1
  while( length(fringe) > 0 )
  {
    print(fringe)
    ind <- head(fringe,1) # pop
    fringe <- tail(fringe,-1)
    order[i] <- ind
    i <- i + 1 
    
    for( j in which(dgraph[ind,] != 0) )
    {
      dgraph[ind,j] <- 0
      if( sum(dgraph[,j]) == 0 )
        fringe <- c(fringe,j)
    }
  }
  
  return(order)
}

