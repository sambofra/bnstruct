#' Constructor method of \code{\link{BN}} class.
#'
#' @name BN
#' @rdname BN-class
setMethod("initialize",
          "BN",
          function(.Object, ...)  
          {
            validObject(.Object)      
            .Object
          })

#' Wrapper for \code{\link{BN}} object
#' 
#' @name BN
#' @rdname BN-class
#' @export
BN <- function(...)
{
  object <- new("BN", ...)
  object
}

# validator
setValidity("BN",
            function(object)
            {
              retval <- NULL
              if (object@num.nodes > 0 && length(object@variables) > 1 && length(object@variables) != object@num.nodes)
              {
                retval <- c(retval, "incoherent number of variable names")
              }
              if (object@num.nodes > 0 && length(object@dag) > 1 &&
                  (ncol(object@dag) != object@num.nodes ||
                   nrow(object@dag) != object@num.nodes   ))
              {
                retval <- c(retval, "incoherent number of variables in DAG")
              }
              if (object@num.nodes > 0 && length(object@wpdag) > 1 &&
                  (ncol(object@wpdag) != object@num.nodes ||
                   nrow(object@wpdag) != object@num.nodes   ))
              {
                retval <- c(retval, "incoherent number of variables in WPDAG")
              }
              if(object@num.nodes > 0 && length(object@discreteness) > 1 &&
                 length(object@discreteness) != object@num.nodes)
              {
                retval <- c(retval, "incoherent number of variable statuses")
              }
              
              if (is.null(retval)) return (TRUE)
              return (retval)
            }
)

#' @rdname get.most.probable.values-methods
#' @aliases get.most.probable.values
setMethod("get.most.probable.values",
          "BN",
          function(bn, ...)
          {
            dag  <- bn@dag
            cpts <- bn@cpts
            num.nodes <- bn@num.nodes
            variables <- bn@variables
            node.sizes <- bn@node.sizes

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
                }
              }
              
              wm        <- which.max(pot)
              mpv[node] <- wm
              
              # propagate information from parent nodes to children
              children <- which(dag[node,] > 0)
              if (length(children) > 0)
              {
                for (child in children)
                {
                  out <- mult(pot, c(node),
                              cpts[[child]], dim.vars[[child]],
                              node.sizes)
                  cpts[[child]]     <- out$potential
                  dim.vars[[child]] <- out$vars
                }
              }
            }
            print(mpv)
          })

# redefition of print() for BN objects
#' @rdname print-methods
#' @aliases print.BN
setMethod("print.BN",
          "BN",
          function(x, ...)
          {
            str <- "\nBayesian Network "
            str <- paste(str, x@name, sep = '')
            str <- paste(str, " with ", sep = '')
            str <- paste(str, x@num.nodes, sep = '')
            str <- paste(str, " nodes\n", sep = '')
            str <- paste(str, paste(x@variables, sep=" ", collapse=', '))
            message(str)
            
            if (x@num.nodes > 0)
            {
              colnames(x@dag) <- x@variables
              rownames(x@dag) <- x@variables
              
              message('\nAdjacency matrix:')
              print(x@dag)
              
              message("\nConditional probability tables:")
              print(x@cpts)
              
            }
            
          })

# plot adjacency matrix
#' @rdname plot-methods
#' @aliases plot.BN,BN-methods
setMethod("plot.BN",
          c("BN"),
          # Plot a weighted connectivity matrix using Rgraphviz
          function( x, use.node.names = TRUE, frac = 0.2, 
                    max.weight = max(x@dag), node.col = rep('white',ncol(x@dag)),
                    plot.wpdag = FALSE)
          {
            object <- x
            
            # check for Rgraphviz
            if (!require(Rgraphviz))
              stop("this function requires the Rgraphviz package.")
            
            # adjacency matrix
            if (plot.wpdag)
              mat <- object@wpdag
            else
              mat <- object@dag
            
            mat.th <- mat
            mat.th[mat <  frac*max.weight] <- 0
            mat.th[mat >= frac*max.weight] <- 1
            # node names
            if (use.node.names && length(object@variables) > 0)
              node.names <- object@variables
            else
              node.names <- as.character(1:object@num.nodes)
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
          })

# save BN as eps file
#' @rdname save.to.eps-methods
#' @aliases save.to.eps
setMethod("save.to.eps",
          c("BN", "character"),
          function(object, filename)
          {
            # problem: I wanted to set filename=NULL in the declaration, but I cannot manage to
            # make it work in case of missing filename...
            
            # problem 2: cannot make dag.to.cpdag work...
            
            postscript(filename)
            #plot(dag.to.cpdag(object))
            plot(object)
            dev.off()
          })

dag.to.cpdag <- function(object, layering = NULL)
{
  return(abs(label.edges(object@dag, layering)))
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
