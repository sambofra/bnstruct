#' Constructor method of \code{\link{BN}} class.
#'
#' @name BN
#' @rdname BN-class
setMethod("initialize",
          "BN",
          function(.Object, dataset = NULL,
                   algo = "mmhc", alpha = 0.05, ess = 1, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset), cont.nodes = c(), raw.data = FALSE, ...)
          {
            if (!is.null(dataset))
            {
              name(.Object)         <- name(dataset)
              num.nodes(.Object)    <- num.variables(dataset)
              variables(.Object)    <- variables(dataset)
              node.sizes(.Object)   <- node.sizes(dataset)
              discreteness(.Object) <- discreteness(dataset)
              validObject(.Object)

              .Object <- learn.structure(.Object, dataset, algo = algo, alpha = alpha, ess = ess, bootstrap = bootstrap,
                                         layering = layering, max.fanin.layers = max.fanin.layers,
                                         max.fanin = max.fanin, cont.nodes = cont.nodes, raw.data = raw.data)
              
              validObject(.Object)

              .Object <- learn.params(.Object, dataset, ess = ess)
            }
            validObject(.Object)
            .Object
          })

#' Wrapper for \code{\link{BN}} object
#' 
#' @name BN
#' @rdname BN-class
#' @export
BN <- function(dataset = NULL, algo = "mmhc", alpha = 0.05, ess = 1, bootstrap = FALSE,
               layering = c(), max.fanin.layers = NULL,
               max.fanin = num.variables(dataset), cont.nodes = c(), raw.data = FALSE, ...)
{
  object <- new("BN", dataset = dataset, algo = algo, alpha = alpha, ess = ess, bootstrap = bootstrap,
                layering = layering, max.fanin.layers = max.fanin.layers,
                max.fanin = max.fanin, cont.nodes = cont.nodes, raw.data = raw.data, ...)
  object
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
              return (retval)
            }
)

# getters and setters

#' @rdname accessors-methods
#' @aliases name
setMethod("name", "BN", function(x) { slot(x, "name") } )


#' @rdname accessors-methods
#' @aliases num.nodes
setMethod("num.nodes", "BN", function(x) { slot(x, "num.nodes") } )


#' @rdname accessors-methods
#' @aliases variables
setMethod("variables", "BN", function(x) { slot(x, "variables") } )


#' @rdname accessors-methods
#' @aliases discreteness
setMethod("discreteness",
          "BN",
          function(x)
          {
            vs  <- slot(x, "discreteness")
            nvs <- rep('c', length(vs))
            nvs[which(vs == TRUE)] <- 'd'
            nvs
          })


#' @rdname accessors-methods
#' @aliases node.sizes
setMethod("node.sizes", "BN", function(x) { slot(x, "node.sizes") } )


#' @rdname accessors-methods
#' @aliases cpts
setMethod("cpts", "BN", function(x) { slot(x, "cpts") } )


#' @rdname accessors-methods
#' @aliases dag
setMethod("dag", "BN", function(x) { slot(x, "dag") } )


#' @rdname accessors-methods
#' @aliases wpdag
setMethod("wpdag", "BN", function(x) { slot(x, "wpdag") } )


# @name name
# @rdname mutators-methods
# @aliases name
setReplaceMethod("name",
                 signature(x="BN", value="character"),
                 function(x, value)
                 {
                   slot(x, "name") <- value
                   validObject(x)
                   x
                 })


# @name num.nodes
# @rdname mutators-methods
# @aliases num.nodes
setReplaceMethod("num.nodes",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "num.nodes") <- value
                   validObject(x)
                   x
                 })


# @name variables
# @rdname mutators-methods
# @aliases variables
setReplaceMethod("variables",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "variables")  <- value
                   num.nodes(x) <- length(value)
                   validObject(x)
                   x
                 })


# @name discreteness
# @rdname mutators-methods
# @aliases discreteness
setReplaceMethod("discreteness",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "discreteness") <- sapply(1:length(value), FUN=function(i){ !is.na(match(value[i],c('d',"D"))) })
                   validObject(x)
                   x
                 })


# @name node.sizes
# @aliases node.sizes
# @rdname mutators-methods
setReplaceMethod("node.sizes",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "node.sizes") <- value
                   validObject(x)
                   x
                 })


# @name cpts
# @aliases cpts
# @rdname mutators-methods
setReplaceMethod("cpts",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "cpts") <- value
                   validObject(x)
                   x
                 })


# @name dag
# @aliases dag
# @rdname mutators-methods
setReplaceMethod("dag",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "dag") <- value
                   validObject(x)
                   x
                 })


# @name wpdag
# @alises wpdag
# @rdname mutators-methods
setReplaceMethod("wpdag",
                 "BN",
                 function(x, value)
                 {
                   slot(x, "wpdag") <- value
                   validObject(x)
                   x
                 })


#' @rdname get.most.probable.values-methods
#' @aliases get.most.probable.values
setMethod("get.most.probable.values",
          "BN",
          function(x, ...)
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
            return(mpv)
          })

# redefition of print() for BN objects
#' @rdname print-methods
#' @aliases print.BN
setMethod("print.BN",
          "BN",
          function(x, ...)
          {
            str <- "\nBayesian Network "
            str <- paste(str, name(x), sep = '')
            str <- paste(str, " with ", sep = '')
            str <- paste(str, num.nodes(x), sep = '')
            str <- paste(str, " nodes\n", sep = '')
            str <- paste(str, paste(variables(x), sep=" ", collapse=', '))
            cat(str)
            
            if (num.nodes(x) > 0 && length(dag(x)) > 1)
            {
              colnames(dag(x)) <- variables(x)
              rownames(dag(x)) <- variables(x)
              
              cat('\nAdjacency matrix:')
              print(dag(x))
              
              cat("\nConditional probability tables:")
              print(cpts(x))
              
            }
            
          })

# plot adjacency matrix
#' @rdname plot-methods
#' @aliases plot.BN,BN-methods
setMethod("plot.BN",
          c("BN"),
          # Plot a weighted connectivity matrix using Rgraphviz
          function( x, use.node.names = TRUE, frac = 0.2, 
                    max.weight = max(dag(x)), node.col = rep('white',ncol(dag(x))),
                    plot.wpdag = FALSE)
          {
            
            # check for Rgraphviz
            if (!require(Rgraphviz))
              stop("this function requires the Rgraphviz package.")
            
            # adjacency matrix
            if (plot.wpdag)
              mat <- wpdag(x)
            else
              mat <- dag(x)
            
            num.nodes <- num.nodes(x)
            variables <- variables(x)
            
            mat.th <- mat
            mat.th[mat <  frac*max.weight] <- 0
            mat.th[mat >= frac*max.weight] <- 1
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
  return(abs(label.edges(dag(object), layering)))
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
