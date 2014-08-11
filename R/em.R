#' @name em
#' @rdname em
#' @aliases em,InferenceEngine
setMethod("em",
          c("InferenceEngine","BNDataset"),
          function(x, dataset, threshold = 0.001, k.impute = 10)
          {
            # We assume:
            # 1) there is a BN with learnt or known parameters
            # 2) there is a raw dataset with missing values (raw complete data case is not treated)
            #
            # We do:
            # 1) belief propagation
            # 2) re-guess imputed data, substituting NAs with most probable values obtained with BP step above
            # 3) re-learn parameters for BN from imputed data from step 2
            # 4) if convergence condition is met (parameters vary by less than threshold) stop, otherwise go back to step 1
            
            # for step2 1,2:
            # - do bp
            # - get computed network
            # - get cpts of computed network
            # - for NA values:
            #   - select corresponding cpts
            #   - set parents to observed values
            #     - in case of parents with missing values: take variables in topological order
            #   - compute values according to distribution

            # raw.data   <- get.imputed.data(dataset)
            raw.data   <- get.raw.data(dataset)
            imputed.data <- get.imputed.data(dataset)
            orig.bn    <- bn(x)
            eng        <- x
            num.items  <- num.items(dataset)
            num.nodes  <- num.nodes(orig.bn)
            node.sizes <- node.sizes(orig.bn)
            var.names  <- variables(orig.bn)
            
            cliques    <- jt.cliques(x)
            
            # ndataset <- dataset
            bn       <- orig.bn
            bak.bn   <- bn

            observations(eng) <- list(NULL, NULL) # clean observations, if needed
            eng <- belief.propagation(eng)

            imp.data <- matrix(data=c(rep(0, prod(dim(raw.data)))), nrow=nrow(raw.data), ncol=ncol(raw.data))
            for (row in 1:num.items)
            {
              y <- raw.data[row,]
              print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
              print(row)
              print(y)
              #mpv <- as.vector(c(rep(0, num.nodes)))#, dim=c(num.nodes), dimnames=list(var.names))
              mpv <- rep(0, num.nodes)
              obsd.vars <- which(!is.na(y))
              obsd.vals <- y[obsd.vars]
              non.obsd.vars <- setdiff(1:num.nodes, obsd.vars)
              print(obsd.vars)
              print(non.obsd.vars)
              j <- jpts(eng)
              
              for (i in non.obsd.vars)
              {
                target.clique <- which.min(!is.na(sapply(cliques, function(cl) {match(i, c(unlist(cl)))})))
                jpt <- j[[target.clique]]
                d   <- c(match(names(dimnames(jpt)),var.names))
                if (length(intersect(d, obsd.vars)) > 0)
                {
                  print("intersect")
                  print(intersect(d, obsd.vars))
                  for (v in intersect(d, obsd.vars))
                  {
                    print(v)
                    print(obsd.vals)
                    print(jpt)
                    dmnms <- dimnames(jpt)
                    nms   <- names(dimnames(jpt))
                    cpt <- rep(0, node.sizes[v])
                    cpt[obsd.vals[which(obsd.vars == v)]] <- 1
                    print(cpt)
                    out <- mult(jpt, d, cpt, c(v), node.sizes)
                    jpt <- out$potential
                    d <- c(unlist(out$vars))
                    jpt <- jpt / sum(jpt)
                    dimnames(jpt) <- dmnms
                    names(dimnames(jpt)) <- nms
                    out <- marginalize(jpt, d, v)
                    jpt <- out$potential
                    d   <- c(unlist(out$vars))
                  }
                }
                wm <- which(!is.na(match(c(jpt),max(jpt))))
                if (length(wm) == 1)
                {
                  mpv[i] <- wm # jpt[wm]
                }
                else
                {
                  print("°°°")
                  print(nms)
                  print(j[[target.clique]])
                  print(jpt)
                  readLines(file("stdin"),1)
                  mpv[i] <- sample(wm,1) #,replace=TRUE
                }
              }
              
              for (i in 1:length(obsd.vars))
                mpv[obsd.vars[i]] <- obsd.vals[i]
              
              print(mpv)
              print(imputed.data[row,])
              #readLines(file("stdin"),1)
              # imp.data[row,] <- mpv
              for (k in 1:num.nodes)
                imp.data[row,k] <- mpv[k]
            }
            
            # imp.data <- as.matrix(imp.data)
            # colnames(imp.data) <- var.names
#             print(imp.data)
# print(dim(imp.data))
            imputed.data(dataset) <- imp.data
            # storage.mode(imp.data) <- "integer"
# print(dim(get.imputed.data(dataset)))
# print(dataset, show.imputed.data = TRUE)
            bn <- learn.params(bn, dataset)
            print(c(unlist(cpts(bn))) - c(unlist(cpts(orig.bn))))
          })
