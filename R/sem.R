#' @rdname sem
#' @aliases sem,InferenceEngine,BNDataset
setMethod("sem",
          c("InferenceEngine","BNDataset"),
          function(x, dataset, struct.threshold = 5, param.threshold = 0.001, k.impute = 10, scoring.func = "BIC",
                   alpha = 0.05, ess = 1, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset), cont.nodes = c(), use.imputed.data = FALSE,
                   num.boots = 100, imputation = TRUE, na.string.symbol='?',
                   seed = 0, ...)
          {
            if(test.updated.bn(x))
              net <- updated.bn(x)
            else
              net <- bn(x)
            
            # if (missing(algo))
            #   struct.algo  <- struct.algo(net)
            # else
            #   struct.algo  <- algo
            
            # if (missing(scoring.func))
            #   scoring.func <- scoring.func(net)
            # else
            #   scoring.func <- scoring.func

            if (sum(dag(net)) == 0)
            {
              # mmhc
              scoring.func <- match(tolower(scoring.func), c("bdeu", "aic", "bic"))
              if (is.na(scoring.func))
              {
                message("scoring function not recognized, using BDeu")
                scoring.func <- 0
              }
              else {
                scoring.func <- scoring.func - 1
              }
              # scoring.func(bn) <- c("BDeu", "AIC", "BIC")[scoring.func + 1]
              w.net <- net
              cpc     <- mmpc( raw.data(dataset), node.sizes(w.net), cont.nodes, alpha, layering, layer.struct=c() )
              dag(w.net) <- hc( raw.data(dataset), node.sizes(w.net), scoring.func, cpc, cont.nodes )
            }
            else
              w.net     <- net
            
            print(w.net)
            print(dag(w.net))
            readLines(file("stdin"),1)
            
            w.dataset <- dataset
            w.eng     <- InferenceEngine(w.net) #x
            
            if (scoring.func == "BDeu")
            {
              stop("BDeu scoring function currently not supported for SEM algorithm.")
            }
            
            if (scoring.func == 2 || scoring.func == 3)
            {
              repeat
              {
                #w.eng     <- InferenceEngine(w.net)
                out <- em(w.eng, dataset, param.threshold, k.impute, ...)
                
                new.eng     <- out$InferenceEngine
                new.dataset <- out$BNDataset
                
                new.net <- learn.structure(updated.bn(new.eng), new.dataset, "mmhc", c("bdeu", "aic", "bic")[scoring.func],
                                           alpha = alpha, ess = ess, bootstrap = bootstrap,
                                           layering = layering, max.fanin.layers = max.fanin.layers,
                                           max.fanin = max.fanin, cont.nodes = cont.nodes,
                                           use.imputed.data = use.imputed.data, ...)
                
                difference <- shd(dag(w.net), dag(new.net))
                print(difference)
                if (difference <= struct.threshold) break
                
                w.net     <- new.net
                w.dataset <- new.dataset
                w.eng     <- InferenceEngine(w.net)
                # w.eng     <- belief.propagation(w.eng)
              }
              
              updated.bn(w.eng) <- new.net
            }
            
            return(list("InferenceEngine" = w.eng, "BNDataset" = w.dataset))
          })
