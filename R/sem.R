#' @rdname sem
#' @aliases sem,InferenceEngine,BNDataset
setMethod("sem",
          c("InferenceEngine","BNDataset"),
          function(x, dataset, struct.threshold = 10, param.threshold = 0.001, k.impute = 10,
                   algo = "mmhc", scoring.func = "BIC",
                   alpha = 0.05, ess = 1, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset), cont.nodes = c(), raw.data = FALSE,
                   num.boots = 100, imputation = TRUE, na.string.symbol='?',
                   seed = 0, ...)
          {
            if(test.updated.bn(x))
              net <- updated.bn(x)
            else
              net <- bn(x)
            
            if (missing(algo))
              struct.algo  <- struct.algo(net)
            else
              struct.algo  <- algo
            
            if (missing(scoring.func))
              scoring.func <- scoring.func(net)
            else
              scoring.func <- scoring.func

            w.net     <- net
            w.dataset <- dataset
            w.eng     <- InferenceEngine(net) #x
            
            if (scoring.func == "BDeu")
            {
              stop("BDeu scoring function currently not supported for SEM algorithm.")
            }
            
            if (scoring.func == "AIC" || scoring.func == "BIC")
            {
              repeat
              {
                #w.eng     <- InferenceEngine(w.net)
                
                out <- em(w.eng, dataset, param.threshold, k.impute, ...)
                
                new.eng     <- out$InferenceEngine
                new.dataset <- out$BNDataset
                
                new.net <- learn.structure(updated.bn(new.eng), new.dataset, struct.algo, scoring.func,
                                           alpha = alpha, ess = ess, bootstrap = bootstrap,
                                           layering = layering, max.fanin.layers = max.fanin.layers,
                                           max.fanin = max.fanin, cont.nodes = cont.nodes, raw.data = raw.data,
                                           num.boots = num.boots, imputation = imputation, na.string.symbol=na.string.symbol,
                                           seed = seed, ...)
                
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
