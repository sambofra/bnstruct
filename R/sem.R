# ' @rdname sem
# ' @aliases sem,InferenceEngine,BNDataset
setMethod("sem",
          c("BN","BNDataset"),
          function(x, dataset, struct.threshold = params@sem_convergence,
                   param.threshold = params@em_convergence, scoring.func = params@scoring.func,
                   alpha = params@alpha, ess = params@ess, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset), cont.nodes = c(), use.imputed.data = FALSE,
                   use.cpc = TRUE, ..., params)
          {
            net <- x

            num.nodes <- num.nodes(net)

            if (is.character(scoring.func))
              scoring.func <- match(tolower(scoring.func), c("bdeu", "aic", "bic"))

            if (is.na(scoring.func))
            {
              message("scoring function not recognized, using BIC")
              scoring.func <- 2
            }
            else
              scoring.func <- scoring.func - 1
            # scoring.func(bn) <- c("BDeu", "AIC", "BIC")[scoring.func + 1]

            # starting from an empty network: learn a starting point using MMHC
            if (is.na(sum(dag(net))) || sum(dag(net)) == 0)
            {
              w.net <- net
              w.net <- learn.network(w.net, dataset, "mmhc", c("bdeu", "aic", "bic")[scoring.func+1],
                                     alpha=alpha, ess = ess, bootstrap = bootstrap,
                                     layering = layering, max.fanin.layers = max.fanin.layers,
                                     max.fanin = max.fanin, cont.nodes = cont.nodes,
                                     use.imputed.data = use.imputed.data, use.cpc = use.cpc, ..., params=params)
            }
            else
            {
              # start from an already learnt network
              w.net     <- net
            }
            
            w.dataset <- dataset
            w.eng     <- InferenceEngine(w.net)

            repeat
            {
              out <- em(w.eng, dataset, param.threshold, params=params)
              
              new.eng     <- out$InferenceEngine
              new.dataset <- out$BNDataset
              
              new.net <- learn.network(new.dataset, "mmhc", c("bdeu", "aic", "bic")[scoring.func+1],
                                         alpha = alpha, ess = ess, bootstrap = bootstrap,
                                         layering = layering, max.fanin.layers = max.fanin.layers,
                                         max.fanin = max.fanin, cont.nodes = cont.nodes,
                                         use.imputed.data = use.imputed.data, use.cpc = use.cpc, ..., params=params)
              
              difference <- shd(dag(w.net), dag(new.net))
              
              w.net     <- new.net
              w.dataset <- new.dataset
              
              if (difference <= struct.threshold)
                break
              else
                w.eng     <- InferenceEngine(w.net)
            }

            
            return(w.net)
          })
