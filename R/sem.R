#' @rdname sem
#' @aliases sem,InferenceEngine,BNDataset
setMethod("sem",
          c("BN","BNDataset"),
          function(x, dataset, struct.threshold = 10, param.threshold = 0.001, scoring.func = "BIC",
                   alpha = 0.05, ess = 1, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset), cont.nodes = c(), use.imputed.data = FALSE,
                   use.cpc = TRUE, ...)
          {
            net <- x
            
            num.nodes <- num.nodes(net)

            # starting from an empty network: learn a starting point using MMHC
            if (is.character(scoring.func))
              scoring.func <- match(tolower(scoring.func), c("bdeu", "aic", "bic"))

            if (is.na(scoring.func))
            {
              message("scoring function not recognized, using BIC")
              scoring.func <- 2
            }
            else {
              scoring.func <- scoring.func - 1
            }
            # scoring.func(bn) <- c("BDeu", "AIC", "BIC")[scoring.func + 1]

            if (sum(dag(net)) == 0)
            {
              w.net <- net
              if (use.cpc)
                cpc <- mmpc( raw.data(dataset), node.sizes(w.net), cont.nodes, alpha, layering, layer.struct=c() )
              else
                cpc <- matrix(rep(0, num.nodes*num.nodes), nrow = num.nodes, ncol = num.nodes)
              dag(w.net) <- hc( raw.data(dataset), node.sizes(w.net), scoring.func, cpc, cont.nodes )
              
              w.net <- learn.params(w.net, dataset, ess = ess, use.imputed.data=use.imputed.data)
            }
            else
            {
              # start from an already learnt network
              w.net     <- net
            }
            
            w.dataset <- dataset
            w.eng     <- InferenceEngine(w.net)

            if (scoring.func == 0)
            {
              warning("Using the linear approximation of BDeu scoring function in SEM.")
            }
            
            #if (scoring.func == 1 || scoring.func == 2) # AIC or BIC
            #{
            repeat
            {
              out <- em(w.eng, dataset, param.threshold)
              
              new.eng     <- out$InferenceEngine
              new.dataset <- out$BNDataset
              
              new.net <- learn.structure(BN(new.dataset), new.dataset, "mmhc", c("bdeu", "aic", "bic")[scoring.func+1],
                                         alpha = alpha, ess = ess, bootstrap = bootstrap,
                                         layering = layering, max.fanin.layers = max.fanin.layers,
                                         max.fanin = max.fanin, cont.nodes = cont.nodes,
                                         use.imputed.data = use.imputed.data, use.cpc = use.cpc, ...)
              new.net <- learn.params(new.net, dataset, ess = ess, use.imputed.data=use.imputed.data)
              
              difference <- shd(dag(w.net), dag(new.net))
              
              w.net     <- new.net
              w.dataset <- new.dataset
              w.eng     <- InferenceEngine(w.net)
              # w.eng     <- belief.propagation(w.eng)
              cat("SHD between networks is ", difference, "\n")
              if (difference <= struct.threshold) break
            }
            
            updated.bn(w.eng) <- new.net
            #}
            
            return(list("InferenceEngine" = w.eng, "BNDataset" = w.dataset))
          })
