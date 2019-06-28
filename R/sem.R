# ' @rdname sem
# ' @aliases sem,InferenceEngine,BNDataset
setMethod("sem",
          c("BN","BNDataset"),
          function(x, dataset, struct.threshold = 0, param.threshold = 0, 
                   max.sem.iterations = 25, max.em.iterations = 10, scoring.func = "BDeu",
                   initial.network = NULL, alpha = 0.05, ess = 1, bootstrap = FALSE,
                   layering = c(), max.fanin.layers = NULL,
                   max.fanin = num.variables(dataset) - 1,
                   max.parents = num.variables(dataset) - 1,
                   cont.nodes = c(), use.imputed.data = FALSE,
                   use.cpc = TRUE, mandatory.edges = NULL, ...)
          {

            # fallback in case there is no missing data: learn the network and return
            if (sum(is.na(raw.data(dataset))) == 0) {
              bnstruct.log("no missing values found, learning the network once")
              net <- learn.network(x, dataset, algo = "mmhc", scoring.func = scoring.func,
                                       initial.network = initial.network,
                                       alpha = alpha, ess = ess, bootstrap = bootstrap,
                                       layering = layering, max.fanin.layers = max.fanin.layers,
                                       max.fanin = max.fanin, max.parents = max.parents,
                                       cont.nodes = cont.nodes, use.imputed.data = FALSE, use.cpc = use.cpc,
                                       mandatory.edges = mandatory.edges, ...)
              return(net)
            }

            net <- x
            
            num.nodes <- num.nodes(net)

            # overwrite max.fanin, in case max.parents is provided
            if (!is.null(max.parents)) {
              max.fanin <- max.parents
            }

            if (is.character(scoring.func))
              scoring.func <- match(tolower(scoring.func), c("bdeu", "aic", "bic"))

            if (is.na(scoring.func))
            {
              message("scoring function not recognized, using BDeu")
              scoring.func <- 0
            }
            else
              scoring.func <- scoring.func - 1

            # starting from an empty network: learn a starting point using MMHC
            if (is.null(initial.network) || is.na(sum(dag(net))) || sum(dag(net)) == 0)
            {
              w.net <- sample.chain(dataset)
            }
            else
            {
              # start from an already learnt network
              w.net     <- initial.network #net
            }
            
            w.dataset <- dataset
            w.eng     <- InferenceEngine(w.net)
            
            sem.iterations <- 0

            repeat
            {
              out <- em(w.eng, dataset, param.threshold, max.em.iterations, ess)
              
              new.eng     <- out$InferenceEngine
              new.dataset <- out$BNDataset
              
              new.net <- learn.network(new.dataset, "mmhc", c("bdeu", "aic", "bic")[scoring.func+1],
                                       initial.network = w.net, # NULL,
                                       alpha = alpha, ess = ess, bootstrap = bootstrap,
                                       layering = layering, max.fanin.layers = max.fanin.layers,
                                       max.fanin = max.fanin, max.parents = max.parents,
                                       cont.nodes = cont.nodes, use.imputed.data = T, use.cpc = use.cpc,
                                       mandatory.edges = mandatory.edges, ...)
              
              difference <- shd(dag(w.net), dag(new.net))
              
              w.net     <- new.net
              w.dataset <- new.dataset
              
              sem.iterations <- sem.iterations + 1
              
              if (difference <= struct.threshold || sem.iterations >= max.sem.iterations)
                break
              else
                w.eng     <- InferenceEngine(w.net)
            }

            
            return(w.net)
          })
