# internal function that holds all estimation syntax so as to streamline blatentEstimate function syntax
estimate <- function(specs, options, data, variables){

  model = blatentModel$new(data = data, specs = specs, options = options, chain = NULL, variables = variables)



  if (options$estimatorType == "external") {
    # reserved for future estimators external to R

  } else if (options$estimatorType == "R"){

    if (options$estimator == "blatent"){


      # use algorithm-specific routines to prepare data for analysis (i.e., adding underlying variables)
      model$prepareData()

      # create blank chain
      blankChain = createBlankChainHolders(specs = model$specs, options = model$options)

      cat("Entering Estimation Chains \n \n")

      chainNumber = 1
      chain = blankChain

      if (model$options$parallel){
        model = initializeClusters(model = model)
      } else {
        set.seed(seed = model$options$seed)
      }

      if (model$options$parallel){
        message =
          quote(if (chainNumber == 1) cat("\r", paste0("Progress: ", sprintf("%03.1f",round((iter/(options$nBurnin+options$nSampled*options$nThin)*100), digits = 1)), "% Complete")))
        # parallel::clusterEvalQ(cl = cl, expr = require(blatent))

        parallel::clusterExport(
          cl = model$specs$cl,
          varlist = c("model", "blankChain"),
          envir = environment()
        )

        model$chain = parallel::parLapply(
          cl = model$specs$cl,
          X = 1:model$options$nChains,
          fun = runChain,
          variables = model$variables,
          data = model$data,
          specs = model$specs,
          options = model$options,
          chain = blankChain,
          message = message
        )
        if (model$options$parallel) parallel::stopCluster(cl = model$specs$cl)
      } else {
        model$chain = list()
        message = quote(cat("\r", paste0(
          "Progress: ", sprintf("%03.1f", round(
            100 * (totalIter / specs$iterationsTotal), digits = 1
          )), "% Complete"
        )))

        for (chainNumber in 1:model$options$nChains){
          model$chain[[chainNumber]] =
            runChain(
              chainNumber = chainNumber,
              variables = model$variables,
              data = model$data,
              specs = model$specs,
              options = model$options,
              chain = blankChain,
              message = message
            )
        }
      }


    }
  }

  return(model)

}
