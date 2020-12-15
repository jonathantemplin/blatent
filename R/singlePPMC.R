singlePPMC = function(model, fullPosterior, estimableCovariances, dataFunctions){

  # draw sample at random from posterior
  postDraw = sample(size = 1, x = 1:nrow(fullPosterior$variables))

  # move posterior values to variables
  temp = lapply(
    X = model$variables,
    FUN = function(x, fullPosterior, postDraw) {
      paramVals = fullPosterior$variables[postDraw, which(colnames(fullPosterior$variables) %in% x$paramNames)][x$paramNames]
      evalThis = paste0("model$",
                        model$specs$parameters$paramLocation[which(model$specs$parameters$paramNames %in% x$paramNames)],
                        "=", paramVals[x$paramNames])
      eval(parse(text=evalThis))
      # x$beta = t(t(fullPosterior$variables[postDraw, which(colnames(fullPosterior$variables) %in% x$paramNames)][x$paramNames]))
      return(x)
    },
    fullPosterior = fullPosterior,
    postDraw = postDraw
  )

  simData = model$data

  # generate data
  for (var in 1:length(model$specs$predOrder)){

    # separated so as to allow for multiple variables to be drawn (for multivariate distributions)
    temp = model$variables[[model$specs$predOrder[var]]]$simulateUnits(
      data = simData
    )

    simData[names(temp)] = temp


  }

  # lapply(X = dataFunctions, FUN = function(x, simData, realData, estimableCovariances) return(x(simData =simData, realData =realData, estimableCovariances = estimableCovariances)), simData = simData[model$specs$observedVariables], realData = model$data, estimableCovariances = estimableCovariances)


  # run each PPMC in dataFunctions
  iterResults = lapply(
    X = dataFunctions,
    FUN = function(x,
                   simData,
                   realData,
                   estimableCovariances)
      return(
        x(
          simData = simData,
          realData = realData,
          estimableCovariances = estimableCovariances
        )
      ),
  simData = simData[model$specs$observedVariables],
  realData = model$data,
  estimableCovariances = estimableCovariances
  )


  # iterResults = list()
  #
  # for (ppmc in 1:length(dataFunctions)){
  #   iterResults[[ppmc]] = dataFunctions[[ppmc]](
  #     simData = simData[model$specs$observedVariables],
  #     realData = model$data,
  #     estimableCovariances = estimableCovariances
  #   )
  # }

  ppcdata = do.call("cbind", iterResults)
  return(data.frame(ppcdata))


}
