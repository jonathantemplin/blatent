logLikelihoodMarginalizeCategorical = function(specs, variables, data, classProbs){

  # loop across each profile
  margLL = 0
  profile = 1
  for (profile in 1:nrow(specs$attributeProfile)){
    data[,specs$latentVariables] = specs$attributeProfile[profile, ]
    margLL = margLL + classProbs[profile]*exp(logLikelihoodObservedOnly(specs = specs, variables = variables, data = data))
  }

  return(log(margLL))

}
