logLikelihoodMarginalLatentCategoricalJoint = function(specs, variables, data){

  # calculate class probability from product of conditional likelihood for each latent:
  ll = logLikelihoodMarginalizeCategorical(
    specs = specs,
    variables = variables,
    data = data,
    classProbs = as.numeric(variables[[specs$jointVariables[1]]]$parameters$probVec)
  )

  return(ll)
}
