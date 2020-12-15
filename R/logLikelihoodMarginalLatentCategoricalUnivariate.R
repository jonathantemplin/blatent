logLikelihoodMarginalLatentCategoricalUnivariate = function(specs, variables, data){

    # calculate class probability from product of conditional likelihood for each latent:
    tempProb = matrix(data = NA, nrow = nrow(specs$attributeProfile), ncol = ncol(specs$attributeProfile))

    for (latent in 1:specs$nLatents){
      tempProb[,latent] = variables[[specs$latentOrder[latent]]]$routines$likelihood$run(
        data = specs$attributeProfile, beta = variables[[specs$latentOrder[latent]]]$beta, log=FALSE)
    }

    attProbs = apply(X = tempProb, MARGIN = 1, FUN = prod)

    ll = logLikelihoodMarginalizeCategorical(specs = specs, variables = variables, data = data, classProbs = attProbs)

    return(ll)
}
