#' @title calculateDIC
#'
#' @description Calculates DIC for a given model using model object specs.
#'
#' @param model A blatent MCMC model object.
#'
#' @export

calculateDIC = function(model){


  # # check to see if model likelihoods have already been calculated
  if (is.null(model$logLikelihoods)) model$calculateLogLikelihoods(force = TRUE)

  # next, calculate deviance when at posterior mean
  model$movePosteriorMeanToVariableBeta()


  D_ThetaBar = sum(-2*model$logLikelihoods$routines$calculateMarginalLogLikelihood(
    specs = model$specs, variables = model$variables, data = model$data))


  Dbar_theta = mean(-2*rowSums(model$logLikelihoods$marginal))

  p_D = Dbar_theta - D_ThetaBar

  DIC = D_ThetaBar + 2*p_D

  return(list(DIC = DIC, p_D = p_D))

}
