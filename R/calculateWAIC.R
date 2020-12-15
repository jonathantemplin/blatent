#' @title calculateWAIC
#'
#' @description Calculates WAIC for a given model using model object specs.
#'
#' @param model A blatent MCMC model object.
#'
#' @export

calculateWAIC = function(model){


  # # check to see if model likelihoods have already been calculated
  if (is.null(model$logLikelihoods)) model$calculateLogLikelihoods(force = TRUE)


  lpd_hat = sum(apply(X = model$logLikelihoods$marginal, MARGIN = 2, FUN = function(x) return(log(mean(exp(x))))))

  p_waic_hat = sum(apply(X = model$logLikelihoods$marginal, MARGIN = 2, FUN = stats::var))

  WAIC = lpd_hat - p_waic_hat

  return(list(WAIC = WAIC, p_WAIC = p_waic_hat))


}
