#' @title Sets the prior distribution parameters for all parameters not named in priorsList
#'
#' @description All parameters not named in priorsList, an input argument to
#'              \code{\link{blatentEstimate}}, recieve these parameters if their prior distributions
#'              are of the same family. Used to quickly set priors for sets of parameters.
#'
#' @param normalMean Sets the prior distribution mean for all parameters with
#' normal distributions not named in priorsList. Defaults to \code{0}.
#'
#' @param normalVariance Sets the prior distribution variance for all parameters with
#' normal distributions not named in priorsList. Defaults to \code{1000}.
#'
#' @param normalCovariance Sets the prior distribution covariance for all parameters with
#' multivariate normal distributions not named in priorsList. Defaults to \code{0}.
#'
#' @param dirichletAlpha Sets the prior distribution parameter values when variable distributions are Dirichlet. Defaults to \code{1}.
#'
#' @return A list containing named values for each argument in the function.
#'
#' @export
setDefaultPriors = function(normalMean = 0, normalVariance = 1, normalCovariance = 0, dirichletAlpha = 1){
  defaultPriors = list(
    normalMean = normalMean,
    normalVariance = normalVariance,
    normalCovariance = normalCovariance,
    dirichletAlpha = dirichletAlpha
  )
  return(defaultPriors)
}
