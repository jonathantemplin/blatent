#' @title Posterior Predictive Model Checking Options
#'
#' @description Provides a list of posterior predictive model checks to be run following estimation of
#'  a blatent model. Currently six types of posterior predictive model checks (PPMCs) are available:
#'  univarate: mean and univariate Chi-square statistic, bivariate: covariance, tetrachoric correlation,
#'  pearson correlation, and bivariate Chi-square statistic.
#'
#'
#' @param estimatePPMC If \code{TRUE}, runs all PPMCs listed in \code{PPMCtypes}. Defaults to \code{TRUE}.
#'
#' @param PPMCsamples The number of samples from the posterior distribution and simulated
#'  PPMC data sets.
#'
#' @param PPMCtypes The type of PPMC tests to conduct. For each test, the statistic listed is calculated
#' on each PPMC-based simulated data set. Comparisons are made with the values of the statistics calculated
#' on the original data set. Currently six PPMC statistics are available:
#'  \itemize{
#'      \item \code{mean} Calculates the mean of each variable
#'      \item \code{univariate} Calculates the Pearson Chi-Square for each variable using simulated data as frequency expected and original data as frequency observed
#'      \item \code{covariance} Calculates the covariance of every pair of variables
#'      \item \code{pearson} Calculates the Pearson correlation of every pair of variables
#'      \item \code{tetrachoric} Calculates the tetrachoric correlation of every pair of variables
#'      \item \code{bivariate} Calculates the Pearson Chi-Square for each pair of variables using simulated data as frequency expected and original data as frequency observed
#'  }
#'
#' @param lowPPMCpercentile A vector of length equal to the length and number of \code{PPMCtypes} listing the lower percentile limit for the statistic in the observed data
#' to be considered extreme. Defaults to .025 for non-Chi-Square based statistics and 0 for the Chi-Square statistics
#'
#' @param highPPMCpercentile A vector of length equal to the length and number of \code{PPMCtypes} listing the upper percentile limit for the statistic in the observed data
#' to be considered extreme. Defaults to .975 for non-Chi-Square based statistics and 1 for the Chi-Square statistics
#'
#' @return A list of named values containing a logical value for each parameter above.
#'
#' @export
setPosteriorPredictiveCheckOptions = function(estimatePPMC = TRUE, PPMCsamples = 1000,
                                                     PPMCtypes = c("mean", "covariance", "univariate", "bivariate", "tetrachoric", "pearson"),
                                                     lowPPMCpercentile = c(.025, .025, 0, 0, .025, .025),
                                                     highPPMCpercentile = c(.975, .975, 1, 1, .975, .975)
                                             ){

  defaultPosteriorPredictiveChecks = list(estimatePPMC = estimatePPMC, PPMCsamples = PPMCsamples, PPMCtypes = PPMCtypes, lowPPMCpercentile = lowPPMCpercentile,
                                          highPPMCpercentile = highPPMCpercentile)
  return(defaultPosteriorPredictiveChecks)

}
