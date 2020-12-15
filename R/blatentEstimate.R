#' Use blatent to estimate a Bayesian latent variable model. Currently supports estimation of the LCDM (Loglinar Cognitive
#' Diagnosis Model).
#'
#' @description
#' Blatantly runs Bayesian latent variable models.
#'
#' @param dataMat A data frame containing the data used for the analysis.
#' @param modelText A character string that contains the specifications for the model to be run. See \code{\link{blatentSyntax}}
#'                  or more information about syntax formatting.
#' @param priorsList A list of priors to be placed on parameters of the model. Defaults to NULL. Currently only accepts NULL.
#'                   All priors not set in \code{priorsList} will be set in options using \code{\link{blatentControl}} via the
#'                   \code{\link{setDefaultPriors}} function.
#' @param options A list of options for estimating the model. Use the \code{\link{blatentControl}} function to specify the options.
#'                See \code{\link{blatentControl}} for more information and default values.
#'
#' @return A blatentModel object (an R6 class).
#'
#' @export
blatentEstimate = function(dataMat, modelText, priorsList = NULL, options = blatentControl()){

  # parse model syntax
  specs = initializeSpecs(modelText = modelText, dataMat = dataMat, options = options)

  # set up data frame for modeling
  data = createModelData(specs = specs)

  # set up object for variables in graph
  variables = lapply(
    X = 1:length(specs$modelVector),
    FUN = createVariable,
    specs = specs,
    data = data,
    options = options
  )

  names(variables) = unlist(lapply(X = variables, FUN = function(x) return(x$variableName)))

  # grab model parameters and make into list used in output summary and estimation
  specs$parameters = getModelParameters(variables = variables, data = data)

  model = estimate(specs = specs, options = options, data = data, variables = variables)

  # prepare information criteria (if requested)
  if (options$calculateDIC | options$calculateWAIC){
    cat("\r Calculating Information Criteria")

    model$informationCriteria = list()
    if (options$calculateDIC) model$informationCriteria$DIC = calculateDIC(model = model)
    if (options$calculateWAIC) model$informationCriteria$WAIC = calculateWAIC(model = model)

  }

  # prepare PPMC (if requested)
  if (options$posteriorPredictiveChecks$estimatePPMC){
    cat("\r Calculating Posteior Predictive Checks\n")

    model$PPMC = blatentPPMC(model = model, nSamples = model$options$posteriorPredictiveChecks$PPMCsamples,
                             seed = NULL, parallel = model$options$parallel, nCores = model$options$nCores,
                             type = model$options$posteriorPredictiveChecks$PPMCtypes,
                             lowPPMCpercentile = model$options$posteriorPredictiveChecks$lowPPMCpercentile,
                             highPPMCpercentile = model$options$posteriorPredictiveChecks$highPPMCpercentile)
  }

  # prepare latent variable summaries (if requested)
  if (options$estimateLatents){
    cat("\r Calculating Latent Variable Summaries\n")

    model$latentEstimates()
  }

  # remove auxiliary variables from data frame
  model$data = model$data[, which(!names(model$data) %in% model$specs$underlyingVariables)]

  # prepare summary of chain estimates here
  model$createParameterSummary()


  model$specs$EndTime = Sys.time()
  model$specs$RunTime = model$specs$EndTime - model$specs$StartTime
  return(model)

}
