#' @title Creates named numeric vector with parameter names for analysis specified by modelText
#'
#' @description Creates named numeric vector with parameter names for analysis specified by modelText.
#'
#' @param modelText A character string that contains the specifications for the model to be run. See \code{\link{blatentSyntax}}
#'                  or more information about syntax formatting.
#'
#' @examples
#' # Generating parameters for data using Q-matrix structure from data example in Chapter 9 of
#' #  Rupp, Templin, & Henson (2010).
#'
#' RTHCh9ModelSyntax = "
#'    item1 ~ A1
#'    item2 ~ A2
#'    item3 ~ A3
#'    item4 ~ A1 + A2 + A1:A2
#'    item5 ~ A1 + A3 + A1:A3
#'    item6 ~ A2 + A3 + A2:A3
#'    item7 ~ A1 + A2 + A3 + A1:A2 + A1:A3 + A2:A3 + A1:A2:A3
#'
#'    # Latent Variable Specifications:
#'   A1 A2 A3 <- latent(unit='rows',distribution='bernoulli',structure='univariate',type='ordinal')
#'
#'    # Observed Variable Specifications:
#'    item1-item7 <- observed(distribution = 'bernoulli', link = 'probit')
#' "
#' paramVals = createParameterVector(modelText = RTHCh9ModelSyntax)
#'
#' @export
createParameterVector = function(modelText){

  options = blatentControl()

  # intialize blatent specs--some will be invalid as blatentControl is for MCMC specs and dataMat does not exist yet
  specs = initializeSpecs(modelText = modelText, dataMat = data.frame(matrix(data = NA, nrow = 1, ncol = 1)),
                          options = options)

  # build better temporary data frame:
  data = data.frame(matrix(data = 1, nrow = 1, ncol = length(specs$allVariables)))
  names(data) = specs$allVariables

  # create variables object:
  # set up object for variables in graph
  variables = lapply(
    X = 1:length(specs$modelVector),
    FUN = createVariable,
    specs = specs,
    data = data,
    options = options
  )

  names(variables) = unlist(lapply(X = variables, FUN = function(x) return(x$variableName)))

  # get parameter list:
  temp =  lapply(
    X = variables,
    FUN = function(x, data) {
      return(x$provideParameterTypesAndLocations(data = data))
    },
    data = data
  )

  paramNames = unlist(lapply(X = temp, FUN = function(x) return(x$paramNames[which(x$paramTypes == "model")])))

  # create empty vector
  paramVec = as.numeric(rep(NA, length(paramNames)))

  # add names to empty vector
  names(paramVec) = paramNames

  return(paramVec)
}
