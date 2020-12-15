#'
#'
# for Bernoulli variables that either have or do not have constraints on their parameters. Currently only uses probit link function (hard wired)
classVariable_Normal <- R6Class(

classname = "variable_Normal",
# public ==================================================================================================================
public = list(

# variables ===================================================================================================================
allDrawnVariables = NA,
meanIdentification = NA,
varianceIdentification = NA,

# functions ==================================================================================================================
initialize = function(variable, ...){
  self$allDrawnVariables = variable$variableName
  self$meanIdentification = variable$distributionSpecs$meanIdentification
  self$varianceIdentification = variable$distributionSpecs$varianceIdentification
},

drawParameters = function(variable, variables, defaultSimulatedParameters = setDefaultSimulatedParameters(), parameterValues = NULL, ...){

  paramNames = rownames(variable$beta)
  predictors = all.vars(variable$formula[[3]])
  predTypes = unlist(lapply(X = predictors, FUN = function(x) return(variables[[x]]$distributionSpecs$type)))

  if (variable$distributionSpecs$meanIdentification == "fixed"){
    # for fixed mean identification, we have to fix the mean to zero--it is not generated
    variable$beta[1,1] = 0


  } else{
    # change so that simulated parameters observe ordinal monotonicity constraints
    if (any(predTypes == "ordinal")) {
      # monotonicity constraints apply (for now, doing so without mixed types)
      constraintMatrix = makeConstraintMatrix(
        model = variable$formula,
        categoricalLatentVariables = predictors
      )
    } else {
      constraintMatrix = matrix(data = 0, nrow = 1, ncol = length(paramNames))
    }

    constraintTest = rep(-1, length(paramNames))

    while(any(constraintTest < 0)){

      for (p in 1:length(paramNames)) {
        if (length(grep(pattern = "(Intercept)", paramNames[p])) > 0) {
          # parameter is intercept: draw parameter
          if (variable$isLatent) {
            simValue = eval(parse(text = defaultSimulatedParameters$latentIntercepts))
          } else {
            simValue = eval(parse(text = defaultSimulatedParameters$observedIntercepts))
          }
        } else if (length(grep(pattern = ":", paramNames[p])) > 0) {
          # parameter is interaction: draw parameter
          # parameter is intercept: draw parameter
          if (variable$isLatent) {
            simValue = eval(parse(text = defaultSimulatedParameters$latentInteractions))
          } else {
            simValue = eval(parse(text = defaultSimulatedParameters$observedInteractions))
          }
        } else {
          # parameter is main effect
          # parameter is intercept: draw parameter
          if (variable$isLatent) {
            simValue = eval(parse(text = defaultSimulatedParameters$latentMainEffects))
          } else {
            simValue = eval(parse(text = defaultSimulatedParameters$observedMainEffects))
          }
        }

        variable$beta[p,1] = simValue

      }
      constraintTest = constraintMatrix %*% variable$beta
    }
  }

  if (variable$distributionSpecs$varianceIdentification == "fixed"){
    variable$covMat[1,1] = 1
    rownames(variable$covMat) = variable$variableName
    colnames(variable$covMat) = variable$variableName
  }


  return(variable)
},

drawUnits = function(variableName, nObs, data, variables){

  # create predicted values:
  linPred = model.matrix(object = variables[[variableName]]$formulaRHS, data = data) %*% variables[[variableName]]$beta

  # varData = data.frame(as.numeric(mvtnorm::rmvnorm(n = nObs, mean = as.numeric(variables[[variableName]]$beta),
  #                                                  sigma = variables[[variableName]]$covMat)))
  names(varData) = variableName
  return(varData)
},

provideParameterLocationsAndTypes = function(variableName, variables, data){

  #provides model parameter locations and types for blatentModel

  paramLocation = NULL
  params = NULL
  if (!is.null(variables[[variableName]]$beta)){
    params = rownames(variables[[variableName]]$beta)
    for (j in 1:nrow(variables[[variableName]]$beta)){
      paramLocation = c(paramLocation, paste0("variables[[", variableName, "]]$beta[", j, ",1]"))
    }
  }

  if (!is.null(variables[[variableName]]$covMat)){
    covNames = NULL
    for (row in 1:nrow(variables[[variableName]]$covMat)){
      for (col in 1:row){
        if (row == col){
          covNames = paste0("var_", rownames(variables[[variableName]]$covMat)[row])
        } else{
          covNames = paste0("cov_", rownames(variables[[variableName]]$covMat)[row], colnames(variables[[variableName]]$covMat)[col])
        }
        paramLocation = c(paramLocation, paste0("variables[[", variableName, "]]$covMat[", row, ",", col,"]"))
      }
    }
    params = c(params, covNames)
  }

  paramType = rep(x = "model", times = length(params))

  if (variables$theta$isLatent){
    dataParams = paste0(rownames(data), ".", variableName)
    params = c(params, dataParams)

    paramType = c(paramType, rep(times = length(dataParams), x = "data"))
    paramLocation = c(paramLocation, paste0("data$", variableName))
  }

  return(list(paramNames=params, paramTypes=paramType, paramLocation = paramLocation))
},

provideParameterValues = function(variableName, variables){
  #provides model parameter values for simulated data

  # create paramVec:

  # mean parameters
  temp = list(paramVec = as.numeric(variables[[variableName]]$beta))

  # variance/covariance parameters
  temp$paramVec = c(temp$paramVec, variables[[variableName]]$covMat[lower.tri(variables[[variableName]]$covMat, diag = TRUE)])

  # add each type of parameter separately

  # mean parameters
  temp2 = lapply(X = rownames(variables[[variableName]]$beta), FUN = function(y) return(variables[[variableName]]$beta[which(rownames(variables[[variableName]]$beta) == y)]))
  names(temp2) = variables[[variableName]]$predNames

  # covariance parameters
  temp3 = list(variables[[variableName]]$covMat[lower.tri(variables[[variableName]]$covMat, diag = TRUE)])
  covNames = NULL
  for (row in 1:nrow(variables[[variableName]]$covMat)){
    for (col in 1:row){
      if (row == col){
        covNames = paste0("var_", rownames(variables[[variableName]]$covMat)[row])
      } else{
        covNames = paste0("cov_", rownames(variables[[variableName]]$covMat)[row], colnames(variables[[variableName]]$covMat)[col])
      }

    }
  }
  names(temp3) = covNames

  # finally, return the parameter matrices
  temp4 = list(meanBeta = variables[[variableName]]$beta, covMat = variables[[variableName]]$covMat)

  return(c(temp, temp2, temp3, temp4))
}


)


)

