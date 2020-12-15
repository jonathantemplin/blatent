#' @import truncnorm
classVariable_Bernoulli <- R6Class(
  classname = "variable_Bernoulli",
  inherit = classVariable_Generic,
  public = list(
# public variables ==================================================================================================================
    lowerBoundZ = NA,
    underlyingVariableZ = NA,
    underlyingVariableLambda = NA,
    underlyingVariables = NA,
    upperBoundZ = NA,
# public functions ==================================================================================================================

drawParametersConstrained = function(n, mu, sigma, constraintMatrix, nConstraints, ...){
  # params = t(
  #   tmvtnorm::rtmvnorm2(
  #     n = 1,
  #     mean = as.numeric(mu),
  #     sigma = sigma,
  #     D = constraintMatrix,
  #     lower = rep(0, nConstraints),
  #     upper = rep(Inf, nConstraints),
  #     algorithm = "rejection"
  #   ))

  # params = t(truncMVNblatent(previous = self$parameters$beta, mu = mu, sigma = sigma, D = constraintMatrix, maxSample = 1000))

  params = t(rtmvnormRejection_Rcpp(mu = mu, sigma = sigma, D = constraintMatrix, maxSample = 10000, previous = t(self$parameters$beta)))
  return(params)
},

drawParametersUnconstrained = function(n, mu, sigma, ...){

  return(t(rmvnorm_Rcpp(n = n, mu = mu, sigma = sigma)))
},

initialize = function(modelNumber, specs, data, options, distributionSpecs, ...){


  self$variableName = as.character(terms(specs$modelVector[[modelNumber]])[[2]])

  self$distributionSpecs = distributionSpecs
  self$defaultSimulatedParameters = specs$defaultSimulatedParameters

  self$isLatentJoint = FALSE # currently bernoulli variables cannot have joint distributions

  if (any(specs$latentVariables == self$variableName)){
    self$isLatent = TRUE
  } else {
    self$isLatent = FALSE
  }

  self$formula = specs$modelVector[[modelNumber]]
  self$formulaRHS = self$formula[-2]

  self$predictedBy = rownames(attr(terms(self$formulaRHS), "factors"))
  # if (is.null(self$predictedBy)) self$predictedBy = NA

  tempParam = colnames(model.matrix(self$formula, data = data))
  self$nParam = length(tempParam)

  if (self$nParam > 0){
    self$paramNames = paste(self$variableName, tempParam, sep=".")
    self$predNames = tempParam
    self$parameters$beta = matrix(data = 0, nrow = self$nParam, ncol = 1)
    rownames(self$parameters$beta) = self$paramNames

    # for prior distribution
    self$hyperparameters$betaMean = matrix(  #default prior mean 0
      data = options$defaultPriors$normalMean,
      nrow = self$nParam,
      ncol = 1
    )

    self$hyperparameters$betaCov = matrix(
      data = options$defaultPriors$normalCovariance,
      nrow = self$nParam,
      ncol = self$nParam
    )
    diag(self$hyperparameters$betaCov) = options$defaultPriors$normalVariance

    # check if parameters are listed in priorsList
    if (any(names(specs$priorsList) %in% self$paramNames)){
      relevantPriors = specs$priorsList[which(names(specs$priorsList) %in% self$paramNames)]

      # loop through and add prior components
      for (param in 1:length(relevantPriors)){
        # get location
        paramLoc = which(self$paramNames == names(relevantPriors)[param])

        # get type
        if ("mean" %in% names(relevantPriors[[param]])){
          self$hyperparameters$betaMean[paramLoc, 1] = relevantPriors[[param]]$mean
        }

        if ("variance" %in% names(relevantPriors[[param]])){
          self$hyperparameters$betaCov[paramLoc, paramLoc] = relevantPriors[[param]]$variance
        }
      }

    }
    self$hyperparameters$invBetaCov = solve(self$hyperparameters$betaCov)
    self$hyperparameters$invBetaCovBetaMean = self$hyperparameters$invBetaCov %*% self$hyperparameters$betaMean


  } else {
    self$paramNames = NA
    self$predNames = NA
    self$attributeProfile = NA
  }

  # set initialization values and distributions
  self$initialParameterValues$initMeanVec = matrix(data = options$defaultInitializeParameters$normalMean,
                                                   nrow = self$nParam,
                                                   ncol = 1)
  self$initialParameterValues$initCovMat = matrix(data = options$defaultInitializeParameters$normalCovariance,
                                                  nrow = self$nParam,
                                                  ncol = self$nParam)
  diag(self$initialParameterValues$initCovMat) = options$defaultInitializeParameters$normalVariance



  # determine what variables are predicted by this variable
  self$predicts = NULL
  for (model in 1:length(specs$modelVector)){
    preds = all.vars(terms(specs$modelVector[[model]])[[3]])
    dv = all.vars(terms(specs$modelVector[[model]])[[2]])

    if (self$variableName %in% preds){
      self$predicts = c(self$predicts, dv)
    }
  }

  if (is.null(self$predicts)){
    self$isPredictor = FALSE
  } else {
    self$isPredictor = TRUE
  }


  #determine if any observations are missing:
  self$missing = which(is.na(data[self$variableName]))
  self$nMissing = length(self$missing)

  self$observed = which(!is.na(data[self$variableName]))
  self$nObserved = length(self$observed)

  # Determine which cases are complete (all predictors and outcome are observed) vs incomplete (! all predictors and outcome are present)
  if (!any(is.na(self$predictedBy))){

    self$incompleteCases = unique(unlist(lapply(
      X = self$predictedBy,
      FUN = function(x, data) {
        return(which(is.na(data[x])))
      },
      data = data
    )))

    if (is.null(self$incompleteCases)){
      self$completeCases = self$observed
    } else {
      self$incompleteCases = self$incompleteCases[order(self$incompleteCases)]
      self$completeCases = self$observed[which(!self$observed %in% self$incompleteCases)]
    }


  } else {

    # predicted by no variables--allObserved == observed
    self$completeCases = self$observed

  }

  # determine which cases are modeled based on missing methods and complete/incomplete cases

  if (self$isLatent){

    # if latent variable--impute all observations
    self$imputeList = self$observed
    self$nImpute = length(self$imputeList)
    self$modeledObs = self$observed
    self$modeledN = nrow(data)

  } else {

    # if not latent variable, impute only missing observations if missingMethod == imputeBayes
    if (options$missingMethod == "omit"){
      self$imputeList = NA
      self$nImpute = 0
      self$modeledObs = self$completeCases
      self$modeledN = length(self$completeCases)

    } else if (options$missingMethod == "imputeBayes") {
      self$imputeList = self$missing
      self$nImpute = length(self$imputeList)
      self$modeledObs = unique(c(self$observed, self$missing))
      self$modeledObs = self$modeledObs[order(self$modeledObs)]
      self$modeledN = nrow(data)

    }
  }


  # next check if constraints on parameters are needed as this will mean we use self$routines list for conditional routines

  useConstraints = FALSE

  if (self$variableName %in% specs$latentVariables){
    useConstraints = FALSE

  } else {
    if (is.null(self$predictedBy)){
      useConstraints == FALSE
    } else {
      # check to see if predictors of variable are ordered categorical latent variables
      if (any(self$predictedBy %in% specs$orderedLatents)){
        useConstraints = TRUE
      } else {
        useConstraints = FALSE
      }

    }
  }

  # gather if any of variables predicting this one are of type==ordinal
  if (useConstraints){
    self$constraintMatrix = makeConstraintMatrix(
      model = self$formula,
      categoricalLatentVariables = specs$orderedLatents
    )
    self$nConstraints = nrow(self$constraintMatrix)
    self$routines$drawParameters = self$drawParametersConstrained

  } else {

    # no constraints needed -- load routines with non-constrained versions
    self$constraintMatrix = matrix(data = 0, nrow = 1, ncol = length(self$paramNames))
    self$nConstraints = 0
    # self$routines$initializeParameters = self$drawParametersUnconstrained
    self$routines$drawParameters = self$drawParametersUnconstrained
  }

  # add underlying variable names initialize limits for Z
  self$underlyingVariableZ = paste0("z_", self$variableName)
  self$underlyingVariableLambda = paste0("lambda_", self$variableName)
  self$underlyingVariables = c(self$underlyingVariableZ, self$underlyingVariableLambda)
  self$allDrawnVariables = c(self$variableName, self$underlyingVariables)

  self$lowerBoundZ = self$upperBoundZ = rep(NA, self$modeledN)

  self$setUnderlyingBounds(checkLoc = self$modeledObs, dataVar = data[self$variableName])


  self$likelihoodPTR = bernoulliLikelihoodPtr("bernoulli")
},

initializeParameters = function(...){

  self$parameters$beta = self$routines$drawParameters(
    n = 1,
    mu = self$initialParameterValues$initMeanVec,
    sigma = self$initialParameterValues$initCovMat,
    constraintMatrix = self$constraintMatrix,
    nConstraints = self$nConstraints
  )
},

initializeUnits = function(data, ...){

  # impute observations that are missing/latent
  if (self$nImpute > 0){
    data[self$imputeList, self$variableName] = as.numeric(rbinom(n = self$nImpute, size = 1, prob = .5))
    self$setUnderlyingBounds(checkLoc = self$imputeList, dataVar = data[self$variableName])
  }

  data[self$modeledObs, self$underlyingVariableZ] = rep(NA, self$modeledN)


  data[self$modeledObs, self$underlyingVariableZ] =
    truncnorm::rtruncnorm(
      n = self$modeledN,
      a = self$lowerBoundZ[self$modeledObs],
      b = self$upperBoundZ[self$modeledObs],
      mean = rep(0, self$modeledN),
      sd = 1
    )

  r = runif(
    n = self$modeledObs,
    min = 1,
    max = 2
  )


  data[self$modeledObs, self$underlyingVariableLambda] = rep(NA, self$modeledN)
  data[self$modeledObs, self$underlyingVariableLambda] = rks(n = self$modeledN, r = r)

  return(data[, c(self$variableName,
                  self$underlyingVariableZ,
                  self$underlyingVariableLambda)])

},

KLI = function(attributeProfile){

  KLImatrix = matrix(
    data = 0,
    nrow = nrow(attributeProfile),
    ncol = nrow(attributeProfile)
  )

  profile1=1

  predictedValues = self$predictedValues(data = as.data.frame(attributeProfile))
  for (profile1 in 1:nrow(attributeProfile)){
    profile2=2
    for (profile2 in 1:nrow(attributeProfile)){
      prob1 = predictedValues[profile1,1]
      prob2 = predictedValues[profile2,1]

      KLImatrix[profile1, profile2] = prob1*log(prob1/prob2) + (1-prob1)*log((1-prob1)/(1-prob2))

    }
  }


  return(KLImatrix)
},

likelihood = function(data, log = TRUE, ...){

  linPred = as.numeric(Matrix::sparse.model.matrix(self$formulaRHS, data = data) %*% self$parameters$beta)

  loglike = dbinom(x = data[, self$variableName], size = 1L,
                   prob = self$distributionSpecs$inverseLinkFunction(linPred),
                   log = log)

  return(loglike)

},

predictedValues = function(data){
  return(self$distributionSpecs$inverseLinkFunction(Matrix::sparse.model.matrix(self$formulaRHS, data = data) %*% self$parameters$beta))
},

prepareData = function(data, ...){

  # here, we must add two underlying variables to the data: z and lambda
  data = cbind(data, matrix(
    data = 0,
    nrow = nrow(data),
    ncol = 2
  ))

  names(data)[(ncol(data)-1):ncol(data)] = self$underlyingVariables
  return(data)

},

provideParameterTypesAndLocations = function(data){

  paramLocation = NULL
  params = NULL
  if (!is.null(self$parameters$beta)){
    params = rownames(self$parameters$beta)
    for (j in 1:nrow(self$parameters$beta)){
      paramLocation = c(paramLocation, paste0('variables[["', self$variableName, '"]]$parameters$beta[', j, ',1]'))
    }
  }

  paramType = rep(x = "model", times = length(params))

  if (self$nImpute > 0){
    dataParams = paste0(rownames(data[self$imputeList,]), ".", self$variableName)
    params = c(params, dataParams)

    paramType = c(paramType, rep(times = length(dataParams), x = "data"))
    for (i in 1:length(self$imputeList)){
      paramLocation = c(paramLocation, paste0("data$", self$variableName, "[", self$imputeList[i], "]"))
    }
    # paramLocation = c(paramLocation, paste0("data$", self$variableName, paste0("[c(", paste0(self$imputeList, collapse = ","), ")]")))
  }

  return(list(paramNames=params, paramTypes=paramType, paramLocation = paramLocation))

},

provideParameterValues = function(){


  temp = list(paramVec = as.numeric(self$parameters$beta))

  # add each type of parameter separately

  # beta parameters
  temp2 = lapply(X = rownames(self$parameters$beta), FUN = function(y) return(self$parameters$beta[which(rownames(self$parameters$beta) == y)]))
  names(temp2) = self$predNames

  # finally, return the parameter matrices
  temp4 = list(meanBeta = self$parameters$beta)

  return(c(temp, temp2, temp4))
},

sampleParameters = function(data, ...){

  temp = getMeanAndCov_HH2006L1(X = model.matrix(self$formula, data = data[self$modeledObs,]),
                                W = diag(1/data[self$modeledObs, self$underlyingVariableLambda]),
                                Y = as.matrix(data[self$modeledObs, self$underlyingVariableZ]),
                                invBetaCov = self$hyperparameters$invBetaCov,
                                invBetaCovBetaMean = self$hyperparameters$invBetaCovBetaMean)

  self$parameters$beta = self$routines$drawParameters(n = 1, mu = temp$mean, sigma = temp$cov,
                                                      constraintMatrix = self$constraintMatrix,
                                                      nConstraints = self$nConstraints)

  invisible(self)
},

sampleUnits = function(data, variables, ...){


  if (self$nImpute > 0){

    data[self$imputeList, self$variableName] = sampleBernoulliUnits(
      data = data[self$imputeList, ],
      variables = variables,
      varName = self$variableName
    )

    # set bounds for underlying variables
    temp = self$setUnderlyingBounds(checkLoc = self$imputeList, dataVar = data[self$variableName])
  }

  # sample lambda variables from KS distribution
  X = model.matrix(self$formula, data = data)
  XBeta = X %*% self$parameters$beta

  r2 = (data[self$modeledObs, self$underlyingVariableZ] - XBeta)^2

  data[self$modeledObs, self$underlyingVariableLambda] =
    rks_Rcpp(n = self$modeledN, r = sqrt(r2))


  # sample underlying variables
  # data[self$observed, self$underlyingVariableName] = rep(NA, self$N)
  data[self$modeledObs, self$underlyingVariableZ] =
    truncnorm::rtruncnorm(
      n = self$modeledN,
      a = self$lowerBoundZ[self$modeledObs],
      b = self$upperBoundZ[self$modeledObs],
      mean = as.numeric(XBeta),
      sd = sqrt(data[self$modeledObs, self$underlyingVariableLambda])
    )
  return(data[,c(self$variableName, self$underlyingVariableZ, self$underlyingVariableLambda)])

},

setUnderlyingBounds = function(checkLoc, dataVar){

  # reset limits for Gibbs step for parameters
  self$lowerBoundZ[checkLoc[which(checkLoc %in% which(dataVar == 0))]] = -Inf
  self$lowerBoundZ[checkLoc[which(checkLoc %in% which(dataVar == 1))]] = 0

  self$upperBoundZ[checkLoc[which(checkLoc %in% which(dataVar == 0))]] = 0
  self$upperBoundZ[checkLoc[which(checkLoc %in% which(dataVar == 1))]] = Inf
  #
  # self$lowerBoundZ[dataVar == 0] = -Inf
  # self$lowerBoundZ[dataVar == 1] = 0
  #
  # self$upperBoundZ[dataVar == 0] = 0
  # self$upperBoundZ[dataVar == 1] = Inf

  return(NULL)
},

simulateParameters = function(){

  constraintTest = rep(-1, length(self$paramNames))

  while(any(constraintTest < 0)){

    for (p in 1:length(self$paramNames)) {
      if (length(grep(pattern = "(Intercept)", self$paramNames[p])) > 0) {
        # parameter is intercept: draw parameter
        if (self$isLatent) {
          simValue = eval(parse(text = self$defaultSimulatedParameters$latentIntercepts))
        } else {
          simValue = eval(parse(text = self$defaultSimulatedParameters$observedIntercepts))
        }
      } else if (length(grep(pattern = ":", self$paramNames[p])) > 0) {
        # parameter is interaction: draw parameter
        # parameter is intercept: draw parameter
        if (self$isLatent) {
          simValue = eval(parse(text = self$defaultSimulatedParameters$latentInteractions))
        } else {
          simValue = eval(parse(text = self$defaultSimulatedParameters$observedInteractions))
        }
      } else {
        # parameter is main effect
        # parameter is intercept: draw parameter
        if (self$isLatent) {
          simValue = eval(parse(text = self$defaultSimulatedParameters$latentMainEffects))
        } else {
          simValue = eval(parse(text = self$defaultSimulatedParameters$observedMainEffects))
        }
      }

      self$parameters$beta[p,1] = simValue

    }
    constraintTest = self$constraintMatrix %*% self$parameters$beta
  }

  invisible(self)
},

simulateUnits = function(data){

  # create predicted values:
  linPred = model.matrix(object = self$formulaRHS, data = data) %*% self$parameters$beta

  predProb = self$distributionSpecs$inverseLinkFunction(linPred)
  varData = data.frame(as.numeric(rbinom(n = nrow(data), size = 1, prob = predProb)))
  names(varData) = self$variableName
  return(varData)
}

  )
)

