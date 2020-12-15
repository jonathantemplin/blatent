classVariable_MultivariateBernoulli <- R6Class(
  classname = "variable_MultivariateBernoulli",
  inherit = classVariable_Generic,
  public = list(
# public variables ==================================================================================================================
    attributeProfile = NA,
    nProfiles = NA,
    nVars = NA,
    vars = NA,
# public functions ==================================================================================================================

initialize = function(modelNumber, specs, data, options, distributionSpecs, ...){

  self$variableName = as.character(terms(specs$modelVector[[modelNumber]])[[2]])

  self$distributionSpecs = distributionSpecs
  self$defaultSimulatedParameters = specs$defaultSimulatedParameters

  if (any(specs$latentVariables == self$variableName) | any(specs$jointVariables == self$variableName)){
    self$isLatent = TRUE

    # for latent joint distributions--right now for multivariate bernoulli
    if (any(specs$jointVariables == self$variableName)){
      self$isLatentJoint = TRUE

      self$vars = distributionSpecs$vars
      self$nVars = length(self$vars)
      self$nProfiles = 2^self$nVars

      # now, create attribute profile for subvariables
      self$attributeProfile = matrix(data = NA, nrow = self$nProfiles, ncol = self$nVars)
      colnames(self$attributeProfile) = self$vars
      for (profile in 1:self$nProfiles){
        self$attributeProfile[profile, ] = dec2bin(
          decimal_number = profile-1,
          nattributes = self$nVars,
          basevector = rep(2, self$nVars)
        )
      }

    } else {
      self$isLatentJoint = FALSE
    }

  } else {
    self$isLatent = FALSE
    self$isLatentJoint = FALSE
  }

  self$formula = specs$modelVector[[modelNumber]]
  self$formulaRHS = self$formula[-2]

  # currently, mvbernoulli variables cannot be predicted by others # self$predictedBy = rownames(attr(terms(self$formulaRHS), "factors"))
  self$nParam = nrow(self$attributeProfile)
  self$paramNames = paste0(self$variableName, ".prob", apply(X = self$attributeProfile, MARGIN = 1, FUN = function(x) return(paste(x, collapse = ""))))
  self$parameters$probVec = matrix(data = 0, nrow = self$nProfiles, ncol = 1)
  rownames(self$parameters$probVec) = self$paramNames

  if (length(options$defaultInitializeParameters$dirichletAlpha) == 1){
    self$initialParameterValues$probVec = rep(options$defaultInitializeParameters$dirichletAlpha, self$nParam)
  }


  if (length(options$defaultPriors$dirichletAlpha) == 1){
    self$hyperparameters$probVec = rep(options$defaultPriors$dirichletAlpha, self$nParam)
  }


  # determine what variables are predicted by this variable
  self$predicts = NULL
  for (model in 1:length(specs$modelVector)){
    preds = all.vars(terms(specs$newModelVector[[model]])[[3]])
    dv = all.vars(terms(specs$newModelVector[[model]])[[2]])

    if (self$variableName %in% preds){
      self$predicts = c(self$predicts, dv)
    }


  }

  if (is.null(self$predicts)){
    self$isPredictor = FALSE
  } else {
    self$isPredictor = TRUE
  }
  self$predNames = paste0("prob", apply(X = self$attributeProfile, MARGIN = 1, FUN = function(x) return(paste(x, collapse = ""))))


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

  self$allDrawnVariables = c(self$variableName, self$vars)

},

initializeParameters = function(...){
  self$parameters$probVec = t(rdirichlet(n = 1, alpha = self$initialParameterValues$probVec))


},

initializeUnits = function(data, ...){

  # first, draw units from initial alpha values
  data[self$modeledObs, self$variableName] = vapply(
    X = self$modeledObs,
    FUN = function(x)
      return(sample(
        x = 1:self$nProfiles,
        size = 1,
        replace = TRUE,
        prob = self$parameters$probVec
      )),
    FUN.VALUE = as.numeric(1)
  )

  data[self$modeledObs, self$vars] = self$attributeProfile[data[self$modeledObs, self$variableName], self$vars]
  return(data[, self$allDrawnVariables])

},

likelihood = function(data, log = TRUE, ...){


  loglike = log(self$parameters$probVec[data[,self$dataVariableName],1])

  loglike[self$missing] = 0

  return(loglike)

},

prepareData = function(data, ...){


  # nothing is needed for joint variables as data gets created in initial createModelData function
  return(data)

},

provideParameterTypesAndLocations = function(data){

  paramLocation = NULL
  params = NULL
  if (!is.null(self$parameters$probVec)){
    params = rownames(self$parameters$probVec)
    for (j in 1:nrow(self$parameters$probVec)){
      paramLocation = c(paramLocation, paste0('variables[["', self$variableName, '"]]$parameters$probVec[', j, ',1]'))
    }
  }

  paramType = rep(x = "model", times = length(params))

  if (self$nImpute > 0){
    # dataParams = paste0(rownames(data[self$imputeList,]), ".", self$variableName)
    # params = c(params, dataParams)
    #
    # paramType = c(paramType, rep(times = length(dataParams), x = "data"))
    # for (i in 1:length(self$imputeList)){
    #   paramLocation = c(paramLocation, paste0("data$", self$variableName, "[", self$imputeList[i], "]"))
    # }
    for (var in 1:length(self$vars)){
      dataParams = paste0(rownames(data[self$imputeList,]), ".", self$vars[var])
      params = c(params, dataParams)

      paramType = c(paramType, rep(times = length(dataParams), x = "data"))
      for (i in 1:length(self$imputeList)){
        paramLocation = c(paramLocation, paste0("data$", self$vars[var], "[", self$imputeList[i], "]"))
      }
    }
  }

  return(list(paramNames=params, paramTypes=paramType, paramLocation = paramLocation))

},

provideParameterValues = function(){

  temp = list(paramVec = as.numeric(self$parameters$probVec))

  # add each type of parameter separately

  # beta parameters
  temp2 = lapply(X = rownames(self$parameters$probVec),
                 FUN = function(y) return(self$parameters$probVec[which(rownames(self$parameters$probVec) == y)]))
  names(temp2) = self$predNames

  # finally, return the parameter matrices
  temp4 = list(probVec = self$parameters$probVec)

  return(c(temp, temp2, temp4))
},

sampleParameters = function(data, ...){

  # calculate alpha
  alphaVec = self$hyperparameters$probVec + table(factor(x = data[self$modeledObs, self$variableName], levels = paste(1:self$nProfiles)))

  # sample parameters:
  self$parameters$probVec = t(rdirichlet(n = 1, alpha = alphaVec))

  invisible(self)
},

sampleUnits = function(data, variables, ...){
  temp = sampleMultivariateBernoulliUnits2(data = data, variables = variables, varName = self$variableName)
  return(temp)
},


simulateParameters = function(){

  nCategories = self$nProfiles
  simValue = t(eval(parse(text = self$defaultSimulatedParameters$latentJointMultinomial)))

  # check that number of rows in simValue equals that in beta
  if (nrow(simValue) != nrow(self$parameters$probVec))
    stop(
      "\n (blatent error) \n Number of parameters specified for generation of joint
      latent variable distribution does not match number specified by model.
      \n Check input to latentJointMultinomial variable of setDefaultSimulatedParameters function."
    )

  rownames(simValue) = rownames(self$parameters$probVec)
  self$parameters$probVec = simValue

  invisible(self)
},

simulateUnits = function(data, ...){

  predProb = self$parameters$probVec
  classData = as.numeric(sample(x = 1:self$nProfiles, size = self$modeledN, replace = TRUE, prob = predProb))

  tempData = data.frame(cbind(classData, self$attributeProfile[classData,]))
  names(tempData) = self$allDrawnVariables
  return(tempData)
}

  )
)

