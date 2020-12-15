
initializeSpecs = function(modelText,
                           dataMat,
                           options) {

  specs = list(
    calculateDIC = options$calculateDIC,
    StartTime = Sys.time(),
    EndTime = NULL,
    modelText = modelText,
    latentVariables = NULL,
    parameters = NULL,
    inputData = dataMat,
    priorsList = NULL,
    posteriorPredictiveChecks = options$posteriorPredictiveChecks,
    cl = NULL
  )

  parsedText = parseModelSyntax(modelText = modelText)
  specs$distributionsInput = parsedText$distributions
  specs$latentsInput = parsedText$latents

  specs$jointInput = parsedText$latentJointDistInfo

  # get lists variables from parsedText
  if (length(parsedText$latents) > 0){
    specs$latentVariables = names(parsedText$latents)
    specs$nLatentVariables = length(parsedText$latents)
  } else {
    specs$latentVariables = NULL
    specs$nLatentVariables = 0
  }

  specs$latentsNotInJoints = NULL
  if (length(specs$jointInput)>0){
    specs$jointVariables = names(parsedText$latentJointDistInfo)
    specs$nJointVariables = length(parsedText$latentJointDistInfo)

    # determine latent variables in each joint distribution
    specs$joints = list()
    for (i in 1:length(specs$jointInput)){
      temp = eval(parse(text = specs$jointInput[i]))
      specs$joints[[names(specs$jointInput[i])]] = temp$vars
    }

    specs$nLatentsNotInJoints = length(which(!specs$latentVariables %in% unlist(specs$joints)))
    if (specs$nLatentsNotInJoints > 0){
      specs$latentsNotInJoints = specs$latentVariables[which(!specs$latentVariables %in% unlist(specs$joints))]
    } else {
      specs$latentsNotInJoints = NULL
    }


  } else {
    specs$jointVariables = NULL
    specs$nJointVariables = 0
    specs$joints = NULL
    specs$nLatentsNotInJoints = specs$nLatentVariables
    specs$latentsNotInJoints = specs$latentVariables
  }

  # build temporary specs for latents as these are needed in model specification
  specs$latentsParsed = lapply(X = specs$latentsInput, FUN = function(x) return(eval(parse(text = x))))

  # build attribute profile if any latent variables are ordinal categorical
  if (any(lapply(X = specs$latentsParsed, FUN = function(x) return(x$type)) == "ordinal")){
    specs$orderedLatents = names(which(lapply(X = specs$latentsParsed, FUN = function(x) return(x$type)) == "ordinal"))
    specs$nOrderedLatents = length(specs$orderedLatents)

    specs$attributeProfile = matrix(data = NA, nrow = 2^specs$nOrderedLatents, ncol = specs$nOrderedLatents)

    # build attribute profile
    for (profile in 1:2^specs$nOrderedLatents){
      specs$attributeProfile[profile,] = dec2bin(
        decimal_number = profile - 1,
        nattributes = specs$nOrderedLatents,
        basevector = rep(2, specs$nOrderedLatents)
      )
    }
  }



  specs$allDVs = unlist(lapply(X = parsedText$modelVector, FUN = function(x) return(as.character(stats::terms(x)[[2]]))))
  specs$allPredictors = lapply(X = parsedText$modelVector, FUN = function(x) return(all.vars(stats::terms(x)[[3]])))
  specs$allVariables = unique(c(unlist(specs$allDVs), unlist(specs$allPredictors)))


  specs$observedVariables = specs$allVariables[which(!(specs$allVariables %in% c(specs$latentVariables, specs$jointVariables)))]
  specs$nObservedVariables = length(specs$observedVariables)
  specs$modelVector = parsedText$modelVector

  specs$nUnits = nrow(specs$inputData)

  # unitList used for reporting unit results for latent variables
  specs$unitList = as.character(1:nrow(specs$inputData))

  # get specs for Latent Variables (used in generation)
  specs$latentVector =
    specs$modelVector[
      unlist(lapply(X = specs$allDVs, FUN = function(x) {
        if (any(x %in% c(specs$latentVariables, specs$jointVariables))) {
          return(TRUE)
        } else {
          return(FALSE)}
      }))]
  if (specs$nLatentVariables > 0 ) specs$latentOrder = determineDependencies(modelVector = specs$latentVector)

  specs$nLatents = length(specs$latentVariables) # until differentiable latents are included
  specs$nCategoricalLatents = length(specs$latentVariables)

  # get order for prediction based on dependencies in graph -- if joint categorical latent, have to remove categorical latents from models first
  if (length(specs$jointInput) > 0) {
    # right now only building for categorical latents--have to fix for other types later
    newModelVector = list()
    for (model in 1:length(specs$modelVector)){
      # get predictors  in model:
      modelPreds = all.vars(stats::terms(specs$modelVector[[model]])[[3]])
      modelDV = as.character(attr(stats::terms(specs$modelVector[[model]]), "variables")[-1])[attr(stats::terms(specs$modelVector[[model]]), "response")]

      # replace predictors that are included in joint distribution with name of joint distribution
      if (length(modelPreds) > 0){
        for (pred in 1:length(modelPreds)){
          if (modelPreds[pred] %in% names(parsedText$latentJointDists)){
            modelPreds[pred] = parsedText$latentJointDists[which(names(parsedText$latentJointDists) == modelPreds[pred])]
          }
        }
      } else {
        modelPreds = "1"
      }


      # remove duplicated predictors, if any
      modelPreds = unique(modelPreds)

      # create new formula
      newModelVector[[model]] = stats::formula(paste0(modelDV, "~", paste(modelPreds, collapse = "+")))

    }
    specs$predOrder = determineDependencies(modelVector = newModelVector)
    specs$newModelVector = newModelVector
  } else {
    specs$predOrder = determineDependencies(modelVector = specs$modelVector)
    specs$newModelVector = NULL
  }


  # get order for observed variables only
  specs$obsOrder = specs$predOrder[which(specs$predOrder %in% specs$observedVariables)]

  # get total iterations per chain
  specs$iterationsPerChain = options$nBurnin + options$nSampled*options$nThin
  specs$iterationsTotal = specs$iterationsPerChain*options$nChains
  specs$underlyingVariables = NULL

  #parse priors (if any)
  if (!is.null(parsedText$priors)){
    #specs$priorsList = sapply(X = priors, FUN = parsePriors, simplify = FALSE)
  }

  # check for options$missingMethod being accurate
  if (!options$missingMethod %in% c("omit", "imputeBayes")){
    stop(paste0("options$missingMethod must be either 'omit' or 'imputeBayes'. Input given: ", options$missingMethod))
  }

  return(specs)
}
