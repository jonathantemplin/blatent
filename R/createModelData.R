createModelData = function(specs){
  data = specs$inputData[specs$observedVariables]

  # check that all variables are numeric type--if not throw error
  varClass = apply(X = data, MARGIN = 2, FUN = class)

  if (any(varClass != "numeric")){
    stop(paste("Observed variables in model are non-numeric in input data:", paste(specs$observedVariables[which(varClass != "numeric")], collapse = " ")))
  }

  # append data with latent variables, if any
  if (specs$nLatents > 0){
    data = cbind(data, matrix(data = 0, nrow = specs$nUnits, ncol = specs$nLatents))
    names(data)[(specs$nObservedVariables+1):(specs$nObservedVariables+specs$nLatents)] = specs$latentVariables
  }

  if (specs$nJointVariables > 0){
    data = cbind(data, matrix(data = 0, nrow = specs$nUnits, ncol = specs$nJointVariables))
    names(data)[(ncol(data)-specs$nJointVariables+1):(ncol(data))] = specs$jointVariables
  }

  return(data)
}
