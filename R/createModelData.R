createModelData = function(specs){
  data = specs$inputData[specs$observedVariables]

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
