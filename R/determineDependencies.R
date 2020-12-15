determineDependencies = function(modelVector){
  # convert edgelist into predictor matrix (upper diagonal)
  predictorMatrix = matrix(data = 0, nrow = length(modelVector), ncol = length(modelVector))
  colnames(predictorMatrix) = unlist(lapply(X = modelVector, FUN = function(x) return(all.vars(stats::terms(x)[[2]]))))
  rownames(predictorMatrix) = colnames(predictorMatrix)

  model = 1
  for (model in 1:length(modelVector)){
    lhs = all.vars(stats::terms(modelVector[[model]])[[2]])
    lhs = lhs[which(lhs %in% colnames(predictorMatrix))]

    rhs = all.vars(stats::terms(modelVector[[model]])[[3]])
    rhs = rhs[which(rhs %in% rownames(predictorMatrix))]

    #put conditional as some variables may be exogenous and not in predictorMatrix
    predictorMatrix[rhs, lhs] = 1
  }

  dependList = unlist(lapply(X = modelVector, FUN = function(x) return(all.vars(stats::terms(x)[[2]]))))
  predOrder = NULL

  while (length(dependList) > 0){

    # start with those attributes that are not predicted (exogenous)
    if (is.null(predOrder)){
      predOrder = colnames(predictorMatrix)[which(apply(X = predictorMatrix, MARGIN = 2, FUN = sum) == 0)]
      if (length(predOrder) == 0) stop("error in edgelist: cycle present")

      dependList = dependList[!(dependList %in% predOrder)]
      modelVector[which(apply(X = predictorMatrix, MARGIN = 2, FUN = sum) == 0)] = NULL
    } else {
      # figure out which attributes depend on those in predOrder and add to list if not any other dependencies
      clearList = unlist(lapply(X = modelVector, FUN = function(x){
        if (length(which(all.vars(stats::terms(x)[[3]]) %in% predOrder)) == length(all.vars(stats::terms(x)[[3]]))){
          return(TRUE)
        } else {
          return(FALSE)
        }
      }))

      addList = unlist(lapply(X = modelVector, FUN = function(x) return(all.vars(stats::terms(x)[[2]]))))[clearList]

      if (length(addList) == 0) stop("error in edgelist: cycle present")

      predOrder = c(predOrder, addList)

      dependList = dependList[!(dependList %in% addList)]

      modelVector[which(clearList)] = NULL

    }


  }

  return(predOrder)
}
