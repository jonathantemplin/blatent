getModelParameters <- function(variables, data){

  params = NULL
  paramType = NULL
  paramLocation = NULL
  for (var in variables){
    temp = var$provideParameterTypesAndLocations(data = data)
    params = c(params, temp$paramNames)
    paramType = c(paramType, temp$paramTypes)
    paramLocation = c(paramLocation, temp$paramLocation)
  }

  # start with all parameters that deal with data side

  # reorder list so that model parameters show up first for easy plotting
  params2 = c(params[which(paramType == "model")], params[which(paramType == "data")])
  paramType2 = c(paramType[which(paramType == "model")], paramType[which(paramType == "data")])
  paramLocation2 = c(paramLocation[which(paramType == "model")], paramLocation[which(paramType == "data")])

  return(list(paramNames=params2, paramTypes=paramType2, paramLocation = paramLocation2))
}

