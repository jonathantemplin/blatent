buildModelDataMatrix <- function(dataMat, specs){


  # check that all observed variables are in dataMat
  if (length(which(!(specs$observedVariables %in% names(dataMat)))) > 0){
    stop(paste("Error: Observed Variables", specs$observedVariables[which(!(specs$observedVariables %in% names(dataMat)))], " not found in input data."))
  }

  # initialize matrix for data for latent variables
  if (specs$nLatentVariables > 0){
    latentData = matrix(data = 0, nrow = nrow(dataMat), ncol = specs$nLatentVariables)
    colnames(latentData) = specs$latentVariables
  } else {
    latentData = NULL
  }


  # put observed and latent together
  modelData = data.frame(cbind(dataMat[specs$observedVariables], latentData))

  return(modelData)

}
