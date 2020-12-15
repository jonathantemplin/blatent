prepareData = function(variables, data, specs){

  # determine which latent variables are currently in a joint distribution

  # prepare data using each variable's estimator specs
  for (variable in 1:length(variables)){
    data = variables[[variable]]$routines$prepareData(data = data, variable = variable, specs = specs)
  }


  # add observed variables
  nAdded = length(specs$observedVariables)

  data = cbind(data, matrix(
    data = 0,
    nrow = specs$nUnits,
    ncol = nAdded
  ))

  names(data)[(ncol(data) - nAdded + 1):ncol(data)] = paste0("z_", specs$observedVariables)

  # add categorical latent variables not in joint distributions
  if (length(specs$nLatentsNotInJoints) > 0){
    if (specs$nLatentsNotInJoints  > 0){
      nAdded = specs$nLatentsNotInJoints

      data = cbind(data, matrix(
        data = 0,
        nrow = specs$nUnits,
        ncol = nAdded
      ))

      names(data)[(ncol(data) - nAdded + 1):ncol(data)] = paste0("z_", specs$latentsNotInJoints)
    }
  }



  return(data)
}
