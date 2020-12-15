sampleUnits = function(data, variables, specs){

  # need a loop to ensure data is updated ahead of each successive variable
  for (var in 1:length(specs$predOrder)){

    data[variables[[specs$predOrder[var]]]$allDrawnVariables] =
      variables[[specs$predOrder[var]]]$sampleUnits(data = data, variables = variables)

  }

  return(data)
}
