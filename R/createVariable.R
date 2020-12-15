createVariable = function(modelNumber, specs, data, options){

  # temporary input information
  currentVariable = as.character(stats::terms(specs$modelVector[[modelNumber]])[[2]])


  # determine which distribution and need for constraints for variable is present

  if (any(specs$latentVariables == currentVariable) | any(specs$jointVariables == currentVariable)) {


    if (any(specs$jointVariables == currentVariable)){
      distributionSpecs = eval(parse(text = specs$jointInput[which(names(specs$jointInput) == currentVariable)]))
    } else {
      distributionSpecs = eval(parse(text = specs$latentsInput[which(names(specs$latentsInput) == currentVariable)]))
    }




  } else {

    distributionSpecs = eval(parse(text = specs$distributionsInput[which(names(specs$distributionsInput) == currentVariable)]))

  }


  if (distributionSpecs$distribution == "bernoulli") {
    variable = classVariable_Bernoulli$new(
      modelNumber = modelNumber,
      specs = specs,
      data = data,
      options = options,
      distributionSpecs = distributionSpecs
    )
  } else if (distributionSpecs$distribution == "mvbernoulli"){
    variable = classVariable_MultivariateBernoulli$new(
      modelNumber = modelNumber,
      specs = specs,
      data = data,
      options = options,
      distributionSpecs = distributionSpecs
    )
  }

  return(variable)
}
