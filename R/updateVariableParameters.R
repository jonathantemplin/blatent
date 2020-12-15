updateVariableParameters = function(variable, data){

  variable$beta = variable$routines$sampleParameters$run(data = data, variable$beta)

  return(variable)
}
