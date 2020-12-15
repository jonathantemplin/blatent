setUnderlyingBounds = function(variable, data){

  # reset limits for Gibbs step for parameters
  variable$lowerBoundZ[data[variable$variableName] == 0] = -Inf
  variable$lowerBoundZ[data[variable$variableName] == 1] = 0

  variable$upperBoundZ[data[variable$variableName] == 0] = 0
  variable$upperBoundZ[data[variable$variableName] == 1] = Inf
  return(variable)
}
