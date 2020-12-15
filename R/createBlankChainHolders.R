createBlankChainHolders = function(specs, options){
  blankChain = matrix(
    data = NA,
    nrow = options$nSampled,
    ncol = length(specs$parameters$paramNames)
  )
  colnames(blankChain) = specs$parameters$paramNames

  return(blankChain)
}
