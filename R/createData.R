createData = function(variable, data){
  return(c(
    rep(NA, variable$Nmissing),
    stats::rbinom(
      n = variable$N,
      size = 1,
      prob = createPredProb(variable = variable, data = data)
    )
  ))
}
