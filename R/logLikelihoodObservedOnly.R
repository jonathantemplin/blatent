# (O)bserved (Only)
logLikelihoodObservedOnly = function(specs, variables, data) {

  return(rowSums(
    vapply(
      X = specs$observedVariables,
      FUN = function(x, variables, data) {
        return(variables[[x]]$likelihood(
          data = data,
          log = TRUE
        ))
      },
      FUN.VALUE = as.numeric(rep(NA, nrow(data))),
      variables = variables,
      data = data
    ),
    na.rm = TRUE
  ))



}
