createPredProb = function(variable, data) {
  return(stats::pnorm(q = as.numeric(
    Matrix::sparse.model.matrix(variable$formulaRHS, data = data) %*% variable$beta
  )))
}
