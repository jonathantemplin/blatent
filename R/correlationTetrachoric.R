
correlationTetrachoric = function(data, correct){
  pairs = t(utils::combn(ncol(data), 2))

  tet = parallel::mcmapply(function(i, j) findTetrachoric(data, i, j), pairs[,1], pairs[,2])
  mat <- diag(ncol(data))
  mat[lower.tri(mat)] <- as.numeric(tet)
  mat <- t(mat) + mat
  diag(mat) <- 1
  rownames(mat) = colnames(mat) = names(data)
  return(mat)
}
