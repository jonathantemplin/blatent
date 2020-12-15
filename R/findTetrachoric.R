
findTetrachoric = function(data, var1, var2, correct = .5){
  # calculates tetrachoric correlation of x with y adding correct to any zero cells
  # borrows heavily from psych package and from polychor package

  x = data[,var1]
  y = data[,var2]

  # convert binary to table
  bin <- x + y * 2 + 1
  dims = c(2, 2)
  table22 <- matrix(tabulate(bin, 4), dims)

  # add correction amount to any zero cells
  if (min(table22) == 0) table22[table22==0] = correct

  # get tau parameters
  prop22 = table22/sum(table22)
  rc <- stats::qnorm(colSums(prop22))[1]
  cc <- stats::qnorm(rowSums(prop22))[1]

  # submit to optimize
  rho <- stats::optimize(tetrachoricLikelihood, interval = c(-1, 1), rc = rc,
                  cc = cc, prop22 = prop22)

  return(rho$minimum)
}
