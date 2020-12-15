#' @import mnormt
tetrachoricLikelihood = function (rho, cc, rc, prop22){

  row.cuts <- c(-Inf, rc, Inf)
  col.cuts <- c(-Inf, cc, Inf)
  P <- matrix(0, 2, 2)
  R <- matrix(c(1, rho, rho, 1), 2, 2)
  P[1, 1] <- mnormt::sadmvn(lower = c(row.cuts[1], col.cuts[1]),
                            upper = c(row.cuts[2], col.cuts[2]), mean = c(0, 0), varcov = R)
  P[2, 1] <- stats::pnorm(rc) - P[1, 1]
  P[1, 2] <- stats::pnorm(cc) - P[1, 1]
  P[2, 2] <- 1 - stats::pnorm(rc) - P[1, 2]


  return(-sum(prop22 * log(P)))
}

