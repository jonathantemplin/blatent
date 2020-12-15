
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
List getMeanAndCov_HH2006L1(arma::mat X, arma::mat W, arma::mat Y, arma::mat invBetaCov, arma::mat invBetaCovBetaMean) {

  arma::mat cov = arma::solve(invBetaCov + trans(X) * W * X, arma::eye(invBetaCov.n_cols, invBetaCov.n_cols));
  arma::mat mean = cov * (invBetaCovBetaMean + trans(X) * W * Y);

  return List::create(_["mean"] = mean, _["cov"] = cov);
}
