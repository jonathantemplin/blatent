
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat rmvnorm_Rcpp(int n, arma::vec mu, arma::mat sigma) {
  int ncols = sigma.n_cols;
  arma::mat Y = arma::randn(n, ncols);
  return arma::repmat(mu, 1, n).t() + Y * arma::chol(sigma);
}

// [[Rcpp::export]]
arma::mat rtmvnormRejection_Rcpp(arma::vec mu, arma::mat sigma, arma::mat D, int maxSample, arma::mat previous) {

  int sampleNum = 1;
  bool replace = true;
  arma::mat X;

  // Rcout << "D " << D << "\n";
  while(sampleNum <= maxSample){
    X = rmvnorm_Rcpp(1, mu, sigma);
    // Rcout << "X " << X << "\n";

    arma::mat check = X * D.t();
    // Rcout << "check " << check << "\n";

    arma::vec testcheck = check.elem( find( check>=0));
    // Rcout << "testcheck " << testcheck << "\n";
    // Rcout << "testcheck.n_rows " << testcheck.n_rows << "\n";
    // Rcout << "D.n_rows " << D.n_rows << "\n";
    // break;
    if (testcheck.n_rows == D.n_rows){
     replace = false;
     break;
    }
    sampleNum = sampleNum+1;
  }
  // Rcout << "sampleNum" << sampleNum << "\n";
  if (replace){
   X = previous;
  }


  return(X);

}
