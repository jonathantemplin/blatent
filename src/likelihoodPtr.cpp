// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// User-supplied C++ functions for logf.

// Note that currently the only interface available in rust is
// double fun(const Rcpp::NumericVector& x, const Rcpp::List& pars).
// However, as shown in the function logdmvnorm below RcppArmadillo
// functions can be used inside the function.

// Each function must be prefaced by the line: // [[Rcpp::export]]

// One-dimensional standard normal.

// [[Rcpp::export]]
double logdN01(const Rcpp::NumericVector& x, const Rcpp::List& pars, const Rcpp::DataFrame& data) {
  return (-pow(x[0], 2.0) / 2.0) ;
}

// A function to create external pointers for any of the functions above.
// See http://gallery.rcpp.org/articles/passing-cpp-function-pointers/
// If you write a new function above called new_name then add the following
//
// else if (fstr == "new_name")
//   return(Rcpp::XPtr<funcPtr>(new funcPtr(&new_name))) ;

// [[Rcpp::export]]
SEXP create_xptr(std::string fstr) {
  typedef double (*funcPtr)(const Rcpp::DataFrame& data,
                            const Rcpp::List& parameters,
                            const Rcpp::RObject& formula,
                            const Rcpp::String& varName);
  //if (fstr == "logdN01")
  //return(Rcpp::XPtr<funcPtr>(new funcPtr(&logdN01))) ;
  // else
    // return(Rcpp::XPtr<funcPtr>(R_NilValue)) ;
  return R_NilValue;
}



// We could create the external pointers when this file is sourced using
// the embedded R code below and/or (re)create them using create_xptr() in
// an R session or R package..

/*** R
ptr_N01 <- create_xptr("logdN01")
*/
