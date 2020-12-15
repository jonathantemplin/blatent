#ifndef BERNOULLI_LIKELIHOOD_H
#define BERNOULLI_LIKELIHOOD_H

#include <RcppArmadillo.h>
Rcpp::NumericVector bernoulli_likelihood(Rcpp::DataFrame data, arma::mat beta, Rcpp::RObject formula, Rcpp::String varName);

#endif
