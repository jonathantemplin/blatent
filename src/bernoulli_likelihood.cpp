#include <RcppArmadillo.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector bernoulli_likelihood(DataFrame data, arma::mat beta, RObject formula, String varName) {

  Rcpp::Function modelmatrix("model.matrix");

  NumericMatrix linPred = modelmatrix(formula, data) ;
  NumericVector var = data[data.findName(varName)];

  arma::mat y = arma::mat(linPred.begin(), linPred.nrow(), linPred.ncol(), false) * beta;

  y = exp(y)/(1+exp(y));

  NumericVector result = y.n_rows;

  for (int i=0; i< y.n_rows; i++){
    result[i] = log(pow(y(i), var(i))*pow(1.0-y(i), 1.0-var(i)));
  }


  return result;
}


// [[Rcpp::export]]
Rcpp::NumericVector bernoulli_likelihood2(DataFrame data, List parameters, RObject formula, String varName) {

  Rcpp::Function modelmatrix("model.matrix");
  arma::mat beta = parameters["beta"];

  NumericMatrix linPred = modelmatrix(formula, data) ;
  NumericVector var = data[data.findName(varName)];

  arma::mat y = arma::mat(linPred.begin(), linPred.nrow(), linPred.ncol(), false) * beta;

  y = exp(y)/(1+exp(y));

  NumericVector result = y.n_rows;

  for (int i=0; i< y.n_rows; i++){
    result[i] = log(pow(y(i), var(i))*pow(1.0-y(i), 1.0-var(i)));
  }


  return result;

}

// [[Rcpp::export]]
SEXP bernoulliLikelihoodPtr(std::string fstr) {
  typedef Rcpp::NumericVector (*funcPtr)(DataFrame data, List parameters, RObject formula, String varName);
//const Rcpp::DataFrame& data,
// const Rcpp::List& parameters,
// const Rcpp::RObject& formula,
// const Rcpp::String& varName
  if (fstr == "bernoulli")
    return(Rcpp::XPtr<funcPtr>(new funcPtr(&bernoulli_likelihood2))) ;
  else
    return R_NilValue;
}


// [[Rcpp::export]]
Rcpp::NumericVector testingLikeFunc(const SEXP& func, DataFrame data, List parameters, RObject formula, String varName){
  typedef Rcpp::NumericVector (*funcPtr)(DataFrame data, List parameters, RObject formula, String varName);
  Rcpp::XPtr<funcPtr> xpfun(func);

  funcPtr fun = *xpfun;

  Rcpp::NumericVector val;

  val = fun(data, parameters, formula, varName);
  return val;

}
