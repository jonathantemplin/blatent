#include "bernoulli_likelihood.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sampleBernoulliUnits(DataFrame data, List variables, String varName){

  List varList = variables[varName];

  List paramList = varList["parameters"];
  bool isPred = varList["isPredictor"];


  // create temporary vector to pass to likelihoood function

  NumericVector like0(data.nrows()), like1(data.nrows());

  // create temporary data frame to protect from changing reference data:
  DataFrame tempData = clone(data);
  NumericVector var = tempData[tempData.findName(varName)];


  // fill with zeros first
  var = var*0.0;


  // zero out likelihood vectors
  std::fill(like0.begin(), like0.end(), 0.0);
  std::fill(like1.begin(), like1.end(), 0.0);

  // calculate loglikelihoods for current variable at var=0.0
  arma::mat beta = paramList["beta"];
  RObject tempFormula = varList["formulaRHS"];

  like0 += bernoulli_likelihood(tempData, beta, tempFormula, varName);

  // calculate loglikelihoods for each variable in predVars plus varName, too
  if (isPred){
    CharacterVector predVars = varList["predicts"];
    for (int i=0; i<predVars.length(); i++){
      String curVar = predVars[i];
      List tempVar = variables[curVar];
      List tempParam = tempVar["parameters"];
      arma::mat beta = tempParam["beta"];
      RObject tempFormula = tempVar["formulaRHS"];

      like0 += bernoulli_likelihood(tempData, beta, tempFormula, curVar);
    }
  }


  // calculate loglikelihoods for current variable at var = 1.0

  // fill data with ones
  var = var+1.0;
  like1 += bernoulli_likelihood(data, beta, tempFormula, varName);

  if (isPred){
    CharacterVector predVars = varList["predicts"];
    for (int i=0; i<predVars.length(); i++){
      String curVar = predVars[i];
      List tempVar = variables[curVar];
      List tempParam = tempVar["parameters"];
      arma::mat beta = tempParam["beta"];
      RObject tempFormula = tempVar["formulaRHS"];

      like1 += bernoulli_likelihood(tempData, beta, tempFormula, curVar);
    }
  }

  NumericVector prob = exp(like1)/(exp(like0) + exp(like1));

  NumericVector output(data.nrows());

  for (int i=0; i<data.nrows(); i++){
    output[i] = R::rbinom(1, prob[i]);
  }

  return output;
}

