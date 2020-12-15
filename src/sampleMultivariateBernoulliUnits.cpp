#include "bernoulli_likelihood.h"
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
DataFrame sampleMultivariateBernoulliUnits(DataFrame data, List variables, String varName){



  List varList = variables[varName];
  List distSpecs = varList["distributionSpecs"];
  List params = varList["parameters"];

  NumericMatrix attributeProfile = varList["attributeProfile"];
  CharacterVector attributeNames = distSpecs["vars"];
  CharacterVector predVars = varList["predicts"];

  // create temporary vector to pass to likelihoood function
  NumericMatrix likelihood(data.nrows(), attributeProfile.nrow());
  NumericVector sumLikelihood(data.nrows());

  // create temporary data frame to protect from changing reference data:
  DataFrame tempData = clone(data);
  NumericVector classDraw(data.nrows()), temp=likelihood.ncol();
  IntegerVector x(attributeProfile.nrow() ) ;
  IntegerVector classNums = seq_along(x);

  // create return vector for holding values drawn
  NumericMatrix preReturnData(data.nrows(), attributeProfile.ncol());
  DataFrame returnData;

  for (int profile = 0; profile < attributeProfile.nrow(); profile++){

    // fill data with profile values
    for (int att = 0; att < attributeNames.length(); att++){
      String attName = attributeNames[att];
      NumericVector attVar = tempData[tempData.findName(attName)];

      std::fill(attVar.begin(), attVar.end(), attributeProfile(profile,att));
    }

    // calculate loglikelihoods for current variable at var=0.0
    arma::mat beta = params["probVec"];
    RObject tempFormula = varList["formulaRHS"];

    // calculate loglikelihoods for each variable in predVars plus varName, too
    for (int i=0; i<predVars.length(); i++){


      String curVar = predVars[i];
      List tempVar = variables[curVar];
      List tempParams = tempVar["parameters"];
      arma::mat beta = tempVar["beta"];
      RObject tempFormula = tempVar["formulaRHS"];

      likelihood(_, profile) = likelihood(_, profile) + bernoulli_likelihood(tempData, beta, tempFormula, curVar);
    }
    likelihood(_, profile) =  exp(likelihood(_,profile));
  }

  for (int unit=0; unit < data.nrows(); unit++){
    sumLikelihood[unit] = sum(likelihood(unit,_));
    likelihood(unit,_) = likelihood(unit,_)/sumLikelihood[unit];
    NumericVector prob = likelihood(unit,_);
    IntegerVector temp = RcppArmadillo::sample(classNums, 1, false, prob);

    classDraw[unit] = classNums[as<int>(temp)-1];
    preReturnData(unit, _) = attributeProfile(as<int>(temp)-1, _);
  }
  returnData = cbind(classDraw, preReturnData);

  return returnData;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
DataFrame sampleMultivariateBernoulliUnits2(DataFrame data, List variables, String varName){

  // information used for calling pointers in variables
  typedef Rcpp::NumericVector (*funcPtr)(DataFrame data, List parameters, RObject formula, String varName);

  List varList = variables[varName];
  List distSpecs = varList["distributionSpecs"];
  List params = varList["parameters"];

  NumericMatrix attributeProfile = varList["attributeProfile"];
  CharacterVector attributeNames = distSpecs["vars"];
  CharacterVector predVars = varList["predicts"];

  // create temporary vector to pass to likelihoood function
  NumericMatrix likelihood(data.nrows(), attributeProfile.nrow());
  NumericVector sumLikelihood(data.nrows());

  // create temporary data frame to protect from changing reference data:
  DataFrame tempData = clone(data);
  NumericVector classDraw(data.nrows()), temp=likelihood.ncol();
  IntegerVector x(attributeProfile.nrow() ) ;
  IntegerVector classNums = seq_along(x);

  // create return vector for holding values drawn
  NumericMatrix preReturnData(data.nrows(), attributeProfile.ncol());
  DataFrame returnData;

  // for initializing on prior probability for class membership
  NumericMatrix beta = params["probVec"];


  for (int profile = 0; profile < attributeProfile.nrow(); profile++){



    // fill data with profile values
    for (int att = 0; att < attributeNames.length(); att++){

      String attName = attributeNames[att];
      NumericVector attVar = tempData[tempData.findName(attName)];

      std::fill(attVar.begin(), attVar.end(), attributeProfile(profile,att));
    }




    // calculate loglikelihoods for each variable in predVars plus varName, too
    for (int i=0; i<predVars.length(); i++){


      String curVar = predVars[i];
      List tempVar = variables[curVar];
      List tempParams = tempVar["parameters"];

      RObject tempFormula = tempVar["formulaRHS"];

      const SEXP& func = tempVar["likelihoodPTR"];

      Rcpp::XPtr<funcPtr> xpfun(func);

      funcPtr fun = *xpfun;

      likelihood(_, profile) = likelihood(_, profile) + fun(tempData, tempParams, tempFormula, curVar);
    }
    likelihood(_, profile) =  exp(likelihood(_,profile));
  }

  for (int unit=0; unit < data.nrows(); unit++){
    sumLikelihood[unit] = sum(likelihood(unit,_));
    likelihood(unit,_) = likelihood(unit,_)/sumLikelihood[unit];
    NumericVector prob = likelihood(unit,_);
    IntegerVector temp = RcppArmadillo::sample(classNums, 1, false, prob);

    classDraw[unit] = classNums[as<int>(temp)-1];
    preReturnData(unit, _) = attributeProfile(as<int>(temp)-1, _);
  }
  returnData = cbind(classDraw, preReturnData);

  return returnData;
}

