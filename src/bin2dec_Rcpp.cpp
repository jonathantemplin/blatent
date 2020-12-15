#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int bin2dec_Rcpp(NumericVector binaryVector, int nAttributes, NumericVector baseVector){
  int decNum = 0;
  int elementNum = 0;
  for(int i = 0; i < nAttributes; ++i ){
    elementNum = nAttributes-i-1;
    decNum += binaryVector[elementNum]*(pow(baseVector[elementNum], i));
  }
  return decNum;
}
