#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rks_Rcpp(int n, NumericVector r){
  NumericVector lambda;
  double U;
  // double Y;
  double Y1;
  double Y2;
  double Y3;
  double Z;
  double X;
  double j;
  double H;
  double lU;
  double K;
  double R2;
  int nReps;
  bool ok;
  bool subOK;
  bool broken;
  lambda = rep(0, n);
  broken = false;


  for (int i = 0; i<n; i++){

    ok = false;
    nReps = 0;
    R2 = r[i];
    if (R2 < 1e-7) R2=1e-7;

    while(!ok){
      nReps = nReps+1;

      if (nReps > 10000){
        R2 = R2 + 1e-6;
        nReps = 1;
      }

      Y1 = R::rnorm(0, 1);
      Y2 = pow(Y1,2);
      Y3 = 1 + ((Y2 - sqrt( Y2*(4*R2+Y2) ))/(2*R2));

      U = R::runif(0,1);


      if (U <= (1/(1+Y3))){
        lambda[i] = R2/Y3;
      } else {
        lambda[i] = R2*Y3;
      }


      if (lambda[i] <= 0) lambda[i] = DBL_EPSILON;


      if (lambda[i] > (4/3)){
        U = R::runif(0,1);
        subOK = false;

        Z = 1;
        X = exp(-.5*lambda[i]);

        j = 0;

        while(!subOK){

          j = j+1;

          Z = Z - pow(j+1,2)*pow(X, pow(j+1, 2)-1);

          if (Z > U){
            ok = true;
            subOK = true;
            broken = false;
            break;

          } else {

            j = j + 1;

            Z = Z + pow(j+1,2)*pow(X, pow(j+1, 2)-1);

            if (Z < U){
              ok = false;
              subOK = true;
              broken = false;
              break;
            }

          }
          // if (j > 10000){
          //   Rcout << "\n broken lambda > 4/3\n";
          //   Rcout << "i " << i << "\n";
          //   Rcout << "lambda " << lambda[i] << "\n";
          //   Rcout << "Y1 "  << Y1 << "\n";
          //   Rcout << "Y2 "  << Y2 << "\n";
          //   Rcout << "Y3 "  << Y3 << "\n";
          //   Rcout << "r " << r[i] << "\n";
          //   ok = false;
          //   subOK = true;
          //   broken = true;
          //   break;
          // }
        }

      } else {
        U = R::runif(0,1);
        subOK = false;

        H = .5*log(2.0) + 2.5*log(M_PI) - 2.5*log(lambda[i]) - (pow(M_PI, 2)/(2*lambda[i])) + .5*lambda[i];

        lU = log(U);

        Z=1;

        X = exp(-(pow(M_PI,2)/(2*lambda[i])));

        K = lambda[i] / pow(M_PI,2);

        j = 0;


        while(!subOK){
          j = j+1;


          Z = Z - K*pow(X, pow(j,2)-1);
          double check = H + log(Z);


          if (check > lU){
            ok = true;
            subOK = true;
            broken = false;
            break;

          } else {
            j = j+1;

            Z = Z + pow(j+1, 2)* pow(X, pow(j+1,2)-1);
            check = H + log(Z);

            if (check <= lU){

              ok = false;
              subOK = true;
              broken = false;
              break;
            }

          }
          // if (j > 10000){
          //   Rcout << "\n broken lambda < 4/3\n";
          //   Rcout << "i " << i << "\n";
          //   Rcout << "lambda " << lambda[i] << "\n";
          //   Rcout << "Y1 "  << Y1 << "\n";
          //   Rcout << "Y2 "  << Y2 << "\n";
          //   Rcout << "Y3 "  << Y3 << "\n";
          //   Rcout << "r " << r[i] << "\n";
          //   ok = false;
          //   subOK = true;
          //   broken = true;
          //   break;
          // }
        }
      }

    }
  }
  // if (broken)  Rcout << lambda;
  return lambda;
}


// [[Rcpp::export]]
double testFunc(double x){

  double y = DBL_EPSILON;
  Rcout << DBL_EPSILON;

  y = y/y;
  Rcout << y << "\n";
  Rcout << Rcpp::traits::is_nan<REALSXP>(y);

  if ( Rcpp::traits::is_nan<REALSXP>(y)) y = 10;

  return y;
}
