rks = function(n, r){
  # algorithm from Holmes and Held (2006), p. 164 (Appendix A4)
  # rejection sampling from KS distribution
  # here, r = (z_i - x_i %*% beta)^2

  lambdaOK = rep(FALSE, n)
  lambda = Y =  U = lambdaTry = rep(NA, n) # initialize objects for the loop below

  while (any(!lambdaOK)){
    nRemaining = n-length(which(lambdaOK))

    Y[which(!lambdaOK)] = stats::rnorm(n = nRemaining, mean = 0, sd = 1)
    Y[which(!lambdaOK)] = Y[which(!lambdaOK)]^2

    Y[which(!lambdaOK)] = 1 +
      (Y[which(!lambdaOK)] - sqrt( Y[which(!lambdaOK)]*( 4*r[which(!lambdaOK)]+Y[which(!lambdaOK)] ) ) )/
      (2*r[which(!lambdaOK)])

    if (any(Y[which(!lambdaOK)]==0)) Y[which(!lambdaOK)] = .Machine$double.eps

    U[which(!lambdaOK)] = stats::runif(n = nRemaining, min = 0, max = 1)

    lambdaTry[which(!lambdaOK)] = rep(NA, nRemaining)

    # if (any(!lambdaOK) & any(U <= (1/(1+Y)))){
      if (any(!lambdaOK & U <= (1/(1+Y)))){
        lambdaTry[which(!lambdaOK & U<= (1/(1+Y)))] =
          r[which(!lambdaOK & U<= (1/(1+Y)))]/Y[which(!lambdaOK & U<= (1/(1+Y)))]
      }
    # }

    # if (any(!lambdaOK) & any(U > (1/(1+Y)))){
      if (any(!lambdaOK & U > (1/(1+Y)))){
        lambdaTry[which(!lambdaOK & U > (1/(1+Y)))] =
          r[which(!lambdaOK & U> (1/(1+Y)))]*Y[which(!lambdaOK & U> (1/(1+Y)))]
      }
    # }


    U[which(!lambdaOK)] = stats::runif(n = nRemaining, min = 0, max = 1)

    if (any(!lambdaOK)){
      if (any(lambdaTry[which(!lambdaOK)] > (4/3))){
        lambdaOK[which(!lambdaOK & lambdaTry > (4 / 3))] =
          rksRightmostInterval(uStar = U[which(!lambdaOK & lambdaTry > (4 / 3))],
                               lambdaStar = lambdaTry[which(!lambdaOK & lambdaTry > (4 / 3))])
      }
    }

    if (any(!lambdaOK)){
      if (any(lambdaTry[which(!lambdaOK)] <= (4/3))){
        lambdaOK[which(!lambdaOK & lambdaTry <= (4/3))] =
          rksLeftmostInterval(uStar = U[which(!lambdaOK & lambdaTry <= (4/3))],
                              lambdaStar = lambdaTry[which(!lambdaOK & lambdaTry <= (4/3))])
      }
    }

  }

  return(lambdaTry)
}

rksRightmostInterval = function(uStar, lambdaStar){

  Zdone = rep(FALSE, length(uStar))

  Z = rep(1, length(lambdaStar))
  X = exp(-.5*lambdaStar)

  j = 0

  Zok = rep(NA, length(uStar))

  while (any(!Zdone)){
    j = j+1

    Z[which(!Zdone)] = Z[which(!Zdone)]  - ( (j+1)^2) * (X[which(!Zdone)]^( ((j+1)^2) - 1))

    if (any(which(!Zdone & Z > uStar))){
      Zok[which(!Zdone & Z > uStar)] = TRUE
      Zdone[which(!Zdone & Z > uStar)] = TRUE
    }


    if (any(!Zdone)){
      j = j+1

      Z[which(!Zdone)] = Z[which(!Zdone)]  + ( (j+1)^2) * (X[which(!Zdone)]^( ((j+1)^2) - 1))

      if (any(which(!Zdone & Z < uStar))){
        Zok[which(!Zdone & Z < uStar)] = FALSE
        Zdone[which(!Zdone & Z < uStar)] = TRUE

      }

    }

  }

  return(Zok)
}


rksLeftmostInterval = function(uStar, lambdaStar){

  H = .5*log(2) + 2.5*log(pi) - 2.5*(log(lambdaStar)) - ((pi^2)/(2*lambdaStar)) + .5*lambdaStar

  lU = log(uStar)

  Z = rep(1, length(uStar))

  X = exp((-1*pi^2)/(2*lambdaStar))

  K = lambdaStar/(pi^2)

  j = 0

  Zok = rep(NA, length(uStar))
  Zdone = rep(FALSE, length(uStar))

  while(any(!Zdone)){
    j = j+1

    Z[which(!Zdone)] = Z[which(!Zdone)] - K[which(!Zdone)]*(X[which(!Zdone)]^((j^2)-1))



    if (any(which(!Zdone & (H + log(Z)) > lU))){
      Zok[which(!Zdone & (H + log(Z)) > lU)] = TRUE
      Zdone[which(!Zdone & (H + log(Z)) > lU)] = TRUE
    }

    if (any(!Zdone)){
      j = j+1

      Z[which(!Zdone)] = Z[which(!Zdone)] + ( ( (j+1)^2) * (X[which(!Zdone)]^( (((j+1)^2)-1)) ) )

      if (any(which(!Zdone & (H + log(Z)) < lU))){
        Zok[which(!Zdone & (H + log(Z)) < lU)] = FALSE
        Zdone[which(!Zdone & (H + log(Z)) < lU)] = TRUE
      }
    }



  }
  return(Zok)

}
