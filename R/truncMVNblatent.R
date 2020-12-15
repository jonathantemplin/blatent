truncMVNblatent = function(previous, mu, sigma, D, maxSample = 1000){
  # blatent-specific truncated MVN via rejection sampling... will return previous values if number of samples exceeds maxSample

  sampleNum = 1
  replace = TRUE

  while(sampleNum <= maxSample){
    tempDraw = rmvnorm_Rcpp(n = 1, mu = mu, sigma = sigma)
    check = tempDraw %*% t(D)
    if (all(check >0)){
      replace = FALSE
      break
    } else {
      sampleNum = sampleNum + 1
    }
  }

  if (replace){
    tempDraw = t(previous)
  }

  return(tempDraw)

}
