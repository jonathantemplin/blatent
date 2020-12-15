dec2bin = function(decimal_number, nattributes, basevector){
  dec = decimal_number
  profile = matrix(NA, nrow = 1, ncol = nattributes)
  for (i in nattributes:1){
    profile[1,i] =  dec%%basevector[i]
    dec = (dec-dec%%basevector[i])/basevector[i]
  }
  return(profile)
}
