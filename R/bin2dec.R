bin2dec = function(binary_vector, nattributes, basevector){
  dec = 0
  for (i in nattributes:1){
    dec = dec + binary_vector[i]*(basevector[i]^(nattributes-i));
  }
  return(dec)
}
