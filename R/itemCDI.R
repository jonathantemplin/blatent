itemCDI = function(itemVariable, attributeProfile){
  # get KLI for item
  itemKLI = itemVariable$KLI(attributeProfile = attributeProfile)

  # CDI Calculation
  CDI = 0
  sumHinv = 0
  for (u in 1:nrow(itemKLI)){
    for (v in 1:nrow(itemKLI)){
      if (u != v){
        h = sum((attributeProfile[u, ] - attributeProfile[v, ])^2)
        hInv = 1/h
        sumHinv = sumHinv + hInv
        CDI = CDI + hInv*itemKLI[u,v]
      }
    }
  }
  CDI = (1/sumHinv)*CDI

  return(CDI)

}
