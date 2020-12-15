itemDiscrimination = function(attributeName, itemVariable, attributeProfile, profileProb){

  # get attribute number
  focalAttribute = which(colnames(attributeProfile) == attributeName)

  # get KLI for item
  itemKLI = itemVariable$KLI(attributeProfile = attributeProfile)

  # for omega_k1
  possibleUprofiles1 = which(attributeProfile[,focalAttribute] == 1)
  possibleVprofiles1 = which(attributeProfile[,focalAttribute] == 0)

  omegaK1 = NULL
  attributeList = 1:ncol(attributeProfile)
  mAttributeList = attributeList[which(attributeList != focalAttribute)]

  D_A1 = 0
  D_B1 = 0
  probSum1 = 0
  for (u in 1:length(possibleUprofiles1)){
    for (v in 1:length(possibleVprofiles1)){

      keep = TRUE
      for (m in 1:length(mAttributeList)){
        if (attributeProfile[possibleUprofiles1[u], mAttributeList[m]] != attributeProfile[possibleVprofiles1[v],mAttributeList[m]]) {
          keep=FALSE
          break
        }

      }

      if (keep){
        omegaK1 = rbind(omegaK1, matrix(data = c(possibleUprofiles1[u], possibleVprofiles1[v]), nrow = 1))
        D_A1 = D_A1 + itemKLI[possibleUprofiles1[u], possibleVprofiles1[v]]
        D_B1 = D_B1 + profileProb[possibleUprofiles1[u]]*itemKLI[possibleUprofiles1[u], possibleVprofiles1[v]]
        probSum1 = probSum1 + profileProb[possibleUprofiles1[u]]
      }

    }
  }

  D_A1 = D_A1/nrow(omegaK1)
  D_B1 = D_B1/probSum1


  # for omega_k0
  possibleUprofiles0 = which(attributeProfile[,focalAttribute] == 0)
  possibleVprofiles0 = which(attributeProfile[,focalAttribute] == 1)

  omegaK0 = NULL
  D_A0 = 0
  D_B0 = 0
  probSum0 = 0
  for (u in 1:length(possibleUprofiles0)){
    for (v in 1:length(possibleVprofiles0)){

      keep = TRUE
      for (m in 1:length(mAttributeList)){
        if (attributeProfile[possibleUprofiles0[u], mAttributeList[m]] != attributeProfile[possibleVprofiles0[v],mAttributeList[m]]) {
          keep=FALSE
          break
        }

      }

      if (keep){
        omegaK0 = rbind(omegaK0, matrix(data = c(possibleUprofiles0[u], possibleVprofiles0[v]), nrow = 1))
        D_A0 = D_A0 + itemKLI[possibleUprofiles0[u], possibleVprofiles0[v]]
        D_B0 = D_B0 + profileProb[possibleUprofiles0[u]]*itemKLI[possibleUprofiles0[u], possibleVprofiles0[v]]
        probSum0 = probSum0 + profileProb[possibleUprofiles0[u]]
      }

    }
  }

  D_A0 = D_A0/nrow(omegaK0)
  D_B0 = D_B0/probSum1

  D_A = D_A0+D_A1
  D_B = D_B0+D_B1



  return(list(D_A = D_A, D_B = D_B, D_A1 = D_A1, D_A0 = D_A0, D_B1 = D_B1, D_B0 = D_B0))
}
