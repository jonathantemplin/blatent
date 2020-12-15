chainAttributeAnalysis = function(chain, model, type, correct){

  if (model$specs$nJointVariables == 0){

    model$specs$attributeProfile = matrix(data = NA, nrow = 2^model$specs$nLatents, ncol = model$specs$nLatents)
    for (profile in 1:2^model$specs$nLatents){
      model$specs$attributeProfile[profile, ] = dec2bin(decimal_number = profile-1,
                                                        nattributes = model$specs$nLatents,
                                                        basevector = rep(2, model$specs$nLatents))
    }
    colnames(model$specs$attributeProfile) = model$specs$latentVariables

    model$specs$attributeProfile = as.data.frame(model$specs$attributeProfile)



  } else {
    model$specs$attributeProfile = as.data.frame(model$variables[[model$specs$jointVariables[1]]]$attributeProfile)
  }

  # get probability parameter names
  probParams = unlist(lapply(X = 1:nrow(model$specs$attributeProfile),
                             FUN = function(x) return(paste0("prob", paste0(model$specs$attributeProfile[x,], collapse = "")))))


  if ("loglinear" %in% type){
    Xmatrix = stats::model.matrix(object = stats::formula(paste0("~(", paste0(model$specs$latentVariables, collapse = "+"), ")^", length(model$specs$latentVariables))),
                           data = model$specs$attributeProfile)
  }

  chain = 1
  iter = 1
  for (iter in 1:nrow(model$chain[[chain]])){


    if (model$specs$nJointVariables == 0){
      # must create chain probabilities from bayesnet parameters
      model$movePosteriorToVariableBeta(chain =chain, iteration = iter)

      # calculate class probability from product of conditional likelihood for each latent:
      tempProb = matrix(data = NA, nrow = nrow(model$specs$attributeProfile), ncol = ncol(model$specs$attributeProfile))

      latent = 1
      for (latent in 1:model$specs$nLatents){
        tempProb[,latent] = model$variables[[model$specs$latentOrder[latent]]]$routines$likelihood$run(
          data = model$specs$attributeProfile, beta = model$variables[[model$specs$latentOrder[latent]]]$beta, log=FALSE)
      }

      probs = apply(X = tempProb, MARGIN = 1, FUN = prod)

    } else {
      model$specs$attributeProfile = as.data.frame(model$variables[[model$specs$jointVariables[1]]]$attributeProfile)


      # get probabilities
      probs = model$chain[[chain]][iter, probParams]


    }

    # set up chain-based vector of parameters
    if (iter==1){
      attributeChain = NULL
      if ("loglinear" %in% type){

        nLogLinParams = ncol(Xmatrix)-1
        attributeChain = matrix(data = NA, nrow = nrow(model$chain[[chain]]), ncol = nLogLinParams)

        colnames(attributeChain) = paste0(colnames(Xmatrix)[2:ncol(Xmatrix)], "_loglin")
      }

      if ("tetrachoric" %in% type){
        nTetrachoricParams = length(model$specs$latentVariables)*(length(model$specs$latentVariables)-1)/2
        ttcNames = NULL
        for (a1 in 1:(length(model$specs$latentVariables)-1)){
          for (a2 in (a1+1):length(model$specs$latentVariables)){
            ttcNames = paste0(model$specs$latentVariables[a1], ".", model$specs$latentVariables[a2], "_ttc")
          }
        }
        attributeChain = cbind(attributeChain, matrix(data = NA, nrow = nrow(model$chain[[chain]]), ncol = nTetrachoricParams))
        colnames(attributeChain)[(nLogLinParams+1):(nLogLinParams+nTetrachoricParams)] = ttcNames
      }
    }


    if ("loglinear" %in% type){
      logProbs = log(probs)
      logProbs = logProbs - logProbs[1]
      logLinParams = solve(t(Xmatrix) %*% Xmatrix) %*% t(Xmatrix) %*% t(t(logProbs))
      attributeChain[iter,1:nLogLinParams] = logLinParams[2:nrow(logLinParams),1]
    }

    if ("tetrachoric" %in% type){
      rhoVal = 1
      for (a1 in 1:(model$specs$nLatents-1)){
        for (a2 in (a1+1):(model$specs$nLatents)){
          probTable22 = matrix(data = 0, nrow = 2, ncol = 2)
          for (v1 in 0:1){
            for (v2 in 0:1){
              probTable22[v1+1, v2+1] = probs[which(model$specs$attributeProfile[,a1]==v1 & model$specs$attributeProfile[,a2]==v2)]
            }
          }

          if (min(probTable22) == 0){
            probTable22[probTable22==0] = correct
            probTable22 = probTable22/sum(probTable22)
          }

          rc <- stats::qnorm(colSums(probTable22))[1]
          cc <- stats::qnorm(rowSums(probTable22))[1]

          # submit to optimize
          rho <- stats::optimize(tetrachoricLikelihood, interval = c(-1, 1), rc = rc,
                          cc = cc, prop22 = probTable22)

          attributeChain[iter, nLogLinParams+rhoVal] = rho$minimum
        }
      }
    }

  }
  return(attributeChain)
}
