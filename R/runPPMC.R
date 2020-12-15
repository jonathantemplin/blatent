runPPMC = function(coreNum, nCores, totalSamples, PPMCmessage, model, fullPosterior, estimableCovariances, dataFunctions){
  # runs PPMC and allows for reporting of messages for progress in parallel runs


  remainder = totalSamples %% nCores
  coreSamples = round(totalSamples/nCores, 0)

  if (coreNum <= remainder) coreSamples = coreSamples+1

  sample = 2
  for (sample in 1:coreSamples){

    eval(PPMCmessage)

    temp = singlePPMC(model = model, fullPosterior = fullPosterior, estimableCovariances = estimableCovariances, dataFunctions = dataFunctions)

    if (sample == 1){
      ppmc = matrix(data = NA, nrow = coreSamples, ncol = length(temp))
      colnames(ppmc) = names(temp)
    }

    ppmc[sample, ] = as.numeric(temp)
  }

  return(ppmc)
}
