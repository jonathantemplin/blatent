initializeClusters = function(model){

  #set number of cores in cluster
  if (model$options$nCores == -1) {
    model$specs$nCores = max(1, parallel::detectCores() - 1)
  } else if (model$options$nCores == 0) {
    model$specs$nCores = parallel::detectCores()
  } else {
    model$specs$nCores = min(model$options$nCores, parallel::detectCores())
  }

  #make cluster
  model$specs$cl = parallel::makeCluster(model$specs$nCores, outfile="", setup_strategy = "sequential")

  #set random seed
  parallel::clusterSetRNGStream(cl = model$specs$cl, iseed = model$options$seed)

  #export packages
  tempResult = parallel::clusterEvalQ(cl = model$specs$cl, library(blatent))

  return(model)
}
