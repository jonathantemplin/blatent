runChain = function(chainNumber, variables, data, specs, options, chain, message){

  # variables = model$variables
  # data = model$data
  # specs = model$specs

  # reinitialize pointers to generic cpp functions
  temp = lapply(X = variables, FUN = function(x) {x$loadPointers(); return(NULL)})


  # initialize parameters from variables object
  temp = lapply(X = variables, FUN = function(x) {x$initializeParameters(); return(NULL)})


  # initialize data (latent, missing, and underlying variables)
  data = initUnits(data = data, variables = variables, specs = specs)

  totalIter = (chainNumber-1)*specs$iterationsPerChain
  lastIterNum = 0
  iter = 1


  for (iter in 1:(options$nBurnin+options$nSampled*options$nThin)){



    totalIter = totalIter+1
    if (iter %% options$nThin == 0 & iter > options$nBurnin){
      lastIterNum = lastIterNum+1
      iterNum = lastIterNum
    } else {
      iterNum = NA
    }

    eval(message)


    # Gibbs Sampling for missing units step
    data = sampleUnits(data = data, variables = variables, specs = specs)

    # Gibbs for variable distributions step

    temp = lapply(X = variables, FUN = function(x, data) {x$sampleParameters(data = data); return(NULL)}, data = data)

    # add sampled values to chain

    if (!is.na(iterNum)) {

      chain[iterNum,] = unlist(lapply(X = specs$parameters$paramLocation, FUN = function(x){return(eval(parse(text=x)))}))
    }

  }


  # chain = mcmc.list(lapply(X = chain, FUN = coda::mcmc))
  cat("\r")
  return(chain)
}
