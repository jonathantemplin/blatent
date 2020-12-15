#' @title blatentPPMC
#'
#' @description Simulates data using parameters from posterior distribution of blatent Markov chain.
#'
#' @param model A blatent MCMC model object.
#'
#' @param nSamples The number of PPMC samples to be simulated.
#'
#' @param seed The random number seed. Defaults to the seed set in the blatent model object.
#'
#' @param parallel If parallelization should be used in PPMC. Defaults to \code{"TRUE"}.
#'
#' @param nCores If \code{"parallel == TRUE"}, then specifies the number of cores to use. Defaults to four.
#'
#' @param type The type of statistic to generate, submitted as a character vector. Options include:
#' \itemize{
#'    \item \code{"mean"} computes and tabulates the mean for the posterior simulated data for each
#'    observed variable.
#'    \item \code{"covariance"} computes and tabulates the covariance for the posterior simulated data
#'    for all pairs of observed variables.
#'    \item \code{"univariate"} computes and tabulates a Pearson Chi-Square comparing the counts for
#'    an observed variable with the counts for a variable from the posterior simulated data, for each observed variable.
#'    \item \code{"bivariate"}computes and tabulates a Pearson Chi-Square comparing the counts for a
#'     pair of observed variables with the counts for a pair of variables from the posterior simulated
#'     data, for each pair of observed variables.
#'    \item \code{"tetrachoric"} computes and tabulates the tetrachoric correlation for the
#'    posterior simulated data for all pairs of observed variables.
#'    \item \code{"pearson"} computes and tabulates the Pearson correlation for the
#'    posterior simulated data for all pairs of observed variables.
#'    }
#'
#' @param lowPPMCpercentile A vector of the lower bound percentiles used for flagging statistics against PPMC
#'  predictive distributions. Results are flagged if the observed statistics percentile is lower than
#'  the number in the vector. Provided in order of each term in \code{"type"}. Defaults to
#'  \code{"c(.025, .025, 0, 0, .025, .025)"}.
#'
#' @param highPPMCpercentile A vector of the upper bound percentiles used for flagging statistics against PPMC
#'  predictive distributions. Results are flagged if the observed statistics percentile is higher than
#'  the number in the vector. Provided in order of each term in \code{"type"}. Defaults to
#'  \code{"c(.975, .975, 1, 1, .975, .975)"}.
#'
#' @export
blatentPPMC = function(model, nSamples, seed = model$options$seed, parallel = TRUE, nCores = 4,
                       type = c("mean", "covariance", "univariate", "bivariate", "tetrachoric", "pearson"),
                       lowPPMCpercentile = c(.025, .025, 0, 0, .025, .025),
                       highPPMCpercentile = c(.975, .975, 1, 1, .975, .975)){

  modelParams = model$specs$parameters$paramNames[which(model$specs$parameters$paramTypes=="model")]

  # put full posterior into one easily readable matrix
  fullPosterior = list(variables = Reduce(f = "rbind", x = model$chain)[, modelParams])

  dataFunctions = list()

  if ("mean" %in% type){
    propFunction = function(simData, ...){
      proportion =
        apply(
          X = simData,
          MARGIN = 2,
          FUN = mean,
          na.rm = TRUE
        )
      proportion = matrix(data = proportion, nrow = 1)
      colnames(proportion) = paste0(colnames(simData),"_proportion")
      cat("\r")
      return(proportion)
    }
    dataFunctions$mean = propFunction
  }


  if (any(c("covariance", "bivariate", "pearson") %in% type)){

    # check list of pairwise variables with data for correlation
    obsCov = stats::cov(model$data[model$specs$observedVariables], use = "pairwise.complete.obs")

    covEst = sapply(
      X = 1:(nrow(obsCov) - 1),
      FUN = function(x, obsCov)
        if (any(!is.na(obsCov[x, (x + 1):ncol(obsCov)])))
          return((which(!is.na(
            obsCov[x, (x + 1):ncol(obsCov)]
          )) + x)),
      obsCov = obsCov
    )

    # convert list into paired numbers:
    estimableCovariances = Reduce("rbind", lapply(
      X = 1:(nrow(obsCov) - 1),
      FUN = function(x, covEst) {
        temp = cbind(rep(x, length(covEst[[x]])), covEst[[x]])
      },
      covEst = covEst
    ))

    # make list of all estimable covariances
    estimableCovariances = cbind(colnames(obsCov)[estimableCovariances[, 1]], colnames(obsCov)[estimableCovariances[, 2]])

  }

  if ("covariance" %in% type){
    covFunction = function(simData, estimableCovariances, ...){
      covEst = stats::cov(simData, use = "pairwise.complete.obs")
      covariance = mapply(
        FUN = function(x, y, covEst)
          return(covEst[x, y]),
        estimableCovariances[, 1],
        estimableCovariances[, 2],
        MoreArgs = list(covEst = covEst)
      )
      covariance = matrix(data = covariance, nrow = 1)
      colnames(covariance) = paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_covariance")

      return(covariance)
    }

    dataFunctions$covariance = covFunction
  }

  if ("pearson" %in% type){
    corFunction = function(simData, estimableCovariances, ...){
      corEst = stats::cor(simData, use = "pairwise.complete.obs")
      correlation = mapply(
        FUN = function(x, y, corEst)
          return(corEst[x, y]),
        estimableCovariances[, 1],
        estimableCovariances[, 2],
        MoreArgs = list(corEst = corEst)
      )
      correlation = matrix(data = correlation, nrow = 1)
      colnames(correlation) = paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_pearson")

      return(correlation)
    }

    dataFunctions$pearson = corFunction
  }

  if ("tetrachoric" %in% type){
    tetraFunction = function(simData, realData, estimableCovariances){
      result = correlationTetrachoric(data = simData)

      tetrachor = mapply(
        FUN = function(x, y, result)
          return(result[x, y]),
        estimableCovariances[, 1],
        estimableCovariances[, 2],
        MoreArgs = list(result = result)
      )
      tetrachor = matrix(data = tetrachor, nrow = 1)
      colnames(tetrachor) = paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_tetrachoric")
      return(tetrachor)
    }
    dataFunctions$tetrachoric = tetraFunction
  }


  if ("univariate" %in% type){
    univarFunction = function(simData, realData, ...){
      result = unlist(lapply(
        X = names(simData),
        FUN = function(x)
          return(
            itemChiSquare(observed = realData[,x], observedLevels = list(c(0,1)),
                                    expected = simData[,x], expectedLevels = list(c(0,1)), minObs = 0, minExp = 0)
          )
      ))
      univariate = c(result, sum(result))
      univariate = matrix(data = univariate, nrow = 1)
      colnames(univariate) = c(paste0(colnames(simData),"_univariate"), "totalUnivariate")
      univariate = round(univariate, digits = 4)
      return(univariate)
    }
    dataFunctions$univariate = univarFunction
  }
  if ("bivariate" %in% type){
    bivarFunction = function(simData, realData, estimableCovariances){
      bivariate = mapply(
        FUN = function(x, y, data, simData){
          # return(c(x,y))
          return(itemChiSquare(observed = data[c(x,y)], observedLevels = list(c(0,1), c(0,1)),
                                         expected = simData[,c(x,y)], expectedLevels = list(c(0,1), c(0,1)), minObs = 0, minExp = 0))
        }
        ,
        estimableCovariances[, 1],
        estimableCovariances[, 2],
        MoreArgs = list(data = model$data, simData = simData)
      )
      bivariate = c(bivariate, sum(bivariate))
      bivariate = matrix(data = bivariate, nrow = 1)
      colnames(bivariate) = c(paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_bivariate"), "totalBivariate")
      return(bivariate)
    }
    dataFunctions$bivariate = bivarFunction
  }

  PPMCmessage  = quote(if (coreNum == 1) cat("\r", paste0("Progress: ", sprintf("%03.1f",round((sample/coreSamples)*100), digits = 1)), "% Complete"))

  if (parallel){

    #make cluster
    cl = parallel::makeCluster(nCores, outfile="", setup_strategy = "sequential")

    #set random seed
    parallel::clusterSetRNGStream(cl = cl, iseed = seed)

    #export packages
    # tempResult = parallel::clusterEvalQ(cl = cl, library(blatent))
    parallel::clusterExport(
      cl = cl,
      varlist = c("model", "fullPosterior", "estimableCovariances"),
      envir = environment()
    )

    chain = parallel::parLapply(
      cl = cl,
      X = 1:nCores,
      fun = runPPMC,
      nCores = nCores,
      totalSamples = nSamples,
      PPMCmessage = PPMCmessage,
      model = model,
      fullPosterior = fullPosterior,
      estimableCovariances = estimableCovariances,
      dataFunctions = dataFunctions
    )
    parallel::stopCluster(cl = cl)
    chain = Reduce(f = "rbind", x = chain)
  } else {
    set.seed(seed)

    coreNum = 1
    nCores = 1
    totalSamples = nSamples

    chain = runPPMC(coreNum = 1, nCores = 1, totalSamples = nSamples, PPMCmessage = PPMCmessage,
                    model = model, fullPosterior = fullPosterior, estimableCovariances = estimableCovariances, dataFunctions = dataFunctions)

  }




  # post-processing PPMC data:

  # run dataFunctions on real data for point estimates
  dataResults = lapply(
    X = dataFunctions,
    FUN = function(x,
                   simData,
                   realData,
                   estimableCovariances)
      return(
        x(
          simData = simData,
          realData = realData,
          estimableCovariances = estimableCovariances
        )
      ),
    simData = model$data[model$specs$observedVariables],
    realData = model$data,
    estimableCovariances = estimableCovariances
  )
  dataResultsMatrix = do.call("cbind", dataResults)

  # separate PPMC by type
  PPMCresults = list()
  if ("mean" %in% type){
    PPMCresults$mean = list(samples = chain[,grep(pattern = "_proportion", x = colnames(chain))])
    PPMCresults$mean$dataResults = dataFunctions$mean(simData = model$data[model$specs$observedVariables])
  }

  if ("univariate" %in% type){
    PPMCresults$univariate = list(samples = chain[,c(grep(pattern = "_univariate", x = colnames(chain)),
                                                     grep(pattern = "totalUnivariate", x = colnames(chain)))])
    PPMCresults$univariate$dataResults = dataFunctions$univariate(simData = model$data[model$specs$observedVariables],
                                                      realData = model$data[model$specs$observedVariables])
  }

  if ("covariance" %in% type){
    PPMCresults$covariance = list(samples = chain[,grep(pattern = "_covariance", x = colnames(chain))])
    if (!is.matrix(PPMCresults$covariance$samples)){
      PPMCresults$covariance$samples = matrix(data = PPMCresults$covariance$samples, ncol = 1)
      colnames(PPMCresults$covariance$samples) = paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_covariance")
    }
    PPMCresults$covariance$dataResults = dataFunctions$covariance(
      simData = model$data[model$specs$observedVariables],
      realData = model$data[model$specs$observedVariables],
      estimableCovariances = estimableCovariances
    )
  }

  if ("pearson" %in% type){
    PPMCresults$pearson = list(samples = chain[,grep(pattern = "_pearson", x = colnames(chain))])
    if (!is.matrix(PPMCresults$pearson$samples)){
      PPMCresults$pearson$samples = matrix(data = PPMCresults$pearson$samples, ncol = 1)
      colnames(PPMCresults$pearson$samples) = paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_pearson")
    }
    PPMCresults$pearson$dataResults = dataFunctions$pearson(
      simData = model$data[model$specs$observedVariables],
      realData = model$data[model$specs$observedVariables],
      estimableCovariances = estimableCovariances
    )
  }

  if ("bivariate" %in% type){
    PPMCresults$bivariate = list(samples = chain[,c(grep(pattern = "_bivariate", x = colnames(chain)),
                                                     grep(pattern = "totalBivariate", x = colnames(chain)))])
    PPMCresults$bivariate$dataResults = dataFunctions$bivariate(
      simData = model$data[model$specs$observedVariables],
      realData = model$data[model$specs$observedVariables],
      estimableCovariances = estimableCovariances
    )
  }

  if ("tetrachoric" %in% type){
    PPMCresults$tetrachoric = list(samples = chain[,grep(pattern = "_tetrachoric", x = colnames(chain))])
    if (!is.matrix(PPMCresults$tetrachoric$samples)){
      PPMCresults$tetrachoric$samples = matrix(data = PPMCresults$tetrachoric$samples, ncol = 1)
      colnames(PPMCresults$tetrachoric$samples) = paste0(estimableCovariances[,1], "_", estimableCovariances[,2], "_tetrachoric")
    }
    PPMCresults$tetrachoric$dataResults = dataFunctions$tetrachoric(
      simData = model$data[model$specs$observedVariables],
      realData = model$data[model$specs$observedVariables],
      estimableCovariances = estimableCovariances
    )
  }

  # calculate posterior predictive p-values for each
  PPMCresults = lapply(X = PPMCresults, FUN = function(x){
    x$quantiles = data.frame(matrix(data = NA, nrow = ncol(x$samples), ncol = 9))
    names(x$quantiles)[1] = "Statistic"
    names(x$quantiles)[2:7] = names(summary(stats::ecdf(x = chain[,1])))
    names(x$quantiles)[8:9] = c("ObservedStatistic", "ObsStatPctile")
    x$quantiles[,1] = colnames(x$dataResults)

    col = 1
    for (col in 1:ncol(x$samples)){
      colECDF = stats::ecdf(x = x$samples[,col])
      x$quantiles[col,2:9] = c(summary(colECDF), x$dataResults[1,col], colECDF(x$dataResults[1,col]))
    }
    return(x)
  })


  ppmc = 1
  for (ppmc in 1:length(type)){

    # flag results in extremes:
    PPMCresults[[ppmc]]$extremes =
      PPMCresults[[ppmc]]$quantiles[which(PPMCresults[[ppmc]]$quantiles[,9] > highPPMCpercentile[ppmc] |
                                            PPMCresults[[ppmc]]$quantiles[,9] < lowPPMCpercentile[ppmc]),]
    ppmcType = names(PPMCresults)[ppmc]
    # format for numeric values
    numDigits = 3
    num.format  <-
      paste("%", max(8L, numDigits + 5L), ".", numDigits, "f", sep = "")
    num.format0  <-
      paste("%", max(8L, 0 + 5L), ".", 0, "f", sep = "")

    char.format <-
      paste("%", max(8L, numDigits + 5L), "s", sep = "")
    # determine results headlines for summary
    if (ppmcType == "univariate"){
      # here, message is median chi-square -- using output format from classBlatentModel
      preamble = paste0("  ", "Median Total Univariate PPMC Chi-Square", collapse = "")

      paramVals = paste0(sprintf(num.format, PPMCresults[[ppmc]]$quantiles[which(PPMCresults[[ppmc]]$quantiles[,1] == "totalUnivariate"), 4], collapse = ""))
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
      PPMCresults[[ppmc]]$summaryMessage = paste0(preamble, buffer, paramVals, collapse = "")

    } else if (ppmcType == "bivariate"){
      # here, message is number of statistics out of total that are extreme
      preamble = paste0("  ", "Median Total Bivariate PPMC Chi-Square", collapse = "")

      paramVals = paste0(sprintf(num.format, PPMCresults[[ppmc]]$quantiles[which(PPMCresults[[ppmc]]$quantiles[,1] == "totalBivariate"), 4], collapse = ""))
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
      PPMCresults[[ppmc]]$summaryMessage = paste0(preamble, buffer, paramVals, collapse = "")
    } else {
      # here, message is number of statistics out of total that are extreme
      preamble = paste0("  ", "Extreme PPMC Percentiles: ", ppmcType, " (out of ", nrow(PPMCresults[[ppmc]]$quantiles), ")", collapse = "")

      paramVals = paste0(sprintf(num.format0, nrow(PPMCresults[[ppmc]]$extremes), collapse = ""))
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
      PPMCresults[[ppmc]]$summaryMessage = paste0(preamble, buffer, paramVals, collapse = "")
    }

  }

  return(PPMCresults)
}
