#' @import R6
#' @import coda
blatentModel <-
  R6::R6Class(
    classname = "blatentModel",
    public = list(
# class variables =================================================================================================================
      attributeAnalyses = NULL,
      chain = NULL,
      data = NULL,
      dataParameterSummary = NULL,
      estimatedLatentVariables = NULL,
      informationCriteria = NULL,
      logLikelihoods = NULL,
      options = NULL,
      parameterSummary = NULL,
      PPMC = NULL,
      specs = NULL,
      variables = NULL,

# Public Functions ==========================================================================================================
analyzeCategoricalStructuralModel = function(type = c("loglinear", "tetrachoric"), correct = .01){
  # transforms iterations of structural model probabilities into specific values, provides summary
  if (self$specs$nCategoricalLatents != 0){

    if (self$options$parallel){
      #make cluster
      cl = parallel::makeCluster(self$options$nCores, outfile="", setup_strategy = "sequential")

      #export packages

      parallel::clusterExport(
        cl = cl,
        varlist = c("self", "type", "correct"),
        envir = environment()
      )

      self$attributeAnalyses$chain = parallel::parLapply(
        cl = cl,
        X = 1:length(self$chain),
        fun = chainAttributeAnalysis,
        model = self,
        type = type,
        correct = correct
      )

      parallel::stopCluster(cl = cl)

    } else {
      self$attributeAnalyses$chain = lapply(
        X = 1:length(self$chain),
        FUN = chainAttributeAnalysis,
        model = self,
        type = type,
        correct = correct
      )
    }

    self$attributeAnalyses$summary = chainSummary(chain = self$attributeAnalyses$chain,
                                                  HDPIntervalValue = self$options$HDPIntervalValue)
    self$attributeAnalyses$summary = as.data.frame(self$attributeAnalyses$summary[,1:11])

  }

},


calculateLogLikelihoods = function(force = FALSE){

  # calculte loglikelihoods only if force is TRUE or private$likelihoodsCalculated==FALSE
  if(!private$likelihoodsCalculated | force){


    # first, create logLikelihoods objects
    self$logLikelihoods = list()
    self$logLikelihoods$routines = list()
    self$logLikelihoods$marginal = matrix(data = NA, nrow = self$options$nSampled*self$options$nChains, ncol = self$specs$nUnits)
    self$logLikelihoods$conditional = matrix(data = NA, nrow = self$options$nSampled*self$options$nChains, ncol = self$specs$nUnits)

    # self$logLikelihoods$routines$calculateConditionalLogLikelihood = logLikelihoodObservedOnly

    # check if latent variables are included in model: if no, the marginal==conditional DIC
    if (self$specs$nLatentVariables == 0){

      # conditional is marginal DIC
      self$logLikelihoods$routines$calculateMarginalLogLikelihood = logLikelihoodObservedOnly
      self$logLikelihoods$type = ""
    } else {
      # conditional and marginal are separate
      self$logLikelihoods$type = "Marginal "
      if (self$specs$nJointVariables == 0){
        # bayesnets structure -- needs joint model probabilities calculated

        #first need to create a joint distribution:

        self$specs$attributeProfile = matrix(data = NA, nrow = 2^self$specs$nLatents, ncol = self$specs$nLatents)
        for (profile in 1:2^self$specs$nLatents){
          self$specs$attributeProfile[profile, ] = dec2bin(decimal_number = profile-1, nattributes = self$specs$nLatents, basevector = rep(2, self$specs$nLatents))
        }
        colnames(self$specs$attributeProfile) = self$specs$latentVariables

        self$specs$attributeProfile = as.data.frame(self$specs$attributeProfile)

        self$logLikelihoods$routines$calculateMarginalLogLikelihood = logLikelihoodMarginalLatentCategoricalUnivariate

      } else {

        # general structure -- has joint model probabilities already part of the model

        if (self$specs$nJointVariables == 1){ # limiting to one joint latent variable...for now

          # built for a single joint variable now--can be changed
          self$specs$attributeProfile = as.data.frame(self$variables[[self$specs$jointVariables[1]]]$attributeProfile)

          self$logLikelihoods$routines$calculateMarginalLogLikelihood = logLikelihoodMarginalLatentCategoricalJoint

        }

      }

    }


    for (chain in 1:length(self$chain)){
      for (iter in 1:nrow(self$chain[[chain]])){
        self$movePosteriorToVariableBeta(chain =chain, iteration = iter)
        self$logLikelihoods$marginal[(chain-1)*self$options$nSampled+iter,] = self$logLikelihoods$routines$calculateMarginalLogLikelihood(
          specs = self$specs, variables = self$variables, data = self$data)

      }
    }



    # with functions loaded, now calculate loglikelihoods for all chain iterations

    private$likelihoodsCalculated = TRUE
  }
  invisible(self)

},

createParameterSummary = function(){

  # parameter summary has two portions: distribution summary (parameter estimates) and data summary (person estimates)

  # separate chain into distribution parameters and data parameters
  # convert chain into coda object mcmc.list
  nChains = length(self$chain)
  if (class(self$chain) == "list"){

    # grab only model parameters for model chain
    modelChain = lapply(X = self$chain, FUN = function(x) return(x[,self$specs$parameters$paramNames[which(self$specs$parameters$paramTypes == "model")]]))
    if (nChains > 1){

      # create stacked chain for HDPI creation in coda package (otherwise it returns HDPI for each chain in list)
      stackedModelChain = coda::mcmc(do.call("rbind", lapply(
        X = modelChain,
        FUN = function(x)
          return(as.matrix(x))
      )))

      modelChain = coda::mcmc.list(lapply(X = modelChain, FUN = coda::mcmc))

    } else {
      modelChain = self$chain[[1]][,self$specs$parameters$paramNames[which(self$specs$parameters$paramTypes == "model")]]
      stackedModelChain = modelChain

      modelChain = coda::mcmc(modelChain)
      stackedModelChain = coda::mcmc(modelChain)

    }
  }

  # build massive matrix of parameters by statistics
  chainSummary = summary(modelChain)
  chainSummary = cbind(chainSummary$statistics, chainSummary$quantiles)

  # add HDPIs:
  HPDIval = self$options$HDPIntervalValue
  HDPI = coda::HPDinterval(stackedModelChain, prob = HPDIval)
  colnames(HDPI) = c(paste0("lowerHDPI", HPDIval), paste0("upperHDPI95", HPDIval))
  chainSummary = cbind(chainSummary, HDPI)

  if (nChains > 1){
    convergenceDiagnostics = coda::gelman.diag(modelChain, multivariate = FALSE)
    colnames(convergenceDiagnostics$psrf) = c("PSRF", "PSRF Upper C.I.")
    chainSummary = cbind(chainSummary, convergenceDiagnostics$psrf)
  } else {
    convergenceDiagnostics = coda::heidel.diag(modelChain)
    temp = convergenceDiagnostics[, c(3,4)]
    colnames(temp) = c("Heidel.Diag p-value", "Heidel.Diag Htest")
    chainSummary = cbind(chainSummary, temp)
  }

  self$parameterSummary = chainSummary
#
#   # next, data parameters (latent variables only for now)
#   # grab only data parameters for model chain
#
#   if (class(self$chain) == "list"){
#
#     # grab only model parameters for model chain
#     modelChain = lapply(X = self$chain, FUN = function(x) return(x[,self$specs$parameters$paramNames[which(self$specs$parameters$paramTypes == "data")]]))
#     if (nChains > 1){
#
#       # create stacked chain for HDPI creation in coda package (otherwise it returns HDPI for each chain in list)
#       stackedModelChain = coda::mcmc(do.call("rbind", lapply(
#         X = modelChain,
#         FUN = function(x)
#           return(as.matrix(x))
#       )))
#
#       modelChain = coda::mcmc.list(lapply(X = modelChain, FUN = coda::mcmc))
#
#     } else {
#       modelChain = self$chain[[1]][,self$specs$parameters$paramNames[which(self$specs$parameters$paramTypes == "data")]]
#       stackedModelChain = modelChain
#
#       modelChain = coda::mcmc(modelChain)
#       stackedModelChain = coda::mcmc(modelChain)
#
#     }
#   }
#
#   # build massive matrix of parameters by statistics
#   chainSummary = summary(modelChain)
#   chainSummary = cbind(chainSummary$statistics, chainSummary$quantiles)
#
#   # add HDPIs:
#   HPDIval = self$options$HDPIntervalValue
#   HDPI = coda::HPDinterval(stackedModelChain, prob = HPDIval)
#   colnames(HDPI) = c(paste0("lowerHDPI", HPDIval), paste0("upperHDPI95", HPDIval))
#   chainSummary = cbind(chainSummary, HDPI)
#
#   if (nChains > 1){
#     convergenceDiagnostics = coda::gelman.diag(modelChain, multivariate = FALSE)
#     colnames(convergenceDiagnostics$psrf) = c("PSRF", "PSRF Upper C.I.")
#     chainSummary = cbind(chainSummary, convergenceDiagnostics$psrf)
#   } else {
#     convergenceDiagnostics = coda::heidel.diag(modelChain)
#     temp = convergenceDiagnostics[, c(3,4)]
#     colnames(temp) = c("Heidel.Diag p-value", "Heidel.Diag Htest")
#     chainSummary = cbind(chainSummary, temp)
#   }
#
#   self$dataParameterSummary = chainSummary
  invisible(self)
},



initialize = function(data, specs, options, chain, variables) {
  self$data = data
  self$specs = specs
  self$options = options
  self$chain = chain
  self$variables = variables
},


movePosteriorMeanToVariableBeta = function(){

  # moves values of parameters from the mean of the posterior distribution to all variables' beta vectors

  # ensure parameter summary is created
  if (is.null(self$parameterSummary)) self$createParameterSummary()

  # move posterior values to variables
  self$variables = lapply(
    X = self$variables,
    FUN = function(x) {
      paramVals = self$parameterSummary[which(rownames(self$parameterSummary) %in% x$paramNames), 1]
      evalThis = paste0("self$",
                        self$specs$parameters$paramLocation[which(self$specs$parameters$paramNames %in% x$paramNames)],
                        "=", paramVals[x$paramNames])
      eval(parse(text=evalThis))
      # x$beta = t(t(self$parameterSummary[which(rownames(self$parameterSummary) %in% x$paramNames), 1]))
      return(x)
    }
  )


  invisible(self)
},


movePosteriorToVariableBeta = function(chain, iteration){


  # moves values of parameters from a posterior distribution to all variables' beta vectors

  if (chain <= length(self$chain) & iteration <= nrow(self$chain[[chain]])){

    # move posterior values to variables
    self$variables = lapply(
      X = self$variables,
      FUN = function(x, chain, iteration) {
        paramVals = self$chain[[chain]][iteration, which(colnames(self$chain[[chain]]) %in% x$paramNames)]
        evalThis = paste0("self$",
          self$specs$parameters$paramLocation[which(self$specs$parameters$paramNames %in% x$paramNames)],
          "=", paramVals[x$paramNames])
        eval(parse(text=evalThis))
        # x$beta = t(t(self$chain[[chain]][iteration, which(colnames(self$chain[[chain]]) %in% x$paramNames)]))
        return(x)
      },
      chain = chain,
      iteration = iteration
    )


  } else {
    stop("self$movePosteriorToVariableBeta: chain or iteration number exceeds values in self.")
  }
  invisible(self)
},

latentEstimates = function(...){

  # latent estimates creates data frame of estimated person parameters
  if (!private$latentEstimatesCalculated){
    if (self$specs$nCategoricalLatents > 0){ # right now all are categorical latent
      # for categorical latent variables, there are two types: univariate and joint

      # but, both need to have a joint distribution built

      # building function that will apply to each person

      # need: names for each profile

      allCategoricalLVProfiles = matrix(data = NA, nrow = 2^self$specs$nCategoricalLatents,
                                        ncol = self$specs$nCategoricalLatents)
      colnames(allCategoricalLVProfiles) = self$specs$latentVariables
      for (profile in 1:nrow(allCategoricalLVProfiles)){
        allCategoricalLVProfiles[profile,] = dec2bin(decimal_number = profile-1,
                                                     nattributes = ncol(allCategoricalLVProfiles),
                                                     basevector = rep(2, ncol(allCategoricalLVProfiles)))
      }

      rownames(allCategoricalLVProfiles) = paste0("profile",
                                                  apply(
                                                    X = allCategoricalLVProfiles,
                                                    MARGIN = 1,
                                                    FUN = function(x)
                                                      return(paste(x, collapse = ""))
                                                  ))

      temp = lapply(
        X = 1:self$specs$nUnits,
        FUN = private$getCategoricalLatentEstimates,
        profileMatrix = allCategoricalLVProfiles
      )
      self$estimatedLatentVariables = do.call("rbind", temp)

    }

    private$latentEstimatesCalculated = TRUE
  }


  invisible(self)
},

prepareData = function(...){

  for (variable in 1:length(self$variables)){
    self$data = self$variables[[variable]]$prepareData(data = self$data)
    self$specs$underlyingVariables =
      c(self$specs$underlyingVariables, self$variables[[variable]]$underlyingVariables)
  }

  invisible(self)
},

summary = function(numDigits = 3L, ...) {

  # format for numeric values
  num.format  <-
    paste("%", max(8L, numDigits + 5L), ".", numDigits, "f", sep = "")
  char.format <-
    paste("%", max(8L, numDigits + 5L), "s", sep = "")


  # header of summary output
  headings = paste0(sprintf(char.format, c("     ")), collapse = "")
  cat(paste0("\nblatent (version ", packageVersion("blatent"), ") Analysis Summary\n"))
  cat(paste0(rep("-", 80), collapse = ""))

  cat("\nAnalysis Specs:")
  cat("\n")
  preamble = paste0("  ", "Algorithm", collapse = "")
  paramVals = paste0(sprintf(char.format, self$options$estimator), collapse = "")
  buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
  cat(paste0(preamble, buffer, paramVals, collapse = ""))
  cat("\n")

  preamble = paste0("  ", "Number of Model Parameters", collapse = "")
  paramVals = paste0(sprintf(char.format, length(which(self$specs$parameters$paramTypes == "model"))), collapse = "")
  buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
  cat(paste0(preamble, buffer, paramVals, collapse = ""))
  cat("\n")

  preamble = paste0("  ", "Number of Observations", collapse = "")
  paramVals = paste0(sprintf(char.format, length(self$specs$unitList)), collapse = "")
  buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
  cat(paste0(preamble, buffer, paramVals, collapse = ""))
  cat("\n")


  cat(paste0(rep("-", 80), collapse = ""))
  cat("\n")
  cat("Convergence Diagnostics:")
  cat("\n")
  if (self$options$nChains > 1) {
    preamble = paste0("  ", "Maximum Univariate PSRF of Model Parameters:", collapse = "")
    paramVals = paste0(sprintf(num.format, max(self$parameterSummary[,"PSRF"])), collapse = "")
    buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
    cat(paste0(preamble, buffer, paramVals, collapse = ""))
    cat("\n")
  } else {
    preamble = paste0("  ", "Minimum Heidel.Diag p-value of Model Parameters:", collapse = "")
    paramVals = paste0(sprintf(num.format, min(self$parameterSummary[,"Heidel.Diag p-value"])), collapse = "")
    buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
    cat(paste0(preamble, buffer, paramVals, collapse = ""))
    cat("\n")
  }

  cat(paste0(rep("-", 80), collapse = ""))

  if (self$options$calculateDIC | self$options$calculateWAIC){
    cat("\nInformation Criteria:")


    headings = paste0(sprintf(char.format, c("     ")), collapse = "")

    if (self$options$calculateDIC & !is.null(self$informationCriteria$DIC)){
      cat("\n")
      preamble = paste0("  ", "DIC", collapse = "")
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(headings)), collapse = "")
      cat(paste0(preamble, buffer, headings))
      cat("\n")

      preamble = paste0("    ", self$logLikelihoods$type, "DIC", collapse = "")
      paramVals = paste0(sprintf(num.format, self$informationCriteria$DIC$DIC), collapse = "")
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
      cat(paste0(preamble, buffer, paramVals, collapse = ""))
      cat("\n")

      preamble = paste0("    ", self$logLikelihoods$type, "DIC Effective Number of Parameters", collapse = "")
      paramVals = paste0(sprintf(num.format, self$informationCriteria$DIC$p_D), collapse = "")
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
      cat(paste0(preamble, buffer, paramVals, collapse = ""))
      cat("\n")
    }

    headings = paste0(sprintf(char.format, c("     ")), collapse = "")

    if (self$options$calculateWAIC & !is.null(self$informationCriteria$WAIC)){

      preamble = paste0("  ", "WAIC", collapse = "")
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(headings)), collapse = "")
      cat(paste0(preamble, buffer, headings))
      cat("\n")

      preamble = paste0("    ", self$logLikelihoods$type, "WAIC (Deviance metric: -2*WAIC)", collapse = "")
      paramVals = paste0(sprintf(num.format, -2*self$informationCriteria$WAIC$WAIC), collapse = "")
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(headings)), collapse = "")
      cat(paste0(preamble, buffer, paramVals, collapse = ""))
      cat("\n")

      preamble = paste0("    ", self$logLikelihoods$type, "WAIC Effective Number of Parameters", collapse = "")
      paramVals = paste0(sprintf(num.format, self$informationCriteria$WAIC$p_WAIC), collapse = "")
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(paramVals)), collapse = "")
      cat(paste0(preamble, buffer, paramVals, collapse = ""))
      cat("\n")
    }

    cat(paste0(rep("-", 80), collapse = ""))
  }


  if (self$options$posteriorPredictiveChecks$estimatePPMC & !is.null(self$PPMC)){
    cat("\nPosterior Predictive Model Check Summary: \n")
    for (ppmc in 1:length(self$PPMC)){
      if (!names(self$PPMC)[[ppmc]] %in% c("univariate", "bivariate"))  {
        cat(self$PPMC[[ppmc]]$summaryMessage)
        cat("\n")
      }
    }
    for (ppmc in 1:length(self$PPMC)){
      if (names(self$PPMC)[[ppmc]] %in% c("univariate", "bivariate"))  {
        cat(self$PPMC[[ppmc]]$summaryMessage)
        cat("\n")
      }
    }
    cat(paste0(rep("-", 80), collapse = ""))
  }


  cat("\nParameter Estimates:")

  if (self$options$nChains > 1) {
    headings = paste0(sprintf(char.format, c(
      "Mean", "SD", "LowHPDI", "UpHDPI", "PSRF"
    )), collapse = "")
  } else if (self$options$nChains == 1) {
    headings = paste0(sprintf(char.format, c(
      "Mean", "SD", "LowHPDI", "UpHDPI", "HDPV"
    )), collapse = "")
  }
  cat("\n")

  for (variable in names(self$variables)) {
    cat(paste0(rep("-", 80), collapse = ""))
    cat("\n")

    preamble = paste0(variable, ":", collapse = "")
    buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(headings)), collapse = "")

    cat(paste0(preamble, buffer, headings))
    cat("\n")
    for (param in self$variables[[variable]]$paramNames) {
      csRow = which(rownames(self$parameterSummary) == param)

      preamble = paste0("  ", param, collapse = "")
      buffer = paste0(rep(" ", 80 - nchar(preamble) - nchar(headings)), collapse = "")
      if (self$options$nChains > 1) {
        paramVals = paste0(sprintf(num.format, self$parameterSummary[csRow, c("Mean",
                                                                              "SD",
                                                                              "lowerHDPI0.95",
                                                                              "upperHDPI950.95",
                                                                              "PSRF")]),
                           collapse = "")
      } else if (self$options$nChains == 1) {
        paramVals = paste0(sprintf(num.format, self$parameterSummary[csRow, c("Mean",
                                                                              "SD",
                                                                              "lowerHDPI0.95",
                                                                              "upperHDPI950.95",
                                                                              "Heidel.Diag p-value")]),
                           collapse = "")
      }

      cat(paste0(preamble, buffer, paramVals, collapse = ""))
      cat("\n")
    }
  }

}

    ),


private = list(
# Private Variables ========================================================================================
  latentEstimatesCalculated = FALSE,
  likelihoodsCalculated = FALSE,


# Private Functions ========================================================================================

getCategoricalLatentEstimates = function(obs, profileMatrix){


  # first, check if chain has values for latent variables
  lvcols = which(colnames(self$chain[[1]]) %in% paste0(obs, ".",self$specs$latentVariables))

  if (length(lvcols) == 0) return(NULL) # future exit

  # temp = vapply(X = model$chain, FUN = function(x) return(x[,lvcols]), FUN.VALUE = cbind(as.numeric(1:dim(model$chain[[1]])[1]), as.numeric(1:dim(model$chain[[1]])[1])))
  temp = lapply(X = self$chain, FUN = function(x) return(x[,lvcols]))
  latentData = do.call("rbind", temp)

  # get marginal means for attributes
  marginalMeans = apply(X = latentData, MARGIN = 2, FUN = mean)

  # convert each iteration to profile number
  latentData = cbind(latentData, apply(X = latentData, MARGIN = 1, FUN = bin2dec, nattributes = ncol(latentData),
                                       basevector = rep(2, ncol(latentData)))+1)

  colnames(latentData)[ncol(latentData)] = "clvProfile"

  nProfiles = 2^self$specs$nCategoricalLatents

  # get EAP estimates of profile probabilities
  eapProfile = table(factor(latentData[,"clvProfile"], levels = 1:nProfiles))/nrow(latentData)

  # build initial table with EAP estimates
  result = cbind(t(marginalMeans), t(eapProfile))
  colnames(result) = c(paste0(self$specs$latentVariables, ".EAP.marginal"),
                       paste0(rownames(profileMatrix), ".EAP.joint"))
  rownames(result) = obs

  # add in MAP estimates
  result = cbind(result, t(round(marginalMeans)), t(as.numeric(which.max(eapProfile))))
  colnames(result)[(ncol(result)-length(marginalMeans)):ncol(result)] =
    c(paste0(self$specs$latentVariables, ".MAP.marginal"),"profileNumber.MAP")

  result = cbind(result, t(profileMatrix[which.max(eapProfile),]))
  colnames(result)[(ncol(result)-length(marginalMeans)+1):ncol(result)] =
    paste0(self$specs$latentVariables, ".MAP.joint")

  # convert to data frame in case obs is a character
  result = as.data.frame(result)
  result = cbind(t(obs), result)
  colnames(result)[1] = "observation"

  return(result)
}

), # end private

) # end class



#
# # S3 dispatch functions:
# summary.blatentModel <- function(object, ...) {
#   object$summary(...)
#
# }
#
#

