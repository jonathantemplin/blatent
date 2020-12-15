#' @import coda
chainSummary = function(chain, HDPIntervalValue){
  nChains = length(chain)
  if (class(chain) == "list"){


    if (nChains > 1){

      # create stacked chain for HDPI creation in coda package (otherwise it returns HDPI for each chain in list)
      stackedModelChain = coda::mcmc(do.call("rbind", lapply(
        X = chain,
        FUN = function(x)
          return(as.matrix(x))
      )))

      modelChain = coda::mcmc.list(lapply(X = chain, FUN = coda::mcmc))

    } else {

      stackedModelChain = chain

      modelChain = coda::mcmc(modelChain)
      stackedModelChain = coda::mcmc(modelChain)

    }
  }

  # build massive matrix of parameters by statistics
  chainSummary = summary(modelChain)
  chainSummary = cbind(chainSummary$statistics, chainSummary$quantiles)

  # add HDPIs:
  HPDIval = HDPIntervalValue
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

  return(chainSummary)

}
