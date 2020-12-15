#' @import R6
blatentSimulatedData <-
  R6::R6Class(
    classname = "blatentSimluatedData",
# public ======================================================================================================================
public = list(
# variables ===================================================================================================================
data = NA,
defaultSimulatedParameters = NA,
latentInformation = NA,
itemInformation = NA,
nObs = NA,
simModel = NA,
seed = NA,
specs = NA,
trueAnalysis = list(),
trueValues = NA,
variables = NA,

# functions ===================================================================================================================
calculateInformationStatistics = function(){

  # currently only calculated for times when there is exactly one mvbernoulli latent variable
  varDists =  unlist(lapply(self$variables, FUN = function(x) return(x$distributionSpecs$distribution)))

  runthis = TRUE
  if (length(which(varDists == "mvbernoulli")) != 1){
    runthis == FALSE
  } else {
    jointVar=names(varDists)[which(varDists == "mvbernoulli")]
    if (!self$variables[[jointVar]]$isLatentJoint){
      runthis == FALSE
    }
  }

  if (!runthis){
    cat("\n information statistics currently available for models with a single mvbernoulli latent variable")
    return(NULL)
  }


  attributeProfile = self$variables[[jointVar]]$attributeProfile
  profileProb = self$variables[[jointVar]]$parameters$probVec

  # loop through each observed item and attribute and calculate item-specific discrimination values
  self$itemInformation = list()
  self$latentInformation = list()
  # calculate CDI across all items
  self$latentInformation$CDI = 0

  item = 1
  for (item in 1:length(self$specs$observedVariables)){
    var = self$specs$observedVariables[item]
    self$itemInformation[[var]] = list()
    self$itemInformation[[var]]$CDI = itemCDI(itemVariable = self$variables[[var]], attributeProfile = attributeProfile)
    self$latentInformation$CDI = self$latentInformation$CDI + self$itemInformation[[var]]$CDI
    att=1
    for (att in 1:ncol(attributeProfile)){
      a = colnames(attributeProfile)[att]
      self$itemInformation[[var]][[a]] =
        itemDiscrimination(
          attributeName = a,
          itemVariable = self$variables[[var]],
          attributeProfile = attributeProfile,
          profileProb = profileProb
        )


    }
  }

  # loop through all attributes and compute latentInformation:




  for (att in 1:ncol(attributeProfile)){
    a = colnames(attributeProfile)[att]
    self$latentInformation[[a]]$D_A = 0
    self$latentInformation[[a]]$D_B = 0
    for (item in 1:length(self$itemInformation)){

      self$latentInformation[[a]]$D_A = self$latentInformation[[a]]$D_A + self$itemInformation[[item]][[a]]$D_A
      self$latentInformation[[a]]$D_B = self$latentInformation[[a]]$D_B + self$itemInformation[[item]][[a]]$D_B
    }

  }

  invisible(self)


},

initialize = function(nObs,
                      simModel,
                      defaultSimulatedParameters,
                      seed,
                      data,
                      trueValues,
                      variables,
                      specs) {

# set values of parameters
  self$data = data
  self$defaultSimulatedParameters = defaultSimulatedParameters
  self$nObs = nObs
  self$simModel = simModel
  self$seed = seed
  self$trueValues = trueValues
  self$variables = variables
  self$specs = specs
# establish specs, options, and variables from blatent objects


},

simTest = function(options=blatentControl()){


  # run blatent using info from simulated parameters
  self$trueAnalysis  = list(model = NA, compare = NA, results = NA, classification = list(marginal = NA, joint = NA))
  self$trueAnalysis$model = blatentEstimate(dataMat = self$data, modelText = self$simModel, options = options)

  # compare true and estimated parameter values


  trueValues = unlist(lapply(X = self$trueValues, FUN = function(x) return(x[2:length(x)])))
  trueValues = data.frame(cbind( names(trueValues), trueValues), stringsAsFactors = FALSE)
  names(trueValues) = c("param", "true")
  trueValues$true = as.numeric(trueValues$true)

  estimatedValues = data.frame(cbind(rownames(self$trueAnalysis$model$parameterSummary),self$trueAnalysis$model$parameterSummary[,1]), stringsAsFactors = FALSE)
  names(estimatedValues) = c("param", "EAP")
  estimatedValues$EAP = as.numeric(estimatedValues$EAP)

  compare = merge(x = trueValues, y = estimatedValues, by = "param")

  results = data.frame(corr = cor(compare$EAP, compare$true), RMSE = RMSE(x = compare$true, y = compare$EAP), MAD = MAD(x = compare$true, y = compare$EAP),
                       bias = bias(true = compare$true, est = compare$EAP))
  rownames(results) = ""

  self$trueAnalysis$compare = compare
  self$trueAnalysis$results = results

  cat("\n")
  cat("blatentSimulate Comparison to True Parameters")
  cat("\n")

  print(results)

  estimatedValues = data.frame(cbind(rownames(self$trueAnalysis$model$parameterSummary),self$trueAnalysis$model$parameterSummary), stringsAsFactors = FALSE)
  names(estimatedValues)[1] = "param"
  analysis = merge(x = trueValues, y = estimatedValues, by = "param")
  for (i in 2:ncol(analysis)){
    analysis[,i] = as.numeric(analysis[,i])
  }

  # for each item/variable, add in information statistics about each item (D_A, D_B, CDI), and also add bias and absdev
  analysis$bias = analysis$Mean - analysis$true
  analysis$absdif = abs(analysis$bias)

  self$trueAnalysis$analysis = analysis

  # calculate classification accuracy if latent variables are categorical
  if (self$trueAnalysis$model$options$estimateLatents & self$trueAnalysis$model$specs$nCategoricalLatents >0){

    # first create profile number for each attribute in data
    latentVariables = self$trueAnalysis$model$specs$latentVariables

    trueProfile = bin2dec(binary_vector = self$data[latentVariables],
                                nattributes = length(latentVariables),
                                basevector = rep(2, length(latentVariables))) +1

    self$trueAnalysis$classification$joint =
      length(which(trueProfile == self$trueAnalysis$model$estimatedLatentVariables$profileNumber.MAP))/nrow(self$data)

    numerator = 0
    for (var in 1:length(latentVariables)){
      self$trueAnalysis$classification[[latentVariables[var]]] =
        length(which(self$data[latentVariables[var]] == self$trueAnalysis$model$estimatedLatentVariables[paste0(latentVariables[var], ".MAP.marginal")]))/
          self$specs$nUnits
      numerator = numerator +
        length(which(self$data[latentVariables[var]] == self$trueAnalysis$model$estimatedLatentVariables[paste0(latentVariables[var], ".MAP.marginal")]))
    }

    self$trueAnalysis$classification$marginal = numerator/(length(latentVariables)*nrow(self$data))
    self$trueAnalysis$classification$results = data.frame(joint = self$trueAnalysis$classification$joint,
                                                          marginal = self$trueAnalysis$classification$marginal)
    rownames(self$trueAnalysis$classification$results) = ""
    cat("\n")
    cat("blatentSimulate Classification Rates")
    cat("\n")
    print(self$trueAnalysis$classification$results)

  }




},

summary = function() {

}

)
)

# # S3 dispatch functions:
#
# summary.blatentSimluatedData <- function(object, ...) {
#   object$summary()
# }
#
