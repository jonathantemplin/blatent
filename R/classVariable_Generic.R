classVariable_Generic <- R6Class(
  classname = "variable_Generic",
  public = list(
# public variables ==================================================================================================================
    allDrawnVariables = NA,            # from simulate
    allLikelihoodVars = NA,            # from classSampleUnits_MultivariateBernoulli_Gibbs
    allObserved = NA,                  # from classSampleParameters_Benoulli_Gibbs_HH2006L1
    attributeProfile = NA,             # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! variableProfiles(classVariable_MultivariateBernoulli)
    constraintMatrix = NA,             # from classSampleParameters_Benoulli_GibbsConstraint_HH2006L1
    completeCases = NA,
    covMat = NA,                       # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    defaultSimulatedParameters = list(),
    distributionSpecs = list(),
    formula = NA,
    formulaRHS = NA,
    hyperparameters = list(),
    imputeList = NA,
    incompleteCases = NA,
    initializeAlpha = NA,               # from classSampleUnits_MultivariateBernoulli_Gibbs
    initialParameterValues = list(),
    isLatent = NA,
    isLatentJoint = NA,
    isPredictor = NA,
    likelihoodPTR = NA,
    meanIdentification = NA,           # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! from classVariable_Normal
    missing = NA,
    nCategories = NA,
    nConstraints = NA,
    nImpute = NA,
    nMissing = NA,
    nObserved = NA,
    nParam = NA,
    nSubVariables = NA,
    modeledN = NA,
    modeledObs = NA,
    observed = NA,
    paramNames = NA,
    parameters = list(),
    predictedBy = NA,
    predicts = NA,
    predNames = NA,
    predTypes = NA,
    priorAlpha = NA,                   # from classSampleParameters_MultivariateBernoulli_Gibbs
    routines = list(),
    subVariableNames = NA, # dataSubVariableNames(classVariable_MultivariateBernoulli)
    variableName = NA,  # dataVariableName(classVariable_MultivariateBernoulli)
    variableNumber = NA,
    varianceIdentification = NA,  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! from classVariable_Normal,
    underlyingVariables = NA, # from classSampleUnits_Bernoulli_gibbs_HH2006L1
# public functions ==================================================================================================================
initialize = function(){

},
initializeParameters = function(){

},
initializeUnits = function(){

},
KLI = function(){

},
likelihood = function(){

},
loadPointers = function(){
  self$likelihoodPTR = bernoulliLikelihoodPtr(self$distributionSpecs$distribution)
},
predictedValues = function(){

},
prepareData = function(){

},
provideParameterTypesAndLocations = function(){

},
provideParameterValues = function(){

},
random = function(){

},
sampleParameters = function(){

},
sampleUnits = function(){

},
setParameterValues = function(){

},
simulateParameters = function(){

},
simulateUnits = function(){

}
  )
)
