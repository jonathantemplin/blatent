
generateBernoulliWrap = function(n, variableOptions){
  # wraps the dbinom function for generating Bernoulli variables with simulation syntax
  # variableOptions must be a list that includes: linearPredictor & inverseLinkFunction | prob

  if ("linearPredictor" %in% names(variableOptions) & "inverseLinkFunction" %in% names(variableOptions)){
    mean = variableOptions$inverseLinkFunction(variableOptions$linearPredictor)
  } else if ("prob" %in% names(variableOptions)){
    mean = variableOptions$prob
  } else {
    stop("Incorrect input into generateBernoulliWrap: see variableOptions list")
  }

  return(stats::rbinom(n = n, size = 1, prob = mean))

}
