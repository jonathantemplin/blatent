separateAdditionalSyntax = function(line.simple){

  # must have <- ___ (
  # determine word that is in ___

  # check for multiple <- signs on
  temp = trimws(unlist(strsplit(x = line.simple, split = "<-")))

  lhs = temp[1]

  # check variables for being part of index list
  if (length(grep(pattern = "-", x = lhs)) > 0) {
    lhs = getVariableList(variableText = temp[1])
  } else {
    lhs = trimws(unlist(strsplit(x = lhs, split = " ")))
  }
  rhs = temp[2]

  # check for open parentheses
  tempRHS = trimws(unlist(strsplit(x = rhs, split = "\\(")))


  # determine which word appears:
  latents = list()
  distributions = list()
  priors = list()

  if (tempRHS[1] == "latent"){
    latents = as.list(paste(lhs, "<-", rhs))
    names(latents) = lhs

    # check to see if latents are MVB and if so, add class variable to latents
    # latentCall = eval(parse(text = rhs))
    # if (latentCall$distribution == "mvbernoulli"){
    #   latents$CLASS = "CLASS"
    # }

  } else if (tempRHS[1] == "observed"){
    distributions = as.list(paste(lhs, "<-", rhs))
    names(distributions) = lhs
  } else if (tempRHS[1] == "prior"){
    priors = as.list(paste(lhs, "<-", rhs))
    names(priors) = lhs
  } else {
    stop(paste0("Blatent syntax error: Unrecognized command ", tempRHS[1], "\n From Line: ", line.simple))
  }
  test = list(latents = latents, distributions = distributions, priors = priors)
  return(test)

}

