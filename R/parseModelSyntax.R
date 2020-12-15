
parseModelSyntax = function(modelText){
  #function modeled after and using syntax from lavaan::lavParseModeString

  #NOTE: MODELTEXT KEEPS BEING VALUE OF ONE BY DEFAULT
  # check for empty syntax
  if(length(modelText) == 0) {
    stop("ERROR: empty model syntax")
  }

  # remove comments prior to split.
  # Match from comment character to newline, but don't eliminate newline
  modelText <- gsub("[#!].*(?=\n)","", modelText, perl=TRUE)

  # replace semicolons with newlines prior to split
  modelText <- gsub(";", "\n", modelText, fixed=TRUE)

  # remove any occurrence of >= 2 consecutive newlines to eliminate \
  # blank statements; this retains a blank newline at the beginning,
  # if such exists, but parser will not choke because of start.idx
  modelText <- gsub("\n{2,}", "\n", modelText, perl=TRUE)

  # break up in lines
  model <- unlist( strsplit(modelText, "\n") )

  # check for multi-line formulas: they contain no "~" or "=" character
  # but before we do that, we remove all modifiers
  # to avoid confusion with for example equal("f1=~x1") statements
  model.simple <- gsub("\\(.*\\)\\*", "MODIFIER*", model)

  start.idx <- grep("[~=<>:|%]", model.simple)

  # check for empty start.idx: no operator found (new in 0.6-1)
  if(length(start.idx) == 0L) {
    stop("blatent ERROR: model does not contain blatent syntax (no operators found)")
  }

  end.idx <- c( start.idx[-1]-1, length(model) )
  model.orig    <- model
  model <- character( length(start.idx) )
  for(i in 1:length(start.idx)) {
    model[i] <- paste(model.orig[start.idx[i]:end.idx[i]], collapse="")
  }

  modelEqnList = which(unlist(lapply(X = model, FUN = function(x) {
    if (length(grep(x =x , pattern = "~")) > 0) {
      return(TRUE)
    } else {
        return(FALSE)

      }
    })))

  additionalInfoList = which(unlist(lapply(X = model, FUN = function(x) {
    if (length(grep(x =x , pattern = "<-")) > 0) {
      return(TRUE)
    } else {
      return(FALSE)

    }
  })))

  if (any(modelEqnList %in% additionalInfoList)){
    stop (paste0("Blatent Syntax Error: each syntax command must contain a ~ or a <-. "))
  }

  # need: if attributes are mvbernoulli then add each attribute to modelVector otherwise order will be destroyed

  modelEqns = model[modelEqnList]
  additionalInfo = model[additionalInfoList]

  modelVector = unlist(lapply(X = modelEqns, FUN = convertSyntaxToFormula))
  additionalSyntax = lapply(X = additionalInfo, FUN = separateAdditionalSyntax)
  latents = unlist(lapply(X = additionalSyntax, FUN = function(y) return(y$latents)))

  # grab names of DVs in model to be sure all latents are accounted for in the model
  modelDVs = unlist(lapply(X = modelVector, FUN = function(x){
    return(as.character(attr(stats::terms(x), "variables")[-1])[attr(stats::terms(x), "response")])
  }))

  # here, we must parse latents additionally to ensure correct type of model is specified
  # main distinction: joint-distribution latent variable vs. marg/cond distribution latent variables

  # submit information about latents to latents() function
  tempLatent = lapply(
    X = c(latents),
    FUN = function(x)
      return(eval(parse(text = trimws(
        strsplit(x = x, split = "<-")[[1]][2]
      ))))
  )

  # next, determine which latents have joint distributions
  latentJointDists = unlist(lapply(
    X = tempLatent,
    FUN = function(x) {
      if (!is.null(x$jointName))
        return(x$jointName)
    }
  ))

  uniqueLatentJointDists = unique(latentJointDists)

  # add joint distribution information to new list for use later
  latentJointDistInfo = list()

  if (length(uniqueLatentJointDists) > 0){
    for (d in 1:length(uniqueLatentJointDists)){
      latentJointDistInfo[[uniqueLatentJointDists[d]]] =
        paste0(
          uniqueLatentJointDists[d],
          " <- joint(distribution = '",
          tempLatent[[names(latentJointDists[which(latentJointDists == uniqueLatentJointDists[d])])[1]]]$distribution,
          "', vars = c('",
          paste(names(latentJointDists[which(latentJointDists == uniqueLatentJointDists[d])]), collapse =
                  "','"),
          "'), unit = '",
          tempLatent[[names(latentJointDists[which(latentJointDists == uniqueLatentJointDists[d])])[1]]]$unit,
          "', type = '",
          tempLatent[[names(latentJointDists[which(latentJointDists == uniqueLatentJointDists[d])])[1]]]$type,
          "')"
        )
    }
  }


  # # put into character form
  # latentJointDistInfo = unlist(latentJointDistInfo)
  #
  # # need to be above next step as need info on latents to decide how to add to modelVector
  # distributions = unlist(lapply(X = additionalSyntax, FUN = function(x) return(x$distributions)))
  # priors = unlist(lapply(X = additionalSyntax, FUN = function(x) return(x$priors)))
  #
  # details = lapply(
  #   X = c(latents, distributions),
  #   FUN = function(x)
  #     return(eval(parse(text = trimws(
  #       strsplit(x = x, split = "<-")[[1]][2]
  #     ))))
  # )
  #
  #
  # # if there are joint distributions, be sure to add values to modelVector
  # if (length(latentJointDistInfo) > 0){
  #
  #   for (i in 1:length(latentJointDistInfo)){
  #     modelVector[[length(modelVector)+1]] = formula(paste0(names(latentJointDistInfo)[i], "~1"))
  #   }
  #
  # } else {
  #
  #   addLatents = names(latents)[which(!(names(latents) %in% modelDVs))]
  #
  #   if (length(addLatents) > 0){
  #     # for univariate cases: to ensure the variable has a distribution
  #     for (latent in 1:length(addLatents)){
  #
  #       if (details[[addLatents[latent]]]$distribution == "normal"){
  #         if (details$theta$meanIdentification == "fixed"){
  #           modelVector[[length(modelVector)+1]] = formula(paste0(addLatents[latent], "~0"))
  #         } else {
  #           modelVector[[length(modelVector)+1]] = formula(paste0(addLatents[latent], "~1"))
  #         }
  #       } else {
  #         modelVector[[length(modelVector)+1]] = formula(paste0(addLatents[latent], "~1"))
  #       }
  #
  #
  #     }
  #
  #
  #   }
  # }

  if (length(latentJointDistInfo) > 0){


    for (i in 1:length(latentJointDistInfo)){
      modelVector[[length(modelVector)+1]] = stats::formula(paste0(names(latentJointDistInfo)[i], "~1"))
    }

  } else {

    # for univariate cases: to ensure the variable has a distribution
    addLatents = names(latents)[which(!(names(latents) %in% modelDVs))]
    if (length(addLatents) > 0){
      for (i in 1:length(addLatents)){
        modelVector[[length(modelVector)+1]] = stats::formula(paste0(addLatents[i], "~1"))
      }
    }
  }

  distributions = unlist(lapply(X = additionalSyntax, FUN = function(x) return(x$distributions)))
  priors = unlist(lapply(X = additionalSyntax, FUN = function(x) return(x$priors)))

  details = lapply(
    X = c(latents, distributions),
    FUN = function(x)
      return(eval(parse(text = trimws(
        strsplit(x = x, split = "<-")[[1]][2]
      ))))
  )

  return(
    list(
      modelVector = modelVector,
      latents = latents,
      distributions = distributions,
      latentJointDists = latentJointDists,
      latentJointDistInfo = latentJointDistInfo,
      priors = priors,
      details = details
    )
  )



}



