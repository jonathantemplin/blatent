joint <- function(distribution = 'mvbernoulli', vars = NULL, unit = 'rows', type = 'ordinal'){

  if (distribution == "mvbernoulli"){
    generationFunction = rdirichlet
    linkFunction = identityLinkInvLink
    inverseLinkFunction = identityLinkInvLink
  } else{
    stop('distribution undefined for joint latent variable')
  }

  return(
    list(
      distribution = distribution,
      generationFunction = generationFunction,
      linkFunction = linkFunction,
      inverseLinkFunction = inverseLinkFunction,
      type = type,
      unit = unit,
      vars = vars
    )
  )
}
