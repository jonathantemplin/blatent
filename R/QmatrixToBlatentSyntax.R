#' Convert a rectangular Q-matrix into blatent model syntax
#'
#' @description
#' Converts a rectangular Q-matrix into blatent model syntax. Q-matrix must have observed variables listed across columns and latent variables listed across rows.
#'
#' @param Qmatrix A data frame or matrix containing a Q-matrix.
#'
#' @param observedVariables If \code{Qmatrix} is data.frame, the variable in the data frame that has the names of the observed variables. Defaults to \code{"rownames"},
#'   which uses \code{rownames(Qmatrix)} and works for data.frame or matrix types of \code{Qmatrix}.
#'
#' @param latentVariables A vector of the variable or column names of the latent variables. Defaults to \code{"colnames"}, which uses \code{colnames(Qmatrix)}
#'  and works for data.frame or matrix types of \code{Qmatrix}.
#'
#' @param lvDist A character that indicates the type of latent variable distribution to be used. \code{"joint"} for joint distributions (multivariate Bernoulli) or
#'  \code{"univariate"} for univariate Bernoulli using BayesNets parameterization. The latter also builds blatent syntax for the BayesNets model terms.
#'
#' @return A character vector containing blatent model syntax.
#'
#' @examples
#'
#'
#' # Example 1: Joint distribution using data.frame
#' # empty data.frame
#'
#' exampleQmatrixDF = data.frame(matrix(data = 0, nrow = 10, ncol = 3))
#'
#' # name columns of Qmatrix
#'
#' names(exampleQmatrixDF) = c("observedVariableName", "Attribute1", "Attribute2")
#'
#' # names of observed variables
#'
#' exampleQmatrixDF[1:10, "observedVariableName"] = paste0("Item",1:10)
#'
#' # Entries for Qmatrix
#'
#' exampleQmatrixDF[1:5,"Attribute1"] = 1
#' exampleQmatrixDF[3:10,"Attribute2"] = 1
#'
#' # produce blatentSyntax using QmatrixToBlatentSyntax() function
#'
#' blatentSyntaxJoint = QmatrixToBlatentSyntax(
#'    Qmatrix = exampleQmatrixDF,
#'    observedVariables = "observedVariableName",
#'    latentVariables = c("Attribute1", "Attribute2"),
#'    lvDist = "joint"
#'  )
#' cat(blatentSyntaxJoint)
#'
#' # Example 2: Univariate distributions using matrix
#' # empty data.frame
#'
#' exampleQmatrixM = matrix(data = 0, nrow = 10, ncol = 2)
#'
#' # name columns of Qmatrix as latent variable names
#'
#' colnames(exampleQmatrixM) = c("Attribute1", "Attribute2")
#'
#' # name rows of Qmatrix as observed variable names
#'
#' rownames(exampleQmatrixM) = paste0("Item",1:10)
#'
#' # Entries for Qmatrix
#'
#' exampleQmatrixM[1:5,"Attribute1"] = 1
#' exampleQmatrixM[3:10,"Attribute2"] = 1
#'
#' # produce blatentSyntax using QmatrixToBlatentSyntax() function
#' #  (with default options for observedVariables and latentVariables)
#'
#' blatentSyntaxM = QmatrixToBlatentSyntax(Qmatrix = exampleQmatrixM,  lvDist = "univariate")
#' cat(blatentSyntaxM)
#'
#'
#' @export
QmatrixToBlatentSyntax = function(Qmatrix, observedVariables = "rownames", latentVariables = "colnames", lvDist = "joint"){
  # latentVariables = c("A1", "A2", "A3", "A4")
  # observedVariables = "Item"


  if (latentVariables[1] == "colnames" & length(latentVariables) == 1){
    latentVarNames = colnames(Qmatrix)
  } else {
    latentVarNames = latentVariables
  }

  if (observedVariables == "rownames" & length(observedVariables) == 1){
    observedVarNames = rownames(Qmatrix)
  } else {
    observedVarNames = Qmatrix[,observedVariables[1]]
  }

  lvCols = which(colnames(Qmatrix) %in% latentVarNames)

  # get list of latent variables

  # get list of observed variables

  # create temporary data frame to build model.matrices with formulae
# Qmatrix[1,"A1"]=1
  obsVar = 1
  blatentSyntax = ""
  for (obsVar in 1:nrow(Qmatrix)){

    # determine which latent variables are measured by variable
    varName = observedVarNames[obsVar]

    # put into initial formula to get specific effects

    varFormula = paste(varName, "~", paste0(latentVarNames[which(Qmatrix[obsVar,latentVarNames] ==1)], collapse = "*"))
    blatentSyntax = paste0(blatentSyntax, "\n", paste(varName, "~", paste(attr(stats::terms(stats::formula(varFormula)), "term.labels"), collapse = " + ")))

  }

  # add latent distributions syntax

  if (lvDist == "joint"){
    blatentSyntax = paste0(blatentSyntax, "\n\n# Latent variable specifications")
    blatentSyntax = paste0(
      blatentSyntax,
      "\n",
      paste(
        paste(latentVarNames, collapse = " "),
        "<- latent(unit = 'rows', distribution = 'mvbernoulli', structure = 'joint', type = 'ordinal')"
      )
    )
  } else if (lvDist == "univariate"){

    blatentSyntax = paste0(blatentSyntax, "\n")
    blatentSyntax = paste0(blatentSyntax, "\n", latentVarNames[1], " ~ 1")
    if (length(latentVariables)>1){
      for (lvNum in 2:length(latentVariables)){
        varFormula = paste(latentVarNames[lvNum], "~", paste0(latentVarNames[1:(lvNum-1)], collapse = "*"))
        blatentSyntax = paste0(blatentSyntax, "\n", paste(latentVarNames[lvNum], "~", paste(attr(stats::terms(stats::formula(varFormula)), "term.labels"), collapse = " + ")))
      }
    }

    blatentSyntax = paste0(blatentSyntax, "\n\n# Latent variable specifications")
    blatentSyntax = paste0(
      blatentSyntax,
      "\n",
      paste(
        paste(latentVarNames, collapse = " "),
        "<- latent(unit = 'rows', distribution = 'bernoulli', structure = 'univariate', type = 'ordinal', link = 'logit')"
      )
    )
  } else {

  }

  # add observed distributions syntax
  blatentSyntax = paste0(blatentSyntax, "\n\n# Observed variable specifications")
  blatentSyntax = paste0(blatentSyntax, "\n", paste(paste(observedVarNames, collapse = " "), "<- observed(distribution = 'bernoulli', link = 'logit')"))



  return(blatentSyntax)


}
