#' @title Syntax specifications for blatent
#'
#' @description The blatent model syntax provides the specifications for a Bayesian latent variable model.
#'
#' @details The model syntax, encapsulated in quotation marks, consists of up to three components:
#' \enumerate{
#'   \item \emph{\bold{Model Formulae:}} R model-like formulae specifying the model for all observed and latent variables in the model. See \code{\link{formula}} for
#'   R formula specifics. Blatent model formulae differ only in that more than one variable can be provided to the left of the \code{~}.
#'
#'   In this section of syntax, there are no differences between latent and observed variables. Model statements are formed using
#'   the linear predictor for each variable. This means that to specify a measurement model, the latent variables will appear to the right-hand side of the \code{~}.
#'
#'   Examples:
#'   \itemize{
#'      \item Measurement model where one latent variable (LV) predicts ten items (item1-item10, implying item1, item2, ..., item10):
#'
#'      \code{item1-item10 ~ LV}
#'
#'      \item One observed variable (X) predicting another observed variable (Y):
#'
#'      \code{Y ~ X}
#'
#'      \item Two items (itemA and itemB) measuring two latent variables (LV1, LV2) with a latent variable interaction:
#'
#'      \code{itemA itemB ~ LV1 + LV2 + LV1:LV2}
#'
#'      \item Two items (itemA and itemB) measuring two latent variables (LV1, LV2) with a latent variable interaction (R \code{\link{formula}} shorthand):
#'
#'      \code{itemA itemB ~ LV1*LV2}
#'
#'      \item Measurement model with seven items (item1-item7) measuring three latent variables (A1, A2, A3) from Chapter 9 of Rupp, Templin, Henson (2010):
#'
#'      \code{item1 ~ A1}
#'
#'      \code{item2 ~ A2}
#'
#'      \code{item3 ~ A3}
#'
#'      \code{item4 ~ A1 + A2 + A1:A2}
#'
#'      \code{item5 ~ A1 + A3 + A1:A3}
#'
#'      \code{item6 ~ A2 + A3 + A2:A3}
#'
#'      \code{item7 ~ A1 + A2 + A3 + A1:A2 + A1:A3 + A2:A3 + A1:A2:A3}
#'   }
#'
#'   \item \emph{\bold{Latent Variable Specifications:}} Latent variables are declared using a unevaluated function call to
#'         the \code{\link{latent}} function. Here, only the latent variables are declared along with options for their estimation.
#'         See \code{\link{latent}} for more information.
#'
#'   \code{A1 A2 A3 <- latent(unit = 'rows', distribution = 'mvbernoulli', structure = 'joint', type = 'ordinal', jointName = 'class')}
#'
#'         Additionally, blatent currently uses a Bayesian Inference Network style of specifying the distributional associations between
#'         latent variables: Model statements must be given to specify any associations between latent variables. By default,
#'         all latent variables are independent, which is a terrible assumption. To fix this, for instance, as shown in
#'         Hu and Templin (2020), the following syntax will give a model that is equivalent to the saturated model for a DCM:
#'
#'   \preformatted{
#'   # Structural Model
#'   A1 ~ 1
#'   A2 ~ A1
#'   A3 ~ A1 + A2 + A1:A2
#'   }
#
#'   \item \emph{\bold{Observed Variable Specifications:}} Observed variables are declared using a unevaluated function call to
#'         the \code{\link{observed}} function. Here, only the observed variables are declared along with options for their estimation.
#'         See \code{\link{observed}} for more information.
#'
#'   \code{item1-item7 <- observed(distribution = 'bernoulli', link = 'probit')}
#' }
#'
#' Continuing with the syntax example from above, the full syntax for the model in Chapter 9 of Rupp, Templin, Henson (2010) is:
#'
#' \preformatted{
#' modelText = "
#' # Measurement Model
#'
#' item1 ~ A1
#' item2 ~ A2
#' item3 ~ A3
#' item4 ~ A1 + A2 + A1:A2
#' item5 ~ A1 + A3 + A1:A3
#' item6 ~ A2 + A3 + A2:A3
#' item7 ~ A1 + A2 + A3 + A1:A2 + A1:A3 + A2:A3 + A1:A2:A3
#'
#' # Structural Model
#' A1 ~ 1
#' A2 ~ A1
#' A3 ~ A1 + A2 + A1:A2
#'
#  # Latent Variable Specifications:
#' A1 A2 A3 <- latent(unit = 'rows', distribution = 'bernoulli', structure = 'univariate', type = 'ordinal')
#'
#' # Observed Variable Specifications:
#' item1-item7 <- observed(distribution = 'bernoulli', link = 'probit')
#'"}
#'
#' @references
#'  Rupp, A. A., Templin, J., & Henson, R. A. (2010). Diagnostic Measurement: Theory, Methods, and Applications. New York: Guilford.
#'
#'  Hu, B., & Templin, J. (2020). Using diagnostic classification models to validate attribute hierarchies and
#'   evaluate model fit in Bayesian networks. Multivariate Behavioral Research. https://doi.org/10.1080/00273171.2019.1632165
#'
#'
#' @name blatentSyntax
#'
NULL
