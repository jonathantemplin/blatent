# blatent
Bayesian Latent Variable Models in R

## Changelist

### Version 0.1.2RC1
* Fixed bug in syntax reading that did not allow for multiple non-list variables on left-hand side of model equation
* Fixed bug in calculation of latent variable summaries for BayesNets specifications
* Implemented check for observed variables that are listed in model but are non-numeric class in input data
* Created two plot functions: 
  * plot(model, type="fit-heatmap") for heatmaps of discrepancies when PPMC covariance, Pearson correlation, and/or tetrachoric correlation are used
  * plot(model, type="nonconverged", PSRFcut = 1.1) for plotting trace and density plots of MCMC chains for model parameters with PSRF > PSRFcut
* Fixed an issue where joint variables could have appeared in model statements as DVs
* Fixed an issue where DVs could be listed multiple times
