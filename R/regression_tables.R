#' Regression Model Tables
#'
#' Display all tables relevant for Regression models. You can include up to
#' three models (x, y, and z) for hierarchical regression results.
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param standardized Logical, indicating whether or not to print standardized
#'      estimates. Standardized estimates are based on "refit" of the model
#'      on standardized data but it will not standardize categorical predictors.
#'      Defualt is TRUE.
#' @param unstandardized Logical, indicating whether or not to print
#'      unstandardized estimates. Default is TRUE.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95%)
#' @param ci_method Documention based on ?parameters::parameters.
#'     Method for computing degrees of freedom for confidence
#'     intervals (CI) and the related p-values. Allowed are following options
#'     (which vary depending on the model class): "residual", "normal",
#'     "likelihood", "satterthwaite", "kenward", "wald", "profile", "boot",
#'     "uniroot", "ml1", "betwithin", "hdi", "quantile", "ci", "eti", "si",
#'     "bci", or "bcai". See section Confidence intervals and approximation of
#'     degrees of freedom in model_parameters() for further details.
#'     When ci_method=NULL, in most cases "wald" is used then.
#' @param bootstrap Documention based on ?parameters::parameters.
#'     Should estimates be based on bootstrapped model? If TRUE, then arguments
#'     of Bayesian regressions apply (see also bootstrap_parameters()).
#' @param iterations Documention based on ?parameters::parameters.
#'     The number of bootstrap replicates. This only apply in the case of
#'     bootstrapped frequentist models.
#' @param digits How many decimal places to round to? Default is 3.
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

regression_tables <- function(x, y = NULL, z = NULL,
                              standardized = TRUE,
                              unstandardized = TRUE,
                              ci = 0.95, ci_method = NULL,
                              bootstrap = FALSE, iterations = NULL,
                              digits = 3,
                              print = TRUE) {

  table_rsquared <- regression_rsquared(x, y, z, print = TRUE)
  table_modelsig <- regression_modelsig(x, y, z, print = TRUE)
  table_coeff <- regression_coeff(x, y, z,
                                  standardized = standardized,
                                  unstandardized = unstandardized,
                                  ci = ci, ci_method = ci_method,
                                  bootstrap = bootstrap,
                                  iterations = iterations,
                                  digits = digits)

  print(table_rsquared)
  print(table_modelsig)
  print(table_coeff)
}
