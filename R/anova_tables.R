#' ANOVA F-Table for Overall Main Effect and Interaction Terms
#'
#' @param x an lmer model object
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
#' @param id_col The column containing subject ids. Default is "Subject"
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

anova_tables <- function(x, standardized = TRUE,
                         unstandardized = TRUE,
                         ci = 0.95, ci_method = NULL,
                         bootstrap = FALSE, iterations = NULL,
                         digits = 3, id_col = "Subject",
                         print = TRUE) {

  table_modelsig <- anova_modelsig(x, digits = digits, id_col = id_col)

  table_contrasts <- regression_coeff(x,
                                      standardized = standardized,
                                      unstandardized = unstandardized,
                                      ci = ci, ci_method = ci_method,
                                      bootstrap = bootstrap,
                                      iterations = iterations,
                                      digits = digits)

  model_terms <- insight::find_variables(x)$conditional
  table_comparisons <- list()
  i <- 1
  for (term in model_terms) {
    table_comparisons[i] <- anova_comparisons(x, term = term, digits = digits)
    i <- i + 1
  }

  print(table_modelsig)
  print(table_contrasts)
  print(table_comparisons[1])
  ifelse((i - 1) > 1, print(table_comparisons[2]), "")
  ifelse((i - 1) > 2, print(table_comparisons[3]), "")
}
