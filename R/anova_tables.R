#' ANOVA Tables
#'
#' Get ANOVA F-Table, contrasts, and pairwise comparisons
#'
#' @param x an lmer model object
#' @param effects "fixed" or "all". default is "fixed" to reduce computation time
#' @param contrast The factor(s) at which to compare levels at
#' @param at Additional interacting factor(s) to compare the effect of contrast at
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
#' @param pbkrtest.limit Optional parameter that can be set to help calculate dfs.
#'     If you need to use this a warning message will appear in the console
#'     telling you what to set this at.
#' @param lmerTest.limit Optional parameter that can be set to help calculate dfs.
#'     If you need to use this a warning message will appear in the console
#'     telling you what to set this at.
#' @param digits How many decimal places to round to? Default is 3.
#' @param id_col The column containing subject ids. Default is "Subject"
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

anova_tables <- function(x, effects = "fixed", contrast = NULL, at = NULL,
                         standardized = TRUE,
                         unstandardized = TRUE,
                         ci = 0.95, ci_method = NULL,
                         bootstrap = FALSE, iterations = NULL,
                         pbkrtest.limit = NULL,
                         lmerTest.limit = NULL,
                         digits = 3, id_col = "Subject",
                         print = TRUE) {

  table_modelsig <- anova_modelsig(x, digits = digits, id_col = id_col)

  table_contrasts <- regression_coeff(x, effects = effects,
                                      standardized = standardized,
                                      unstandardized = unstandardized,
                                      ci = ci, ci_method = ci_method,
                                      bootstrap = bootstrap,
                                      iterations = iterations,
                                      digits = digits)

  writeLines(table_modelsig)
  writeLines(table_contrasts)

  table_comparisons <- list()
  i <- 1
  for (contr in contrast) {
    table_comparisons[[i]] <- anova_comparisons(x, contrast = contr,
                                                digits = digits,
                                                pbkrtest.limit = pbkrtest.limit,
                                                lmerTest.limit = lmerTest.limit)
    writeLines(table_comparisons[[i]])
    i <- i + 1
  }

  if (!is.null(at)) {
    for (contr in contrast) {
      interaction <- at[which(at != contr)]
      for (int in interaction) {
        table_comparisons[[i]] <- anova_comparisons(x, contrast = contr, at = int,
                                                    digit = digits,
                                                    pbkrtest.limit = pbkrtest.limit,
                                                    lmerTest.limit = lmerTest.limit)
        writeLines(table_comparisons[[i]])
        i <- i + 1
      }
    }
  }
}
