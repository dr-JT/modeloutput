#' Regression Coefficients
#'
#' Displays a table for regression coefficients.
#'      Multiple models can be added (x, y, and z).
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
#' @param effects "fixed" or "all" fixed and random effects.
#'     default is "all"
#' @param digits How many decimal places to round to? Default is 3.
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

regression_coeff <- function(x, y = NULL, z = NULL,
                             standardized = TRUE,
                             unstandardized = TRUE,
                             ci = 0.95, ci_method = NULL,
                             bootstrap = FALSE, iterations = NULL,
                             effects = "all",
                             digits = 3, print = TRUE) {

  table <- get_coeff(x, effects = effects,
                     standardized = standardized,
                     ci = ci, ci_method = ci_method,
                     bootstrap = bootstrap,
                     iterations = iterations,
                     digits = digits)

  table <- dplyr::mutate(table, Model = "H1")
  x_formula <- insight::get_call(x)
  x_n <- insight::model_info(x)$n_obs
  dv <- insight::find_response(x)


  footer_x <- paste("H1: ", deparse(x_formula), "; N = ", x_n, sep = "")

  if (!is.null(y)) {
    y_table <- get_coeff(y, effects = effects,
                         standardized = standardized,
                         ci = ci, ci_method = ci_method,
                         bootstrap = bootstrap,
                         iterations = iterations,
                         digits = digits)
    y_table <- dplyr::mutate(y_table, Model = "H2")
    y_formula <- insight::find_formula(y)$conditional
    y_n <- insight::model_info(y)$n_obs
    table <- dplyr::bind_rows(table, y_table)
    footer_y <- paste("H2: ", deparse(y_formula), "; N = ", y_n, sep = "")
  }
  if (!is.null(z)) {
    z_table <- get_coeff(z, effects = effects,
                         standardized = standardized,
                         ci = ci, ci_method = ci_method,
                         bootstrap = bootstrap,
                         iterations = iterations,
                         digits = digits)
    z_table <- dplyr::mutate(z_table, Model = "H3")
    z_formula <- insight::find_formula(z)$conditional
    z_n <- insight::model_info(z)$n_obs
    table <- dplyr::bind_rows(table, z_table)
    footer_z <- paste("H3: ", deparse(z_formula), "; N = ", z_n, sep = "")
  }

  table <- dplyr::relocate(table, Model, .before = Term)

  if (print == TRUE) {
    table_title <- paste("Regression Coefficients: ", dv, sep = "")

    table <- gt::gt(table) |>
      table_styling() |>
      gt::tab_header(title = table_title) |>
      gt::cols_align(align = "left", columns = c(Model, Term)) |>
      gt::sub_small_vals(columns = p, threshold = .001) |>
      gt::fmt_number(decimals = 3) |>
      gt::fmt_number(columns = df, decimals = 0) |>
      gt::tab_footnote(footer_x)

    if (unstandardized == TRUE & standardized == TRUE) {
      table <- table |>
        gt::tab_spanner(label = "Unstandardized",
                        columns = c(b, SE, CI_unstd)) |>
        gt::tab_spanner(label = "Standardized",
                        columns = c(B, SE_B, CI_std)) |>
        gt::cols_label(CI_unstd = "95% CI",
                       B = "{{:Beta:}}",
                       SE_B = "SE",
                       CI_std = "95% CI")
    }

    if (unstandardized == TRUE & standardized == FALSE) {
      table <- table |>
        gt::tab_spanner(label = "Unstandardized",
                        columns = c(b, SE, CI_unstd)) |>
        gt::cols_label(CI_unstd = "95% CI")
    }

    if (unstandardized == FALSE & standardized == TRUE) {
      table <- table |>
        gt::cols_hide(columns = c(b, SE, CI_unstd)) |>
        gt::tab_spanner(label = "Standardized",
                        columns = c(B, SE_B, CI_std)) |>
        gt::cols_label(B = "{{:Beta:}}",
                       SE_B = "SE",
                       CI_std = "95% CI")
    }

    if (!is.null(y)) {
      table <- gt::tab_footnote(table, footer_y)
    }
    if (!is.null(z)) {
      table <- gt::tab_footnote(table, footer_z)
    }
  } else if (print == FALSE) {
    table <- as.data.frame(table)
  }

  return(table)
}
