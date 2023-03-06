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
#' @export
#'

regression_coeff <- function(x, y = NULL, z = NULL,
                             standardized = TRUE,
                             unstandardized = TRUE,
                             ci = 0.95, ci_method = NULL,
                             bootstrap = FALSE, iterations = NULL,
                             effects = "all",
                             digits = 3) {

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
  }

  table <- dplyr::relocate(table, Model, .befor = Term)

  if (unstandardized == TRUE) {
    if (standardized == TRUE) {
      header_names <- c("Model", "Term", "b", "SE", "95% CI", "B", "SE",
                        "95% CI", "t", "df", "p")
      table <- knitr::kable(table, format = "html",
                            caption = paste("Regression Coefficients: ",
                                            dv, sep = ""),
                            row.names = FALSE,
                            col.names = header_names,
                            align = c("l", "l", rep("c", 9)))
      table <- kableExtra::kable_classic(table, position = "left")
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left")
      table <- kableExtra::row_spec(table, 0, bold = TRUE)
      table <- kableExtra::add_header_above(table,
                                            c(" ", " ", "Unstandardized" = 3,
                                              "Standardized" = 3,
                                              " ", " ", " "), bold = TRUE)
      table <- kableExtra::collapse_rows(table, columns = 1, valign = "top")
    }

    if (standardized == FALSE) {
      header_names <- c("Model", "Term", "b", "SE", "95% CI", "t", "df", "p")
      table <- knitr::kable(table, format = "html",
                            caption = paste("Regression Coefficients: ",
                                            dv, sep = ""),
                            row.names = FALSE,
                            col.names = header_names,
                            align = c("l", "l", rep("c", 6)))
      table <- kableExtra::kable_classic(table, position = "left")
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left")
      table <- kableExtra::row_spec(table, 0, bold = TRUE)
      table <- kableExtra::add_header_above(table,
                                            c(" ", " ", "Unstandardized" = 3,
                                              " ", " ", " "), bold = TRUE)
      table <- kableExtra::collapse_rows(table, columns = 1, valign = "top")
    }
  }

  if (unstandardized == FALSE) {
    if (standardized == TRUE) {
      table <- dplyr::select(table, -b, -SE, -CI_unstd)
      header_names <- c("Model", "Term", "B", "SE",
                        "95% CI", "t", "df", "p")
      table <- knitr::kable(table, format = "html",
                            caption = paste("Regression Coefficients: ",
                                            dv, sep = ""),
                            row.names = FALSE,
                            col.names = header_names,
                            align = c("l", "l", rep("c", 6)))
      table <- kableExtra::kable_classic(table, position = "left")
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left")
      table <- kableExtra::row_spec(table, 0, bold = TRUE)
      table <- kableExtra::add_header_above(table,
                                            c(" ", " ", "Standardized" = 3,
                                              " ", " ", " "), bold = TRUE)
      table <- kableExtra::collapse_rows(table, columns = 1, valign = "top")
    }
  }



  if (is.null(y) & is.null(z)) {
    table <- kableExtra::footnote(table,
                                  number = paste("<small>", "H1: ", deparse(x_formula),
                                                 "; N = ", x_n, "</small>", sep = ""),
                                  escape = FALSE)
  } else if (!is.null(y) & is.null(z)) {
    table <- kableExtra::footnote(table,
                                  number = c(paste("<small>", "H1: ", deparse(x_formula),
                                                   "; N = ", x_n, "</small>", sep = ""),
                                             paste("<small>", "H2: ", deparse(y_formula),
                                                   "; N = ", y_n, "</small>", sep = "")),
                                  escape = FALSE)
  } else {
    table <- kableExtra::footnote(table,
                                  number = c(paste("<small>", "H1: ", deparse(x_formula),
                                                   "; N = ", x_n, "</small>", sep = ""),
                                             paste("<small>", "H2: ", deparse(y_formula),
                                                   "; N = ", y_n, "</small>", sep = ""),
                                             paste("<small>", "H3: ", deparse(z_formula),
                                                   "; N = ", z_n, "</small>", sep = "")),
                                  escape = FALSE)
  }

  return(table)
}
