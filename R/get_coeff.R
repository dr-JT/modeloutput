#' Create Coefficients Data Frame
#'
#' @param x an lm model object
#' @param standardized see regression_coeff()
#' @param ci_level see regression_coeff()
#' @param ci_method see regression_coeff()
#' @param bootstrap see regression_coeff()
#' @param iterations see regression_coeff()
#' @param effects "fixed" or "all" fixed and random effects.
#'     default is "all"
#' @export
#'

get_coeff <- function(x, standardized = TRUE,
                      ci_level = 0.95, ci_method = NULL,
                      bootstrap = FALSE, iterations = NULL, effects = "all") {

  table <- x |>
    parameters::parameters(effects = effects,
                           standardize = NULL,
                           ci = ci_level, ci_method = ci_method,
                           bootstrap = bootstrap, iterations = iterations) |>
    as.data.frame() |>
    dplyr::select(Term = Parameter, b = Coefficient, CI_low, CI_high,
                  SE, t, df = df_error, p) |>
    dplyr::rename(ci_low_unstd = CI_low, ci_high_unstd = CI_high)

  if (standardized == TRUE) {
    table_std <- x |>
      parameters::parameters(effects = effects,
                             standardize = "refit",
                             ci = ci_level, ci_method = ci_method,
                             bootstrap = bootstrap,
                             iterations = iterations) |>
      as.data.frame() |>
      dplyr::select(Term = Parameter, B = Coefficient,
                    ci_low_std = CI_low, ci_high_std = CI_high, SE_B = SE)

    table <- dplyr::full_join(table, table_std, by = "Term")
    table <- dplyr::relocate(table, t, df, p, .after = SE_B)
  }

  return(table)
}
