#' Create Coefficients Data Frame
#'
#' @param x an lm model object
#' @param standardized see regression_coeff()
#' @param ci see regression_coeff()
#' @param ci_method see regression_coeff()
#' @param bootstrap see regression_coeff()
#' @param iterations see regression_coeff()
#' @param effects "fixed" or "all" fixed and random effects.
#'     default is "all"
#' @param digits see regression_coeff()
#' @export
#'

get_coeff <- function(x,
                      standardized = TRUE,
                      ci = 0.95, ci_method = NULL,
                      bootstrap = FALSE, iterations = NULL, effects = "all",
                      digits = 3) {

  format_table <- function(x, digits = digits) {
    x <- as.data.frame(x)
    x <- dplyr::select(x, Term = Parameter, b = Coefficient,
                       SE, CI_low, CI_high,
                       t, df = df_error, p)
    x <- dplyr::mutate(x,
                       CI_low = round(CI_low, 3), CI_high = round(CI_high, 3))
    x <- tidyr::unite(x, CI, CI_low, CI_high, sep = ", ")
    x <- dplyr::mutate(x,
                       CI = paste("[", CI, "]", sep = ""))
    return(x)
  }

  table <- parameters::parameters(x, effects = effects,
                                  standardize = NULL,
                                  ci = ci, ci_method = ci_method,
                                  bootstrap = bootstrap, iterations = iterations)
  table <- format_table(table, digits = digits)
  table <- dplyr::rename(table, CI_unstd = CI)

  if (standardized == TRUE) {
    table_std <- parameters::parameters(x, effects = effects,
                                        standardize = "refit",
                                        ci = ci, ci_method = ci_method,
                                        bootstrap = bootstrap,
                                        iterations = iterations)
    table_std <- format_table(table_std, digits = digits)
    table_std <- dplyr::rename(table_std, B = b, SE_B = SE, CI_std = CI)
    table_std <- dplyr::select(table_std, -t, -df, -p)

    table <- merge(table, table_std, by = "Term", all = TRUE)
    table <- dplyr::relocate(table, t, df, p, .after = CI_std)
  }

  return(table)
}
