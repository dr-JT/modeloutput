#' Regression Model Tables
#'
#' Display all tables relevant for Regression models. You can include up to
#' three models (x, y, and z) for hierarchical regression results.
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

table_model <- function(x, y = NULL, z = NULL, print = TRUE) {
  fit_table <- table_fit(x, y, z, print = TRUE)
  rsquared_table <- table_rsquared(x, y, z, print = print)
  coeff_table <- table_coeff(x, y, z)

  if (fit_table != "") print(fit_table)
  if (rsquared_table != "") print(rsquared_table)
  if (coeff_table != "") print(coeff_table)
}
