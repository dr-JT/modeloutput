#' Regression Coefficients
#'
#' Displays a table for regression coefficients using sjPlot::tab_model().
#' Multiple models can be added (x, y, and z).
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @export
#'

table_coeff <- function(x, y = NULL, z = NULL) {
  if (is.null(y) & is.null(z)) {
    table <- sjPlot::tab_model(x)
  } else if (!is.null(y) & is.null(z)) {
    table <- sjPlot::tab_model(x, y)
  } else if (!is.null(z)) {
    table <- sjPlot::tab_model(x, y, z)
  }

  return(table)
}
