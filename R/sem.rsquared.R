#' A Results Output Function
#'
#' This function will display a table of R squared values (explained variance)
#' @param x results from a cfa() or sem() lavaan model
#' @export
#' @examples
#' sem.rsquared(x)

sem.rsquared <- function(x){
  x <- lavaan::inspect(x, 'r2')
  x <- data.frame(x)
  x <- tibble::rownames_to_column(x)
  colnames(x) <- c("Variable", "R-Squared")
  table <- knitr::kable(x, digits=3, format="html", caption="R-Squared Values", row.names = FALSE)
  table <- kableExtra::kable_styling(table)
  return(table)
}
