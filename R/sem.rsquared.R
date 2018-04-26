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
  colnames(x) <- c("Variable", "R-Squared")
  table <- knitr::kable(x, digits=3, format="html", caption="R-Squared Values")
  table <- kableExtra::kable_styling(table)
  return(table)
}
