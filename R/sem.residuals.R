#' A Results Output Function
#'
#' This function will display a table of the residual correlation matrix of a lavaan cfa() or sem() model
#' @param x results from a cfa() or sem() lavaan model
#' @export
#' @examples
#' sem.residuals(x)

sem.residuals <- function(x){
  resid <- lavaan::residuals(x, type="cor")$cor
  resid[upper.tri(resid)] <- NA
  diag(resid) <- NA
  knitr::options(knitr.kable.NA='')
  table <- knitr::kable(resid, digits=2, format="html") %>%
  table <- kableExtra::kable_styling(table, full_width=FALSE, position = "left")
  return(table)
}
