#' A Results Output Function
#'
#' This function will display a table of SEM regression paths
#' @param x results from a cfa() or sem() lavaan model
#' @param standardized logical whether to include standardized loadings (default = TRUE)
#' @export
#' @examples
#' sem.paths(x)

sem.paths <- function(x, standardized = TRUE){
  x <- lavaan::parameterEstimates(x, standardized = standardized)
  x <- dplyr::filter(x, op=="~")
  x <- dplyr::mutate(x, stars = ifelse(pvalue < .001, "***",
                                       ifelse(pvalue < .01, "**",
                                              ifelse(pvalue < .05, "*", ""))))
  x <- dplyr::select(x, DV=lhs, IV=rhs, Beta=std.all, B=est, SE=se, z, 'sig'=stars)
  table <- knitr::kable(x, digits=3, format="html", caption="Regression Paths")
  table <- kableExtra::kable_styling(table)
  return(table)
}
