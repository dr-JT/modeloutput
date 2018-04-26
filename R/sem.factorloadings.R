#' A Results Output Function
#'
#' This function will display a table of Factor loadings
#' @param x results from a cfa() or sem() lavaan model
#' @param standardized logical whether to include standardized loadings (default = TRUE)
#' @export
#' @examples
#' sem.factorloadings(x)

sem.factorloadings <- function(x, standardized = TRUE){
  x <- lavaan::parameterEstimates(x, standardized = standardized)
  x <- dplyr::filter(x, op=="=~")
  x <- dplyr::mutate(x, stars = ifelse(pvalue < .001, "***",
                          ifelse(pvalue < .01, "**",
                                 ifelse(pvalue < .05, "*", ""))))
  x <- dplyr::select(x, 'Latent Factor'=lhs, Indicator=rhs, Beta=std.all, B=est, SE=se, z, 'sig'=stars)
  table <- knitr::kable(x, digits=3, format="html", caption="Factor Loadings")
  table <- kableExtra::kable_styling(table)
  return(table)
}
