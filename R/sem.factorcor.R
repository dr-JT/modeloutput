#' A Results Output Function
#'
#' This function will display a table of Latent factor correlations
#' @param x results from a cfa() or sem() lavaan model
#' @param standardized logical whether to include standardized loadings (default = TRUE)
#' @param factors list c() of factors included in the model
#' @export
#' @examples
#' sem.factorcor(x)

sem.factorcor <- function(x, standardized = TRUE, factors = c()){
  x <- lavaan::parameterEstimates(x, standardized = standardized)
  x <- dplyr::filter(x, op=="~~", lhs %in% factors, !is.na(pvalue))
  x <- dplyr::mutate(x, stars = ifelse(pvalue < .001, "***",
                                       ifelse(pvalue < .01, "**",
                                              ifelse(pvalue < .05, "*", ""))))
  x <- dplyr::select(x, 'Factor 1'=lhs, 'Factor 2'=rhs, r=est, sig=stars)
  table <- knitr::kable(x, digits=3, format="html", caption="Latent Factor Correlations")
  table <- kableExtra::kable_styling(table, full_width = FALSE, position = "left")
  return(table)
}
