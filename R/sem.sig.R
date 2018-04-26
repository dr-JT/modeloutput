#' A Results Output Function
#'
#' This function will display a table of Model significance tests
#' @param x results from a cfa() or sem() lavaan model
#' @export
#' @examples
#' sem.sig(x)

sem.sig <- function(x){
  stats <- lavaan::fitMeasures(fit, c("ntotal", "chisq", "pvalue", "df"))
  table <- data.frame(stats, 'Sample Size'=stats[["ntotal"]], 'Chi-Square'=stats[["chisq"]], df=stats[["df"]], 'p-value'=stats[["pvalue"]])
  table <- knitr::kable(table, digits=3, format="html", caption="Model Significance", row.names = FALSE)
  table <- knitr::kable_styling(table, full_width=FALSE, position = "left")
  return(table)
}
