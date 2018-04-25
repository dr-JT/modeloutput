#' A Results Output Function
#'
#' This function will display basic descriptive statistics for a dataframe
#' @param x dataframe
#' @export
#' @examples
#' descriptives(x)

descriptives <- function(x){
  x <- tidyr::gather(x, "Variable", "value")
  x <- dplyr::group_by(x, Variable)
  x <- dplyr::summarise(n = n(),
                        Mean = mean(value, na.rm=TRUE),
                        SD = sd(value, na.rm=TRUE),
                        min = min(value, na.rm=TRUE),
                        max = max(value, na.rm=TRUE),
                        '% Missing' = 100*(length(which(is.na(value)))/n()))
  x <- knitr::kable(x, digits=2, format="html", caption="Descriptive Statistics")
  x <- knitr::kable_styling(x)
  return(x)
}
