#' A Results Output Function
#'
#' This function will display a table and table for between, within, or between-within mean group(s) comparison
#' @param x dataframe
#' @param measurevar a
#' @param withinvars a
#' @param betweenvars a
#' @param idvar a
#' @param errorbars a
#' @param errorbars.color a
#' @param bar.color a
#' @param x.abel a
#' @param y.label a
#' @export plot.means
#' @examples
#' table.means(x)

table.means <- function(x, measurevar, withinvars = NULL, betweenvars = NULL, idvar = NULL){

  if (is.null(withinvars)){
    x <- Rmisc::summarySE(x, measurevar = measurevar, groupvars = betweenvars, na.rm = TRUE)
  } else {
    x <- Rmisc::summarySEwithin(x, measurevar = measurevar, withinvars = withinvars, betweenvars = betweenvars,
                                idvar = idvar, na.rm = TRUE)
  }

  colnames(x)[which(colnames(x)=="N")] <- "n"
  colnames(x)[which(colnames(x)==measurevar)] <- "Mean"
  colnames(x)[which(colnames(x)=="sd")] <- "SD"
  colnames(x)[which(colnames(x)=="se")] <- "SE"
  colnames(x)[which(colnames(x)=="ci")] <- "CI"

  x <- knitr::kable(x, digits=2, format="html", caption="Mean Comparisons")
  x <- kableExtra::kable_styling(x, full_width = FALSE, position = "left")
  return(x)
}
