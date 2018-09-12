#' A Results Output Function
#'
#' This function will display a graph and table for between, within, or between-within mean group(s) comparison
#' @param x dataframe
#' @param measurevar a
#' @param withinvars a
#' @param betweenvars a
#' @param idvar a
#' @param errorbars a
#' @param errorbars.color a
#' @param bar.color a
#' @param y.label a
#' @export plot.means
#' @examples
#' plot.means(x)

plot.means <- function(x, measurevar = "", withinvars = "", betweenvars = "", idvar = "", errorbars = "se", errorbars.color = "black", bar.color = "", y.label = ""){
  x <- Rmisc::summarySEwithin(x, measurevar = measurevar, withinvars = withinvars, idvar = idvar, na.rm = TRUE)
  plot <- ggplot(x, aes(x = get(withinvars), y = get(measurevar), group = 1)) +
    geom_bar(stat = "identity", fill = bar.color) +
    geom_errorbar(aes(ymin = get(measurevar)-get(errorbars), ymax = get(measurevar)+get(errorbars)), width = .5, color = errorbars.color) +
    labs(x = withinvars, y = y.label)
  x <- knitr::kable(x, digits=2, format="html", caption="Mean Comparisons")
  x <- kableExtra::kable_styling(x)
  print(plot)
  return(x)
}
