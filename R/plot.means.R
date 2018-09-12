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

plot.means <- function(x, measurevar = "", withinvars = "", betweenvars = "", idvar = "",
                       errorbars = "se", errorbars.color = "black", bar.color = "gray", y.label = ""){
  if (withinvars==""){
    if (length(betweenvars)==1){
      fill <- ""
    } else {
      fill <- betweenvars[2]
    }

    x <- Rmisc::summarySE(x, measurevar = measurevar, groupvars = betweenvars[1])
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = get(betweenvars[1]), y = get(measurevar), fill = fill))

    if (x.label==""){
      x.label <- betweenvars
    }
  } else {
    if (betweenvars==""){
      if (length(withinvars)==1){
        fill <- ""
      } else {
        fill <- withinvars[2]
      }
    } else {
      fill = betweenvars
    }

    x <- Rmisc::summarySEwithin(x, measurevar = measurevar, withinvars = withinvars, betweenvars = betweenvars, idvar = idvar, na.rm = TRUE)
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = get(withinvars), y = get(measurevar), fill = fill))

    if (x.label==""){
      x.label <- withinvars
    }
  }
  if (y.label==""){
    y.label <- measurevar
  }

  plot <- plot +
    ggplot2::geom_bar(stat = "identity", color = bar.color) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = get(measurevar)-get(errorbars), ymax = get(measurevar)+get(errorbars)), width = .5, color = errorbars.color) +
    ggplot2::labs(x = x.label, y = y.label)
  x <- knitr::kable(x, digits=2, format="html", caption="Mean Comparisons")
  x <- kableExtra::kable_styling(x, full_width = FALSE, position = "left")
  print(plot)
  return(x)
}
