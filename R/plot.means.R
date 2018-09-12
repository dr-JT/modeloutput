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
#' @param x.abel a
#' @param y.label a
#' @export plot.means
#' @examples
#' plot.means(x)

plot.means <- function(x, measurevar, withinvars = NULL, betweenvars = NULL, idvar = NULL,
                       errorbars = "se", errorbars.color = "black", bar.color = "grey", x.label = NULL, y.label = NULL){
  if (!is.null(y.label)){
    colnames(x)[which(colnames(x)==measurevar)] <- y.label
    measurevar <- y.label
  }
  if (is.null(withinvars)){
    if (length(betweenvars)==1){
      group <- ""
      fill <- bar.color
    } else {
      fill <- betweenvars[2]
      group <- betweenvars[2]
    }
    if (!is.null(x.label)){
      colnames(x)[which(colnames(x)==betweenvars)] <- x.label
      betweenvars[1] <- x.label
    }
    x <- Rmisc::summarySE(x, measurevar = measurevar, groupvars = betweenvars[1], na.rm = TRUE)
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = get(betweenvars[1]), y = get(measurevar),
                                            group = group, fill = fill))

  } else {
    if (is.null(betweenvars)){
      if (length(withinvars)==1){
        group <- ""
        fill <- bar.color
      } else {
        fill <- withinvars[2]
        group <- withinvars[2]
      }
    } else {
      group <- betweenvars
      fill <- betweenvars
    }
    if (!is.null(x.label)){
      colnames(x)[which(colnames(x)==withinvars[1])] <- x.label
      withinvars[1] <- x.label
    }
    x <- Rmisc::summarySEwithin(x, measurevar = measurevar, withinvars = withinvars[1], betweenvars = betweenvars[1],
                                idvar = idvar, na.rm = TRUE)
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = get(withinvars[1]), y = get(measurevar),
                                            group = group, fill = fill))

  }

  plot <- plot +
    ggplot2::geom_bar(stat = "identity", fill = fill) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = get(measurevar)-get(errorbars), ymax = get(measurevar)+get(errorbars)),
                           width = .5, color = errorbars.color) +
    ggplot2::labs(x = x.label, y = y.label)

  if (group == ""){
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }

  colnames(x)[which(colnames(x)=="N")] <- "n"
  colnames(x)[which(colnames(x)==measurevar)] <- "Mean"
  colnames(x)[which(colnames(x)=="sd")] <- "SD"
  colnames(x)[which(colnames(x)=="se")] <- "SE"
  colnames(x)[which(colnames(x)=="ci")] <- "CI"

  x <- knitr::kable(x, digits=2, format="html", caption="Mean Comparisons")
  x <- kableExtra::kable_styling(x, full_width = FALSE, position = "left")
  output <- list(plot = plot, table = x)
  return(output)
}
