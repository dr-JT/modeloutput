#' A Results Output Function
#'
#' This function will display a bar graph for between, within, or between-within mean group(s) comparison
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
                       errorbars = "se", color = "black", x.label = NULL, y.label = NULL){
  if (!is.null(y.label)){
    colnames(x)[which(colnames(x)==measurevar)] <- y.label
    measurevar <- y.label
  }
  if (is.null(withinvars)){
    if (length(betweenvars)==1){
      legend <- FALSE
      group <- color
    } else {
      legend <- TRUE
      color <- betweenvars[2]
      group <- betweenvars[2]
    }
    if (!is.null(x.label)){
      colnames(x)[which(colnames(x)==betweenvars)] <- x.label
      betweenvars[1] <- x.label
    }
    x.sum <- Rmisc::summarySE(x, measurevar = measurevar, groupvars = betweenvars, na.rm = TRUE)
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = get(betweenvars[1]), y = get(measurevar),
                                            group = group, color = color))

  } else {
    if (is.null(betweenvars)){
      if (length(withinvars)==1){
        legend <- FALSE
        group <- color
      } else {
        legend <- TRUE
        color <- withinvars[2]
        group <- withinvars[2]
      }
    } else {
      legend <- TRUE
      group <- betweenvars
      color <- betweenvars
    }
    if (!is.null(x.label)){
      colnames(x)[which(colnames(x)==withinvars[1])] <- x.label
      withinvars[1] <- x.label
    }
    x.sum <- Rmisc::summarySEwithin(x, measurevar = measurevar, withinvars = withinvars, betweenvars = betweenvars,
                                    idvar = idvar, na.rm = TRUE)
    plot <- ggplot2::ggplot(x, ggplot2::aes(x = get(withinvars[1]), y = get(measurevar),
                                            group = group, color = color))

  }

  plot <- plot +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = .05), size = .5, shape = 20, alpha = 0.4, color = group)+
    ggplot2::geom_point(data = x.sum,
                        ggplot2::aes(x = get(withinvars[1]), y = get(measurevar)), shape = 18, size = 4, color = "black") +
    ggplot2::geom_errorbar(data = x.sum,
                           ggplot2::aes(x = get(withinvars[1]), y = get(measurevar),
                                        ymin = get(measurevar)-get(errorbars), ymax = get(measurevar)+get(errorbars)),
                           width = .15, color = "black")+
    ggplot2::labs(x = x.label, y = y.label)

  if (legend==FALSE){
    plot <- plot +
      ggplot2::theme(legend.position = "none")
  }
  return(plot)
}
