#' A Data Output Function
#'
#' This function will print a scatter plot with the correlation value in the upper right
#' @param df dataframe
#' @param x x-axis variable
#' @param y y-axis variable
#' @param se which variable to use for the standard error bars
#' @param x.lim Lower and upper limits of x-axis (c(lower,upper)). If one of the limits should be defined automatically set to NA
#' @param y.lim Lower and upper limits of x-axis (c(lower,upper)). If one of the limits should be defined automatically set to NA
#' @param x.by Interval of tick marks for x-axis
#' @param y.by Interval of tick marks for y-axis
#' @param title Add a plot title
#' @param subtitle Add a plot subtitle
#' @param caption Add a plot caption
#' @param grp What is the grouping variable?
#' @param se.transparency The level of transparance of error bars. 0 = completely transparent
#' @export plot.timeseries
#' @examples
#' plot.timeseries(data, x = "x.variable", y = "y.variable", se = TRUE, x.lim = c(NA,3), y.lim = c(0,100))

plot.timeseries <- function(df, x = "", y = "", se = "",
                         x.lim = "", y.lim = "", x.by = 1, y.by = 1, title = "",
                         subtitle = "", caption = "", grp = "", se.transparency = .1){

  xy.scale <- function(plot, x.lim = "", y.lim = "", x.by = "", y.by = ""){
    if (length(x.lim)==2 & length(y.lim)==2){
      plot <- plot +
        ggplot2::scale_x_continuous(breaks = seq(x.lim[1], x.lim[2], x.by)) +
        ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], y.by)) +
        ggplot2::coord_cartesian(ylim = y.lim, xlim = x.lim)

    } else if (length(x.lim)==2){
      plot <- plot +
        ggplot2::scale_x_continuous(breaks = seq(x.lim[1], x.lim[2], x.by)) +
        ggplot2::coord_cartesian(xlim = x.lim)
    } else if (length(y.lim)==2){
      plot <- plot +
        ggplot2::scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], y.by)) +
        ggplot2::coord_cartesian(ylim = y.lim)
    }
    return(plot)
  }

  if (grp==""){
    plot <- plot + ggplot2::ggplot(df, ggplot2::aes(x = get(x), y = get(y)))
  } else {
    plot <- plot + ggplot2::ggplot(df, ggplot2::aes(x = get(x), y = get(y), color = factor(get(grp))))
  }

  plot <- ggplot2::geom_line(na.rm = TRUE)

  if (se!=""){
    plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = (get(y)-get(grp)),
                                     ymax = (get(y)+get(grp))),
                                 alpha = se.transparency)
  }
  plot <- xy.scale(plot, x.lim = x.lim, y.lim = y.lim, x.by = x.by, y.by = y.by)
  return(plot)
}
