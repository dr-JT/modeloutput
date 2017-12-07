#' A Data Output Function
#'
#' This function will print a scatter plot with the correlation value in the upper right
#' @param df dataframe
#' @param x x-axis variable
#' @param y y-axis variable
#' @param p.value probablity value to test for significance (default = .05)
#' @param se logical. Display standard error? (default = FALSE)
#' @param line.color default = "dodgerblue4"
#' @param point.color default = "black"
#' @param x.lim Lower and upper limits of x-axis (c(lower,upper)). If one of the limits should be defined automatically set to NA
#' @param y.lim Lower and upper limits of x-axis (c(lower,upper)). If one of the limits should be defined automatically set to NA
#' @param x.by Interval of tick marks for x-axis
#' @param y.by Interval of tick marks for y-axis
#' @param title Add a plot title
#' @param subtitle Add a plot subtitle
#' @param caption Add a plot caption
#' @param grp What is the grouping variable?
#' @param point.show Show data points?
#' @export plot.scatter
#' @examples
#' plot.scatter(data, x = "x.variable", y = "y.variable", se = TRUE, x.lim = c(NA,3), y.lim = c(0,100))

plot.scatter <- function(df, x = "", y = "", p.value = .05, se = FALSE,
                         line.color = "dodgerblue4", point.color = "black",
                         x.lim = "", y.lim = "", x.by = 1, y.by = 1, title = "",
                         subtitle = "", caption = "", grp = "", point.show = TRUE){

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

  plot <- ggplot2::theme(plot.margin = ggplot2::unit(c(.5,2,.5,.5), "cm"),
                         panel.background = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_line(color = "gray96"),
                         plot.background = ggplot2::element_rect(fill="white", color = NA),
                         legend.background = ggplot2::element_rect(fill="transparent", color = NA),
                         legend.key = ggplot2::element_rect(fill="transparent", color = NA),
                         panel.border = ggplot2::element_rect(fill="transparent", color = NA),
                         axis.line = ggplot2::element_line(color = "black"),
                         axis.title = ggplot2::element_text(face="bold"),
                         axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t=18)),
                         axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r=18), angle = 90))

  if (point.show==FALSE){
    point.color = "white"
  }
  if (grp==""){
    r <- sub("0.", ".", round(psych::corr.test(df[c(x,y)])$r[2],2))
    p <- psych::corr.test(df[c(x,y)])$p[2]
    if (p < p.value){
      text <- paste("r = ", r, "*", sep = "")
    } else {
      text <- paste("r = ", r, sep = "")
    }

    label <- grid::grobTree(grid::textGrob(text, x=1.01, y=.90,
                                           check.overlap = FALSE,
                                           just = c("left", "top"),
                                           gp = grid::gpar(fontfamily = "Arial",
                                                           col="black",
                                                           fontsize = 14,
                                                           fontface = "bold")))

    plot <- plot +
      ggplot2::ggplot(df, ggplot2::aes(x = get(x), y = get(y) )) +
      ggplot2::labs(x = x, y = y, title = title, subtitle = subtitle, caption = caption) +
      ggplot2::geom_point(shape=19, position = "jitter", color = point.color) +
      ggplot2::geom_smooth(method=lm,se=se, color = line.color) +
      ggplot2::annotation_custom(label)
    plot <- xy.scale(plot, x.lim = x.lim, y.lim = y.lim, x.by = x.by, y.by = y.by)

  } else {
    plot <- plot +
      ggplot2::ggplot(df, ggplot2::aes(x = get(x), y = get(y), color = get(grp))) +
      ggplot2::labs(x = x, y = y, title = title, subtitle = subtitle, caption = caption, color = grp) +
      ggplot2::geom_smooth(method=lm,se=se)
    plot <- xy.scale(plot, x.lim = x.lim, y.lim = y.lim, x.by = x.by, y.by = y.by)
    if (point.show==TRUE){
      plot <- plot + ggplot2::geom_point(shape=19, position = "jitter")
    } else {
      plot <- plot + ggplot2::geom_point(shape=19, position = "jitter", color = point.color)
    }
  }

  plot <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  plot$layout$clip[plot$layout$name=="panel"] <- "off"
  plot(plot)
}
