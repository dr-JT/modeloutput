#' Frequency distribution histograms for all variables
#'
#' This function will print out frequency distribution historgrams for each
#' variable in a dataframe
#' @param x dataframe
#' @export freq_plot
#' @examples
#' freq_plot(data)

freq_plot <- function(x) {
  sjPlot::set_theme(geom.outline.color = "black", geom.outline.size = .15)
  variableList <- colnames(x)
  i <- 1
  p <- list()
  for (variable in variableList) {
    if (range(x[[variable]], na.rm = TRUE)[1] < 1 &
        range(x[[variable]], na.rm = TRUE)[1] >= 0 &
        range(x[[variable]], na.rm = TRUE)[2] <= 1 &
        range(x[[variable]], na.rm = TRUE)[2] > 0) {
      data.hist <- x[[variable]]
      p[[i]] <- sjPlot::sjp.frq(data.hist,
                        type = "hist",
                        geom.size = .10,
                        axis.title = variable,
                        xlim = c(0,1))
    } else {
      data.hist <- x[[variable]]
      p[[i]] <- sjPlot::sjp.frq(data.hist,
                        type = "hist",
                        axis.title = variable,
                        xlim = c(min(x[[variable]], na.rm = TRUE),
                                 max(x[[variable]], na.rm = TRUE)))
    }
    plot(p[[i]])
    i <- i + 1
  }
}
