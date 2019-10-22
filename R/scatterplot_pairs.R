#' A Data Output Function
#'
#' This function will print out scatterplots of all pairs of variables in a dataframe.
#' Histograms on x and y variables are displayed on the margins, using ggMarginal()
#' Correlation statistics are printed at the bottom left of figure.
#' @param data dataframe
#' @param x X-axis variables
#' @param y Y-axis variables
#' @param all.pairs Logical. If TRUE then will overwrite x, and y parameters
#' and just print out all combinations of paris in the dataframe. (Default: FALSE)
#' @export
#' @examples
#' scatterplot_pairs(x, variable.list = colnames(x))

scatterplot_pairs <- function(data, x = c(), y = c(), all.pairs = FALSE){

  if (all.pairs==FALSE){
    for (i in seq_along(x)){
      for (k in seq_along(y)){
        # Print a table of variable pair. This is handy to use Ctrl+F search
        grid::grid.newpage()
        print(c(x[i], y[k]))

        # Calculate correlation statistics
        corr <- stats::cor.test(data[[x[i]]], data[[y[k]]])
        corr.r <- round(corr$estimate[[1]], digits = 2)
        corr.p <- corr$p.value
        corr.n <- corr$parameter[[1]] + 2
        # Create text of statistics to add to plot
        if (corr.p < .001) {
          stats <- paste("r(", corr.n, ") = ", corr.r, ", ", "p < .001", sep = "")
        } else if (corr.p < .01) {
          stats <- paste("r(", corr.n, ") = ", corr.r, ", ", "p < .01", sep = "")
        } else if (corr.p >= .05) {
          stats <- paste("r(", corr.n, ") = ", corr.r, ", ", "p > .05", sep = "")
        }

        # Create main scatter plot. Add text annotation
        p <- ggplot2::ggplot(data, ggplot2::aes(x = get(x[i]), y = get(y[k]))) +
          ggplot2::xlab(x[i]) + ggplot2::ylab(y[k]) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method="lm") +
          ggplot2::geom_jitter() +
          ggplot2::theme_light(20) +
          ggplot2::annotate("text", Inf, -Inf, label = stats, hjust = .5, vjust = 4, size = 5) +
          ggplot2::coord_cartesian(clip = 'off')

        # Add marginal histograms
        p <- ggExtra::ggMarginal(p, type = "histogram")

        # Print plot
        grid::grid.newpage()
        grid::grid.draw(p)
      }
    }
  }

  if (all.pairs==TRUE){
    variable.list <- colnames(data)
    # loop over all possible combination of pairs
    for (i in seq_along(variable.list)){
      if (i!=length(variable.list)){
        for (k in (i+1):length(variable.list)){

          # Print a table of variable pair. This is handy to use Ctrl+F search
          grid::grid.newpage()
          print(c(variable.list[i], variable.list[k]))

          # Calculate correlation statistics
          corr <- stats::cor.test(data[[variable.list[i]]], data[[variable.list[k]]])
          corr.r <- round(corr$estimate[[1]], digits = 2)
          corr.p <- corr$p.value
          corr.n <- corr$parameter[[1]] + 2
          # Create text of statistics to add to plot
          if (corr.p < .001) {
            stats <- paste("r(", corr.n, ") = ", corr.r, ", ", "p < .001", sep = "")
          } else if (corr.p < .01) {
            stats <- paste("r(", corr.n, ") = ", corr.r, ", ", "p < .01", sep = "")
          } else if (corr.p >= .05) {
            stats <- paste("r(", corr.n, ") = ", corr.r, ", ", "p > .05", sep = "")
          }

          # Create main scatter plot. Add text annotation
          p <- ggplot2::ggplot(data, ggplot2::aes(x = get(variable.list[i]), y = get(variable.list[k]))) +
            ggplot2::xlab(variable.list[i]) + ggplot2::ylab(variable.list[k]) +
            ggplot2::geom_point() +
            ggplot2::geom_smooth(method="lm") +
            ggplot2::geom_jitter() +
            ggplot2::theme_light(20) +
            ggplot2::annotate("text", Inf, -Inf, label = stats, hjust = .5, vjust = 4, size = 5) +
            ggplot2::coord_cartesian(clip = 'off')

          # Add marginal histograms
          p <- ggExtra::ggMarginal(p, type = "histogram")

          # Print plot
          grid::grid.newpage()
          grid::grid.draw(p)

          rm(corr)
          rm(corr.r)
          rm(corr.p)
          rm(corr.n)
        }
      }
    }
  }
}
