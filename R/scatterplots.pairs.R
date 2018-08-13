#' A Data Output Function
#'
#' This function will print out scatterplots of all pairs of variables in a dataframe.
#' Histograms on x and y variables are displayed on the margins, using ggMarginal()
#' Correlation statistics are printed at the bottom left of figure.
#' @param x dataframe
#' @param id.label column that contains subject IDs
#' @export
#' @examples
#' scatterplots.pairs(x, variable.list = colnames(x))

scatterplots.pairs <- function(x, id.label = ""){

  x <- dplyr::select(x, -(id.label))
  variable.list <- colnames(x)
  # loop over all possible combination of pairs
  for (i in 1:length(variable.list)){
    if (i!=length(variable.list)){
      for (k in (i+1):length(variable.list)){

        # Print a table of variable pair. This is handy to use Ctrl+F search
        t <- data.frame(x = variable.list[i], y = variable.list[k])
        t <- knitr::kable(t, format="html")
        t <- kableExtra::kable_styling(t, full_width = FALSE, position = "right")
        grid::grid.newpage()
        print(t)

        # Calculate correlation statistics
        corr <- stats::cor.test(x[[variable.list[i]]], x[[variable.list[k]]])
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
        p <- ggplot2::ggplot(x, ggplot2::aes(x = get(variable.list[i]), y = get(variable.list[k]))) +
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
      }
    }
  }
}
