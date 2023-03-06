#' Descriptive Statistics Table
#'
#' This function will display a table of descriptive statistics
#' @param x dataframe
#' @export
#'

descriptives_table <- function(x) {
  col_order <- colnames(x)
  x <- tidyr::gather(x, "Variable", "value")
  x <- dplyr::group_by(x, Variable)
  table <- dplyr::summarise(x,
                            n = length(which(!is.na(value))),
                            Mean = mean(value, na.rm = TRUE),
                            SD = sd(value, na.rm = TRUE),
                            min = min(value, na.rm = TRUE),
                            max = max(value, na.rm = TRUE),
                            Skewness =
                              e1071::skewness(value, na.rm = TRUE, type = 2),
                            Kurtosis =
                              e1071::kurtosis(value, na.rm = TRUE, type = 2),
                            '% Missing' =
                              100 * (length(which(is.na(value))) / dplyr::n()))

  N <- dplyr::summarise(x, N.total = dplyr::n())
  N <- N$N.total[1]
  table <- dplyr::ungroup(table)
  table <- dplyr::arrange(table, match(Variable, col_order))
  table <- knitr::kable(table, digits = 2, format = "html",
                        caption = "Descriptive Statistics")
  table <- kableExtra::kable_classic(table, position = "left")
  table <- kableExtra::kable_styling(table, full_width = FALSE,
                                     position = "left")
  table <- kableExtra::footnote(table, general_title = "\n",
                                general = paste("Total N = ", N, sep = ""))
  return(table)
}
