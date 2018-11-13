#' A Data Output Function
#'
#' This function will perform and display a bi-variate correlation analysis
#' @param x dataframe
#' @param use "pairwise" or "complete"
#' @param method "pearson"
#' @param adjust "bonferroni"
#' @export
#' @examples
#' bivariate.corr(x, use = "pairwise", method = "pearson", adjust = "bonferroni")

bivariate.corr <- function(x, use = "pairwise", method = "pearson", adjust = "bonferroni"){
  table.stats <- psych::corr.test(x, use = use, method = method, adjust = adjust)
  r <- knitr::kable(table.stats$r, format = "html", digits = 3, caption = "r values", col.names = 1:length(x)) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "left") %>%
    kableExtra::add_footnote(paste("Computed correlation used ", method, "-method with ", use, "-deletion", sep = ""), notation = "number")
  p <- knitr::kable(table.stats$p, format = "html", digits = 3, caption = "p values", col.names = 1:length(x)) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "left")
  n <- knitr::kable(table.stats$n, format = "html", digits = 3, caption = "n", col.names = 1:length(x)) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE, position = "left")

  print(r)
  print(p)
  print(n)
}
