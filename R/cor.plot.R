#' A Results Output Function
#'
#' This function will display a Correlation Plot heat map
#' @param x dataframe
#' @param use How to deal with missing data. Pairwise or listwise deletion (default = "pairwise")
#' @export
#' @examples
#' cor.plot(x)

cor.plot <- function(x, use = "pairwise"){
  table_r <- cor(x, use = use)
  table_p <- corrplot::cor.mtest(x)$p
  corrplot::corrplot(table_r, method = "color", type="upper", number.cex = .7,
                             tl.cex = .6, addCoef.col = "black", tl.col = "black", tl.srt = 90,
                             p.mat = table_p,  sig.level = .05, insig = "blank", diag = FALSE,
                             number.digits = 2)
}
