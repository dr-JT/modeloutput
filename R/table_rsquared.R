#' R-Squared Values
#'
#' Display a table of R-Squared values. If more than one model is added
#' (x, y, and z), then R-Squared and relevant stats will be displayed.
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

table_rsquared <- function(x, y = NULL, z = NULL, print = TRUE) {
  x_summary <- summary(x)

  x_table <- data.frame(model = "H1",
                        r2 = x_summary$r.squared,
                        r2_adj = x_summary$adj.r.squared)

  if (!is.null(y)) {
    y_summary <- summary(y)
    y_table <- data.frame(model = "H2",
                          r2 = y_summary$r.squared,
                          r2_adj = y_summary$adj.r.squared)
    y_comp <- anova(x, y)
    y_comp <- dplyr::filter(y_comp, !is.na(Df))
    y_comp <- dplyr::mutate(y_comp,
                            r2_change =
                              y_summary$r.squared - x_summary$r.squared)
    y_comp <- dplyr::select(y_comp, r2_change,
                            `F Change` = `F`, df1 = Df, df2 = Res.Df,
                            p = `Pr(>F)`)
    y_table <- dplyr::bind_cols(y_table, y_comp)
    x_table <- dplyr::mutate(x_table, r2_change = NA, `F Change` = NA,
                      df1 = NA, df2 = NA, p = NA)
  } else {
    y_table <- data.frame()
  }
  if (!is.null(z)) {
    z_summary <- summary(z)
    z_table <- data.frame(model = "H3",
                          r2 = z_summary$r.squared,
                          r2_adj = z_summary$adj.r.squared)
    z_comp <- anova(y, z)
    z_comp <- dplyr::filter(z_comp, !is.na(Df))
    z_comp <- dplyr::mutate(z_comp,
                            r2_change =
                              z_summary$r.squared - y_summary$r.squared)
    z_comp <- dplyr::select(z_comp, r2_change,
                            `F Change` = `F`, df1 = Df, df2 = Res.Df,
                            p = `Pr(>F)`)
    z_table <- dplyr::bind_cols(z_table, z_comp)
  } else {
    z_table <- data.frame()
  }

  table <- dplyr::bind_rows(x_table, y_table, z_table)
  if (!is.null(y)) {
    table <- dplyr::mutate(table, r2_change = round(r2_change, 3),
                           `F Change` = round(`F Change`, 3),
                           p = round(p, 3))
    table[is.na(table)] <- " "
    colnames(table) <- c("Model", "R-squared", "R-squared adj.",
                         "R-squared Change", "F Change", "df1", "df2", "p")
  } else {
    colnames(table) <- c("Model", "R-squared", "R-squared adj.")
  }

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "R-Squared Values", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left",
                                       bootstrap_options = "striped")
  } else if (print == FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}
