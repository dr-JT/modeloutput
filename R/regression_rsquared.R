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

regression_rsquared <- function(x, y = NULL, z = NULL, print = TRUE) {
  x_formula <- insight::find_formula(x)$conditional
  x_n <- insight::model_info(x)$n_obs
  dv <- insight::find_response(x)

  x_summary <- summary(x)

  x_table <- data.frame(model = "H1",
                        r2 = x_summary$r.squared,
                        r2_adj = x_summary$adj.r.squared)

  x_fit <- broom::glance(x)
  x_fit <- dplyr::select(x_fit, logLik, AIC, BIC)

  x_table <- dplyr::bind_cols(x_table, x_fit)

  if (!is.null(y)) {
    y_formula <- insight::find_formula(y)$conditional
    y_n <- insight::model_info(y)$n_obs
    y_summary <- summary(y)
    y_table <- data.frame(model = "H2",
                          r2 = y_summary$r.squared,
                          r2_adj = y_summary$adj.r.squared)

    y_fit <- broom::glance(y)
    y_fit <- dplyr::select(y_fit, logLik, AIC, BIC)

    y_table <- dplyr::bind_cols(y_table, y_fit)

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
    z_formula <- insight::find_formula(z)$conditional
    z_n <- insight::model_info(z)$n_obs
    z_summary <- summary(z)
    z_table <- data.frame(model = "H3",
                          r2 = z_summary$r.squared,
                          r2_adj = z_summary$adj.r.squared)

    z_fit <- broom::glance(z)
    z_fit <- dplyr::select(z_fit, logLik, AIC, BIC)

    z_table <- dplyr::bind_cols(z_table, z_fit)

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
    table <- dplyr::relocate(table, logLik, AIC, BIC, .after = p)
    table <- dplyr::mutate(table, r2_change = round(r2_change, 3),
                           `F Change` = round(`F Change`, 3),
                           p = round(p, 3),
                           BF =
                             dplyr::case_when(model == "H2" ~
                                                exp((dplyr::first(BIC) -
                                                       dplyr::nth(BIC, 2)) / 2),
                                              model == "H3" ~
                                                exp((dplyr::nth(BIC, 2) -
                                                       dplyr::last(BIC)) / 2),
                                              model == "H1" ~ as.numeric(NA)),
                           BF = round(BF, 3),
                           `P(Model|Data)` = BF / (BF + 1),
                           `P(Model|Data)` = round(`P(Model|Data)`, 3))
    table[is.na(table)] <- " "
    col_labels <- c("Model", "$R^2$", "$R^2$ adj.",
                    "$R^2 \\Delta$", "$F \\Delta$", "df1", "df2", "p",
                    "logLik", "AIC", "BIC", "BF", "P(Model|Data)")
    column_align <- c("l", rep("c", 12))
  } else {
    col_labels <- c("Model", "$R^2$", "$R^2$ adj.", "logLik", "AIC", "BIC")
    column_align <- c("l", rep("c", 5))
  }

  if (print == TRUE) {
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = paste("Model Summary: ", dv, sep = ""),
                          row.names = FALSE, col.names = col_labels,
                          align = column_align)
    table <- kableExtra::kable_classic(table, position = "left",
                                       protect_latex = TRUE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left", protect_latex = TRUE)
    table <- kableExtra::row_spec(table, 0, bold = TRUE)
    if (is.null(y) & is.null(z)) {
      table <- kableExtra::footnote(table,
                                    number = paste("<small>", "H1: ", deparse(x_formula),
                                                   "; N = ", x_n, "</small>", sep = ""),
                                    escape = FALSE)
    } else if (!is.null(y) & is.null(z)) {
      table <- kableExtra::footnote(table,
                                    number = c(paste("<small>", "H1: ", deparse(x_formula),
                                                     "; N = ", x_n, "</small>", sep = ""),
                                               paste("<small>", "H2: ", deparse(y_formula),
                                                     "; N = ", y_n, "</small>", sep = "")),
                                    escape = FALSE)
    } else {
      table <- kableExtra::footnote(table,
                                    number = c(paste("<small>", "H1: ", deparse(x_formula),
                                                     "; N = ", x_n, "</small>", sep = ""),
                                               paste("<small>", "H2: ", deparse(y_formula),
                                                     "; N = ", y_n, "</small>", sep = ""),
                                               paste("<small>", "H3: ", deparse(z_formula),
                                                     "; N = ", z_n, "</small>", sep = "")),
                                    escape = FALSE)
    }
  } else if (print == FALSE) {
    table <- as.data.frame(table)
  }

  return(table)
}
