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
  x_formula <- insight::find_formula(x)$conditional |>
    deparse() |>
    stringr::str_trim("left") |>
    paste(collapse = "")
  x_n <- insight::model_info(x)$n_obs
  dv <- insight::find_response(x)

  x_summary <- summary(x)

  x_table <- data.frame(model = "H1",
                        r2 = x_summary$r.squared,
                        r2_adj = x_summary$adj.r.squared)

  x_fit <- broom::glance(x)
  x_fit <- dplyr::select(x_fit, BIC)

  x_table <- dplyr::bind_cols(x_table, x_fit)
  footer_x <- paste("H1: ", x_formula, "; N = ", x_n, sep = "")

  if (!is.null(y)) {
    x_table <- dplyr::mutate(x_table, r2_change = NA, F_change = NA,
                             df1 = NA, df2 = NA, p = NA)

    y_formula <- insight::find_formula(y)$conditional |>
      deparse() |>
      stringr::str_trim("left") |>
      paste(collapse = "")
    y_n <- insight::model_info(y)$n_obs
    y_summary <- summary(y)
    y_table <- data.frame(model = "H2",
                          r2 = y_summary$r.squared,
                          r2_adj = y_summary$adj.r.squared)

    y_fit <- broom::glance(y)
    y_fit <- dplyr::select(y_fit, BIC)

    y_table <- dplyr::bind_cols(y_table, y_fit)

    y_comp <- anova(x, y)
    y_comp <- dplyr::filter(y_comp, !is.na(Df))
    y_comp <- dplyr::mutate(y_comp,
                            r2_change =
                              y_summary$r.squared - x_summary$r.squared)
    y_comp <- dplyr::select(y_comp, r2_change,
                            F_change = `F`, df1 = Df, df2 = Res.Df,
                            p = `Pr(>F)`)
    y_table <- dplyr::bind_cols(y_table, y_comp)
    footer_y <- paste("H2: ", y_formula, "; N = ", y_n, sep = "")
  } else {
    y_table <- data.frame()
  }
  if (!is.null(z)) {
    z_formula <- insight::find_formula(z)$conditional |>
      deparse() |>
      stringr::str_trim("left") |>
      paste(collapse = "")
    z_n <- insight::model_info(z)$n_obs
    z_summary <- summary(z)
    z_table <- data.frame(model = "H3",
                          r2 = z_summary$r.squared,
                          r2_adj = z_summary$adj.r.squared)

    z_fit <- broom::glance(z)
    z_fit <- dplyr::select(z_fit, BIC)

    z_table <- dplyr::bind_cols(z_table, z_fit)

    z_comp <- anova(y, z)
    z_comp <- dplyr::filter(z_comp, !is.na(Df))
    z_comp <- dplyr::mutate(z_comp,
                            r2_change =
                              z_summary$r.squared - y_summary$r.squared)
    z_comp <- dplyr::select(z_comp, r2_change,
                            F_change = `F`, df1 = Df, df2 = Res.Df,
                            p = `Pr(>F)`)
    z_table <- dplyr::bind_cols(z_table, z_comp)
    footer_z <- paste("H3: ", z_formula, "; N = ", z_n, sep = "")
  } else {
    z_table <- data.frame()
  }

  table <- dplyr::bind_rows(x_table, y_table, z_table)
  if (!is.null(y)) {
    table <- dplyr::relocate(table, BIC, .after = p)
    table <- dplyr::mutate(table,
                           BF =
                             dplyr::case_when(model == "H2" ~
                                                exp((dplyr::first(BIC) -
                                                       dplyr::nth(BIC, 2)) / 2),
                                              model == "H3" ~
                                                exp((dplyr::nth(BIC, 2) -
                                                       dplyr::last(BIC)) / 2),
                                              model == "H1" ~ as.numeric(NA)),
                           `P(Model|Data)` = BF / (BF + 1))
  }

  if (print == TRUE) {

    table_title <- paste("Model Summary: ", dv, sep = "")

    table <- gt::gt(table) |>
      table_styling() |>
      gt::tab_header(title = table_title) |>
      gt::cols_label(model = "Model",
                     r2 = "{{R^2}}",
                     r2_adj = "{{R^2 adj.}}") |>
      gt::cols_align(align = "left", columns = model) |>
      gt::tab_footnote(footer_x) |>
      gt::fmt_number(decimals = 3, use_seps = FALSE)

    if (!is.null(y)) {
      table <- table |>
        gt::tab_footnote(footer_y) |>
        gt::cols_label(r2_change = "{{:Delta:R^2}}",
                       F_change = "{{:Delta:F}}") |>
        gt::fmt_number(columns = c(df1, df2), decimals = 0, use_seps = FALSE) |>
        gt::fmt_scientific(columns = BF) |>
        gt::sub_small_vals(columns = p, threshold = .001)
    }
    if (!is.null(z)) {
      table <- gt::tab_footnote(table, footer_z)
    }

  } else if (print == FALSE) {
    table <- as.data.frame(table)
  }

  return(table)
}
