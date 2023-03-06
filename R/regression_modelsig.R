#' Regression Model Fit
#'
#' Displays a table for the fit of regression models. Multiple models can
#' be added (x, y, and z).
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

regression_modelsig <- function(x, y = NULL, z = NULL, print = TRUE) {
  x_formula <- insight::find_formula(x)$conditional
  x_n <- insight::model_info(x)$n_obs
  dv <- insight::find_response(x)

  x_table <- car::Anova(x, type = "III")
  x_table <- dplyr::mutate(x_table,
                           Term = rownames(x_table),
                           Term = ifelse(Term == "Residuals",
                                         "Residual", "Regression"),
                           Model = "H1")
  x_table <- dplyr::group_by(x_table, Model, Term)
  x_table <- dplyr::summarise(x_table, Df = sum(Df), `Sum Sq` = sum(`Sum Sq`))
  x_table <- dplyr::ungroup(x_table)
  x_table <- dplyr::mutate(x_table, `Mean Sq` = `Sum Sq` / Df)

  x_fit <- broom::glance(x)
  x_fit <- dplyr::select(x_fit, statistic, p.value)
  x_fit <- dplyr::mutate(x_fit, Term = "Regression")

  x_table <- merge(x_table, x_fit, by = "Term", all = TRUE)
  x_table <- dplyr::select(x_table,
                           Model, Term, `Sum Sq`, Df, `Mean Sq`,
                           statistic, p.value)

  if (!is.null(y)) {
    y_formula <- insight::find_formula(y)$conditional
    y_n <- insight::model_info(y)$n_obs
    y_table <- car::Anova(y, type = "III")
    y_table <- dplyr::mutate(y_table,
                             Term = rownames(y_table),
                             Term = ifelse(Term == "Residuals",
                                           "Residual", "Regression"),
                             Model = "H2")
    y_table <- dplyr::group_by(y_table, Model, Term)
    y_table <- dplyr::summarise(y_table, Df = sum(Df), `Sum Sq` = sum(`Sum Sq`))
    y_table <- dplyr::ungroup(y_table)
    y_table <- dplyr::mutate(y_table, `Mean Sq` = `Sum Sq` / Df)

    y_fit <- broom::glance(y)
    y_fit <- dplyr::select(y_fit, statistic, p.value)
    y_fit <- dplyr::mutate(y_fit, Term = "Regression")

    y_table <- merge(y_table, y_fit, by = "Term", all = TRUE)
    y_table <- dplyr::select(y_table,
                             Model, Term, `Sum Sq`, Df, `Mean Sq`,
                             statistic, p.value)
  } else {
    y_table <- data.frame()
    y_top <- data.frame()
  }
  if (!is.null(z)) {
    z_formula <- insight::find_formula(z)$conditional
    z_n <- insight::model_info(z)$n_obs
    z_table <- car::Anova(z, type = "III")
    z_table <- dplyr::mutate(z_table,
                             Term = rownames(z_table),
                             Term = ifelse(Term == "Residuals",
                                           "Residual", "Regression"),
                             Model = "H3")
    z_table <- dplyr::group_by(z_table, Model, Term)
    z_table <- dplyr::summarise(z_table, Df = sum(Df), `Sum Sq` = sum(`Sum Sq`))
    z_table <- dplyr::ungroup(z_table)
    z_table <- dplyr::mutate(z_table, `Mean Sq` = `Sum Sq` / Df)

    z_fit <- broom::glance(z)
    z_fit <- dplyr::select(z_fit, statistic, p.value)
    z_fit <- dplyr::mutate(z_fit, Term = "Regression")

    z_table <- merge(z_table, z_fit, by = "Term", all = TRUE)
    z_table <- dplyr::select(z_table,
                             Model, Term, `Sum Sq`, Df, `Mean Sq`,
                             statistic, p.value)
  } else {
    z_table <- data.frame()
  }

  table <- dplyr::bind_rows(x_table, y_table, z_table)
  table <- dplyr::mutate(table, statistic = round(statistic, 3),
                         p.value = round(p.value, 3))
  table[is.na(table)] <- " "
  colnames(table) <- c("Model", "Term", "Sum of Squares", "df", "Mean Square",
                       "F-value", "p")

  if (print == TRUE) {
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = paste("ANOVA Table: ", dv, sep = ""),
                          row.names = FALSE,
                          align = c("l", "l", rep("c", 5)))
    table <- kableExtra::kable_classic(table, position = "left")
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
    table <- kableExtra::collapse_rows(table, columns = 1, valign = "top")
    table <- kableExtra::row_spec(table, 0, bold = TRUE)
    if (is.null(y) & is.null(z)) {
      table <- kableExtra::footnote(table,
                                    number = paste("<small>", "H1: ",
                                                   deparse1(x_formula),
                                                   "; N = ", x_n, "</small>",
                                                   sep = ""),
                                    escape = FALSE)
    } else if (!is.null(y) & is.null(z)) {
      table <- kableExtra::footnote(table,
                                    number = c(paste("<small>", "H1: ",
                                                     deparse1(x_formula),
                                                     "; N = ", x_n, "</small>",
                                                     sep = ""),
                                               paste("<small>", "H2: ",
                                                     deparse1(y_formula),
                                                     "; N = ", y_n, "</small>",
                                                     sep = "")),
                                    escape = FALSE)
    } else {
      table <- kableExtra::footnote(table,
                                    number = c(paste("<small>", "H1: ",
                                                     deparse1(x_formula),
                                                     "; N = ", x_n, "</small>",
                                                     sep = ""),
                                               paste("<small>", "H2: ",
                                                     deparse1(y_formula),
                                                     "; N = ", y_n, "</small>",
                                                     sep = ""),
                                               paste("<small>", "H3: ",
                                                     deparse1(z_formula),
                                                     "; N = ", z_n, "</small>",
                                                     sep = "")),
                                    escape = FALSE)
    }
  } else if (print == FALSE) {
    table <- as.data.frame(table)
  }

  return(table)
}
