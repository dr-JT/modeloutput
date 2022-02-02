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

table_fit <- function(x, y = NULL, z = NULL, print = TRUE) {
  x_table <- anova(x)
  x_table <- dplyr::mutate(x_table,
                           Term = rownames(x_table),
                           Term = ifelse(Term == "Residuals",
                                         "Residual", "Regression"),
                           Model = ifelse(Term == "Regression", "H1", " "))
  x_table <- dplyr::group_by(x_table, Model, Term)
  x_table <- dplyr::summarise(x_table, Df = sum(Df), `Sum Sq` = sum(`Sum Sq`))
  x_table <- dplyr::ungroup(x_table)
  x_table <- dplyr::mutate(x_table, `Mean Sq` = `Sum Sq` / Df)

  x_fit <- broom::glance(x)
  x_fit <- dplyr::select(x_fit, statistic, p.value, logLik, AIC, BIC)
  x_fit <- dplyr::mutate(x_fit, Term = x_table$Term[2])

  x_table <- merge(x_table, x_fit, by = "Term", all = TRUE)
  x_table <- dplyr::select(x_table,
                           Model, Term, `Sum Sq`, Df, `Mean Sq`,
                           statistic, p.value, logLik, AIC, BIC)

  if (!is.null(y)) {
    y_table <- anova(y)
    y_table <- dplyr::mutate(y_table,
                             Term = rownames(y_table),
                             Term = ifelse(Term == "Residuals",
                                           "Residual", "Regression"),
                             Model = ifelse(Term == "Regression", "H2", " "))
    y_table <- dplyr::group_by(y_table, Model, Term)
    y_table <- dplyr::summarise(y_table, Df = sum(Df), `Sum Sq` = sum(`Sum Sq`))
    y_table <- dplyr::ungroup(y_table)
    y_table <- dplyr::mutate(y_table, `Mean Sq` = `Sum Sq` / Df)

    y_fit <- broom::glance(y)
    y_fit <- dplyr::select(y_fit, statistic, p.value, logLik, AIC, BIC)
    y_fit <- dplyr::mutate(y_fit, Term = y_table$Term[2])

    y_table <- merge(y_table, y_fit, by = "Term", all = TRUE)
    y_table <- dplyr::select(y_table,
                             Model, Term, `Sum Sq`, Df, `Mean Sq`,
                             statistic, p.value, logLik, AIC, BIC)
  } else {
    y_table <- data.frame()
    y_top <- data.frame()
  }
  if (!is.null(z)) {
    z_table <- anova(z)
    z_table <- dplyr::mutate(z_table,
                             Term = rownames(z_table),
                             Term = ifelse(Term == "Residuals",
                                           "Residual", "Regression"),
                             Model = ifelse(Term == "Regression", "H3", " "))
    z_table <- dplyr::group_by(z_table, Model, Term)
    z_table <- dplyr::summarise(z_table, Df = sum(Df), `Sum Sq` = sum(`Sum Sq`))
    z_table <- dplyr::ungroup(z_table)
    z_table <- dplyr::mutate(z_table, `Mean Sq` = `Sum Sq` / Df)

    z_fit <- broom::glance(z)
    z_fit <- dplyr::select(z_fit, statistic, p.value, logLik, AIC, BIC)
    z_fit <- dplyr::mutate(z_fit, Term = z_table$Term[2])

    z_table <- merge(z_table, z_fit, by = "Term", all = TRUE)
    z_table <- dplyr::select(z_table,
                             Model, Term, `Sum Sq`, Df, `Mean Sq`,
                             statistic, p.value, logLik, AIC, BIC)
  } else {
    z_table <- data.frame()
  }

  table <- dplyr::bind_rows(x_table, y_table, z_table)
  table <- dplyr::mutate(table, statistic = round(statistic, 3),
                         p.value = round(p.value, 3), logLik = round(logLik, 3),
                         AIC = round(AIC, 3), BIC = round(BIC, 3))
  table[is.na(table)] <- " "
  colnames(table) <- c("Model", "Term", "Sum of Squares", "df", "Mean Square",
                       "F-value", "p", "log-likelihood", "AIC", "BIC")

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = "Model Summary Fit", row.names = FALSE)
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left",
                                       bootstrap_options = "striped")
  } else if (print == FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}
