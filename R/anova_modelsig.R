#' ANOVA F-Table for Overall Main Effect and Interaction Terms
#'
#' @param x an lmer model object
#' @param digits How many decimal places to round to? Default is 3.
#' @param id_col The column containing subject ids. Default is "Subject"
#' @export
#'

anova_modelsig <- function(x, digits = 3, id_col = "Subject") {
  x_formula <- insight::get_call(x)
  x_obs <- insight::model_info(x)$n_obs
  x_n <- length(unique(insight::get_data(x)[[id_col]]))
  dv <- insight::find_response(x)

  format_table <- function(x, digits = digits) {
    x <- dplyr::rename(x, Term = Parameter, Sum_of_Squares = Sum_Squares)
    x <- dplyr::mutate(x,
                       dplyr::across(Sum_of_Squares:`F`,
                                     ~ round(.x, digits = digits)))
    x <- dplyr::mutate(x,
                       dplyr::across(Eta2_partial:Epsilon2_partial,
                                     ~ round(.x, digits = digits)))
    x <- dplyr::mutate(x,
                       p = round(p, 3))
    return(x)
  }

  x_anova <- anova(x)
  x_anova <- dplyr::mutate(x_anova, Parameter = rownames(x_anova))
  x_anova <- dplyr::select(x_anova, Parameter, df_error = DenDF)

  table <- parameters::model_parameters(anova(x), type = 3,
                                        eta_squared = TRUE,
                                        omega_squared = TRUE,
                                        epsilon_squared = TRUE)
  table <- as.data.frame(table)
  table <- dplyr::mutate(table, Mean_Square_Error = Mean_Square / `F`)

  table <- merge(table, x_anova, by = "Parameter", all = TRUE)
  table <- dplyr::relocate(table, df_error, .after = df)
  table <- dplyr::relocate(table, Mean_Square_Error, .after = Mean_Square)
  table <- dplyr::relocate(table, Eta2_partial, .before = Omega2_partial)

  table <- format_table(table, digits = digits)

  table <- knitr::kable(table, digits = digits, format = "html",
                        caption = paste("ANOVA Table: ", dv, sep = ""),
                        row.names = FALSE,
                        align = c("l", rep("c", 10)))
  table <- kableExtra::kable_classic(table, position = "left")
  table <- kableExtra::kable_styling(table, full_width = FALSE,
                                     position = "left")
  table <- kableExtra::row_spec(table, 0, bold = TRUE)
  table <- kableExtra::footnote(table,
                                number =
                                  c(paste("<small>", "Model: ", deparse(x_formula),
                                          "</small>", sep = ""),
                                    paste("<small>", "N = ", x_n,
                                          "</small>", sep = ""),
                                    paste("<small>", "Observations = ", x_obs,
                                          "</small>", sep = "")),
                                escape = FALSE)

  return(table)
}
