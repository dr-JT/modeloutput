#' ANOVA F-Table for Overall Main Effect and Interaction Terms
#'
#' @param x an lmer model object
#' @param eta_squared logical. Include partial-eta sqaured effect size?
#'     Default: TRUE
#' @param omega_squared logical. Include omega sqaured effect size?
#'     Default: TRUE
#' @param epsilon_squared logical. Include epsilon sqaured effect size?
#'     Default: FALSE
#' @param digits How many decimal places to round to? Default is 3.
#' @param id_col The column containing subject ids. Default is "Subject"
#' @export
#'

anova_modelsig <- function(x,
                           eta_squared = TRUE,
                           omega_squared = TRUE,
                           epsilon_squared = FALSE,
                           digits = 3, id_col = "Subject") {

  format_table <- function(x, digits = digits) {
    x <- dplyr::rename(x, Term = Parameter, Sum_of_Squares = Sum_Squares)
    x <- dplyr::mutate(x,
                       dplyr::across(Sum_of_Squares:`F`,
                                     ~ round(.x, digits = digits)))
    if (eta_squared == TRUE) {
      x <- dplyr::mutate(x, Eta2_partial = round(Eta2_partial, digits))
    }
    if (omega_squared == TRUE) {
      x <- dplyr::mutate(x, Omega2_partial = round(Omega2_partial, digits))
    }
    if (epsilon_squared == TRUE) {
      x <- dplyr::mutate(x, Epsilon2_partial = round(Epsilon2_partial, digits))
    }
    x <- dplyr::mutate(x,
                       p = round(p, 3))
    return(x)
  }
  effectsize_list <- c()
  if (eta_squared == TRUE) {
    effectsize_list <- c(effectsize_list, "eta")
  }
  if (omega_squared == TRUE) {
    effectsize_list <- c(effectsize_list, "omega")
  }
  if (epsilon_squared == TRUE) {
    effectsize_list <- c(effectsize_list, "epsilon")
  }

  model_type <- insight::model_name(x)
  if (stringr::str_detect(model_type, "lmer")) {
    x_formula <- insight::get_call(x)
    add_fun_name <- ""
    add_parenth <- ""
    x_obs <- insight::model_info(x)$n_obs
    x_n <- length(unique(insight::get_data(x)[[id_col]]))
    x_parameters <- anova(x)
    x_anova <- dplyr::mutate(x_parameters, Parameter = rownames(x_parameters))
    x_anova <- dplyr::select(x_anova, Parameter, df_error = DenDF)
  }
  if (model_type == "afex_aov" | model_type == "aov") {
    x_formula <- insight::find_formula(x)$conditional
    add_fun_name <- "aov_car("
    add_parenth <- ")"
    x_obs <- ""
    x_n <- insight::model_info(x)$n_obs
    x_parameters <- x
  }
  dv <- insight::find_response(x)

  table <- parameters::model_parameters(x_parameters, type = 3,
                                        effectsize_type = effectsize_list)
  table <- as.data.frame(table)

  if ("df_error" %in% colnames(table)) {
    table <- dplyr::mutate(table, Mean_Square_Error = Mean_Square / `F`)
    if (stringr::str_detect(model_type, "lmer")) {
      table <- merge(table, x_anova, by = "Parameter", all = TRUE)
    }
    if (model_type == "afex_aov") {
      table <- dplyr::select(table, -Method)
    }
    table <- dplyr::relocate(table, df_error, .after = df)
    table <- dplyr::relocate(table, Mean_Square_Error, .after = Mean_Square)
  }
  c_col <- ncol(table) - 1

  table <- format_table(table, digits = digits)

  table <- knitr::kable(table, digits = digits, format = "html",
                        caption = paste("ANOVA Table: ", dv, sep = ""),
                        row.names = FALSE,
                        align = c("l", rep("c", c_col)))
  table <- kableExtra::kable_classic(table, position = "left")
  table <- kableExtra::kable_styling(table, full_width = FALSE,
                                     position = "left")
  table <- kableExtra::row_spec(table, 0, bold = TRUE)
  if (stringr::str_detect(model_type, "lmer")) {
    table <- kableExtra::footnote(table,
                                  number =
                                    c(paste("<small>", "Model: ",
                                            add_fun_name, deparse1(x_formula),
                                            add_parenth, "</small>", sep = ""),
                                      paste("<small>", "N = ", x_n,
                                            "</small>", sep = ""),
                                      paste("<small>", "Observations = ", x_obs,
                                            "</small>", sep = "")),
                                  escape = FALSE)
  } else {
    table <- kableExtra::footnote(table,
                                  number =
                                    c(paste("<small>", "Model: ",
                                            add_fun_name, deparse1(x_formula),
                                            add_parenth, "</small>", sep = ""),
                                      paste("<small>", "N = ", x_n,
                                            "</small>", sep = "")),
                                  escape = FALSE)
  }

  return(table)
}
