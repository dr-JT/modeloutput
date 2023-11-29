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
    table <- dplyr::relocate(table, df_error, .after = df)
    table <- dplyr::relocate(table, Mean_Square_Error, .after = Mean_Square)
    ws_design <- TRUE
  } else {
    ws_design <- FALSE
  }

  if (model_type == "afex_aov") {
    table <- dplyr::select(table, -Method)
  }

  table <- dplyr::rename(table, Term = Parameter, Sum_of_Squares = Sum_Squares)

  table_title <- paste("ANOVA Table: ", dv, sep = "")
  df_correction <- attr(x$anova_table, "correction")
  if (df_correction == "GG") {
    df_correction <- "Greenhouse-Geisser"
  }

  gt_table <- gt::gt(table) |>
    table_styling() |>
    gt::tab_header(title = table_title) |>
    gt::cols_label(Sum_of_Squares = "SS",
                   Mean_Square = "MS")

  if (ws_design == TRUE) {
    gt_table <- gt::cols_label(gt_table,
                               Sum_Squares_Error = "SS Error",
                               df_error = "df Error",
                               Mean_Square_Error = "MS Error")
  }

  if (eta_squared == TRUE) {
    if ("Eta2_partial" %in% colnames(table)) {
      gt_table <- gt::cols_label(gt_table,
                                 Eta2_partial = "{{:eta:_p^2}}")
    } else {
      gt_table <- gt::cols_label(gt_table,
                                 Eta2 = "{{:eta:^2}}")
    }

  }

  if (omega_squared == TRUE) {
    if ("Omega2_partial" %in% colnames(table)) {
      gt_table <- gt::cols_label(gt_table,
                                 Omega2_partial = "{{:omega:_p^2}}")
    } else {
      gt_table <- gt::cols_label(gt_table,
                                 Omega2 = "{{:omega:^2}}")
    }

  }

  if (epsilon_squared == TRUE) {
    if ("Epsilon2_partial" %in% colnames(table)) {
      gt_table <- gt::cols_label(gt_table,
                                 Epsilon2_partial = "{{:epsilon:_p^2}}")
    } else {
      gt_table <- gt::cols_label(gt_table,
                                 Epsilon2 = "{{:epsilon:^2}}")
    }

  }

  gt_table <- gt_table |>
    gt::cols_align(align = "left", columns = 1) |>
    gt::sub_small_vals(columns = p, threshold = .001) |>
    gt::fmt_number(decimals = digits, use_seps = FALSE) |>
    gt::tab_footnote(paste("Model: ", add_fun_name,
                           deparse1(x_formula), add_parenth, sep = "")) |>
    gt::tab_footnote(paste("N = ", x_n, sep = ""))

  if (stringr::str_detect(model_type, "lmer")) {
    gt_table <- gt::tab_footnote(gt_table,
                                 paste("Observations = ", x_obs, sep = ""))
  }

  gt_table <- gt::tab_footnote(gt_table,
                               paste("df correction: ",
                                     df_correction, sep = ""))

  return(gt_table)
}
