#' Post-hoc Comparison Tables
#'
#' @param x an lmer model object
#' @param contrast The factor at which to compare levels at
#' @param at A second interacting factor to compare the effect of contrast at
#' @param p_adjust The p-values adjustment method for frequentist multiple
#'     comparisons. Can be one of "holm", "tukey", "hochberg",
#'     "hommel", "bonferroni", "BH", "BY", "fdr" or "none" (default). See the
#'     p-value adjustment section in the emmeans::test documentation.
#' @param digits How many decimal places to round to? Default is 3.
#' @param pbkrtest.limit Optional parameter that can be set to help calculate dfs.
#'     If you need to use this a warning message will appear in the console
#'     telling you what to set this at.
#' @param lmerTest.limit Optional parameter that can be set to help calculate dfs.
#'     If you need to use this a warning message will appear in the console
#'     telling you what to set this at.
#' @export
#'

anova_comparisons <- function(x, contrast = NULL, at = NULL, p_adjust = "none",
                              digits = 3,
                              pbkrtest.limit = NULL,
                              lmerTest.limit = NULL) {

  model_type <- insight::model_name(x)

  contrast_levels <- levels(insight::get_data(x)[[contrast]])
  if (model_type == "afex_aov") {
    contrast_levels <- stringr::str_remove(contrast_levels, "X")
  }
  if (!is.null(at)) {
    at_levels <- levels(insight::get_data(x)[[at]])
    if (model_type == "afex_aov") {
      at_levels <- stringr::str_remove(at_levels, "X")
    }
  }

  if (is.null(pbkrtest.limit) & is.null(lmerTest.limit)) {
    table <- modelbased::estimate_contrasts(x, contrast = contrast, at = at,
                                            p_adjust = p_adjust)
  } else {
    table <- modelbased::estimate_contrasts(x, contrast = contrast, at = at,
                                            p_adjust = p_adjust,
                                            pbkrtest.limit = pbkrtest.limit,
                                            lmerTest.limit = lmerTest.limit)
  }

  p_adjust <- stringr::str_split(attr(table, "table_footer")[1], "\n")[[1]][3]

  table_std <- effectsize::standardize(table)

  if (is.null(at)) {
    table_std <- dplyr::select(table_std, Level1, Level2, Cohen_D = Difference)
    table <- merge(table, table_std, by = c("Level1", "Level2"))
  }

  if (!is.null(at)) {
    table_std <- dplyr::select(table_std, Level1, Level2,
                               dplyr::all_of(at), Cohen_D = Difference)
    table <- merge(table, table_std, by = c("Level1", "Level2", at))
  }

  if (model_type == "afex_aov") {
    table <- dplyr::mutate(table,
                           Level1 = stringr::str_remove(Level1, "X"),
                           Level2 = stringr::str_remove(Level2, "X"))
  }
  table <- dplyr::mutate(table,
                         Level1 = factor(Level1, levels = contrast_levels),
                         Level2 = factor(Level2, levels = contrast_levels))

  table <- dplyr::arrange(table, Level1, Level2)

  table_title <- paste("Post-hoc Comparisons: ", contrast, sep = "")
  cols_left_align <- c(1,2)

  if (!is.null(at)) {
    colnames(table)[which(colnames(table) == at)] <- "placeholder"
    if (model_type == "afex_aov") {
      table <- dplyr::mutate(table,
                             placeholder =
                               stringr::str_remove(placeholder, "X"))
    }
    table <- dplyr::mutate(table,
                           placeholder =
                             factor(placeholder, levels = at_levels))
    table <- dplyr::arrange(table, Level1, Level2, placeholder)
    colnames(table)[which(colnames(table) == "placeholder")] <- at

    table_title <- paste("Post-hoc Comparisons: ",
                         contrast, " x ", at, sep = "")
    cols_left_align <- c(1,2,3)
  }

  table <- gt::gt(table) |>
    table_styling() |>
    gt::tab_header(title = table_title) |>
    gt::cols_merge_range(col_begin = CI_low,
                         col_end = CI_high,
                         sep = gt::html("&nbsp;&ndash;&nbsp;")) |>
    gt::cols_label(Level1 = "Level 1",
                   Level2 = "Level 2",
                   Cohen_D = "Cohen's D",
                   CI_low = "CI 95%") |>
    gt::cols_align(align = "left", columns = cols_left_align) |>
    gt::sub_small_vals(columns = p, threshold = .001) |>
    gt::fmt_number(decimals = digits) |>
    gt::fmt_number(columns = df, decimals = 0) |>
    gt::tab_footnote(p_adjust)

  return(table)
}
