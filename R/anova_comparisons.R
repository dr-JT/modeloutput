#' Post-hoc Comparison Tables
#'
#' @param x an lmer model object
#' @param contrast The factor at which to compare levels at
#' @param at A second interacting factor to compare the effect of contrast at
#' @param adjust The p-values adjustment method for frequentist multiple
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

anova_comparisons <- function(x, contrast = NULL, at = NULL, adjust = "none",
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
                                            adjust = adjust)
  } else {
    table <- modelbased::estimate_contrasts(x, contrast = contrast, at = at,
                                            adjust = ajust,
                                            pbkrtest.limit = pbkrtest.limit,
                                            lmerTest.limit = lmerTest.limit)
  }

  table_std <- effectsize::standardize(table)

  if (is.null(at)) {
    table_std <- dplyr::select(table_std, Level1, Level2, Cohen_D = Difference)
    table <- merge(table, table_std, by = c("Level1", "Level2"))
  }

  if (!is.null(at)) {
    table_std <- dplyr::select(table_std, Level1, Level2,
                               at, Cohen_D = Difference)
    table <- merge(table, table_std, by = c("Level1", "Level2", at))
  }

  table <- dplyr::mutate(table,
                         dplyr::across(Difference:Cohen_D, ~
                                         round(.x, digits = digits)),
                         p = round(p, 3))
  table <- tidyr::unite(table, CI, CI_low, CI_high, sep = ", ")
  table <- dplyr::mutate(table,
                     CI = paste("[", CI, "]", sep = ""))

  if (model_type == "afex_aov") {
    table <- dplyr::mutate(table,
                           Level1 = stringr::str_remove(Level1, "X"),
                           Level2 = stringr::str_remove(Level2, "X"))
  }
  table <- dplyr::mutate(table,
                         Level1 = factor(Level1, levels = contrast_levels),
                         Level2 = factor(Level2, levels = contrast_levels))

  table <- dplyr::arrange(table, Level1, Level2)

  if (is.null(at)) {
    table <- knitr::kable(table, digits = digits,
                          caption = paste("Post-hoc Comparisons: ",
                                          contrast, sep = ""),
                          row.names = FALSE,
                          align = c("l", "l", rep("c", 7)))
  }

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

    table <- knitr::kable(table, digits = digits,
                          caption = paste("Post-hoc Comparisons: ", contrast,
                                          " x ", at, sep = ""),
                          row.names = FALSE,
                          align = c("l", "l", "l", rep("c", 7)))
  }

  table <- kableExtra::kable_classic(table, position = "left")
  table <- kableExtra::kable_styling(table, full_width = FALSE,
                                     position = "left")
  table <- kableExtra::collapse_rows(table, columns = 1, valign = "top")
  table <- kableExtra::row_spec(table, 0, bold = TRUE)
  table <- kableExtra::footnote(table,
                                number =
                                  paste("<small>",
                                        "p-value adjustment method: Holm(1979)",
                                        "</small>", sep = ""),
                                escape = FALSE)

  return(table)
}
