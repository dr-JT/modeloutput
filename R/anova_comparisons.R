#' ANOVA F-Table for Overall Main Effect and Interaction Terms
#'
#' @param x an lmer model object
#' @param term The factor at which to compare levels at
#' @param digits How many decimal places to round to? Default is 3.
#' @param pbkrtest.limit Optional parameter that can be set to help calculate dfs.
#'     If you need to use this a warning message will appear in the console
#'     telling you what to set this at.
#' @param lmerTest.limit Optional parameter that can be set to help calculate dfs.
#'     If you need to use this a warning message will appear in the console
#'     telling you what to set this at.
#' @export
#'

anova_comparisons <- function(x, term = NULL, digits = 3,
                              pbkrtest.limit = NULL,
                              lmerTest.limit = NULL) {

  term_levels <- levels(insight::get_data(x)[[term]])

  if (is.null(pbkrtest.limit) & is.null(lmerTest.limit)) {
    table <- modelbased::estimate_contrasts(x, contrast = term)
  } else {
    table <- modelbased::estimate_contrasts(x, contrast = term,
                                            pbkrtest.limit = pbkrtest.limit,
                                            lmerTest.limit = lmerTest.limit)
  }

  table_std <- effectsize::standardize(table)
  table_std <- dplyr::select(table_std, Level1, Level2, Cohen_D = Difference)

  table <- merge(table, table_std, by = c("Level1", "Level2"))
  table <- dplyr::mutate(table,
                         dplyr::across(Difference:Cohen_D, ~
                                         round(.x, digits = digits)),
                         p = round(p, 3))
  table <- tidyr::unite(table, CI, CI_low, CI_high, sep = ", ")
  table <- dplyr::mutate(table,
                     CI = paste("[", CI, "]", sep = ""))

  table <- dplyr::mutate(table,
                         Level1 = factor(Level1, levels = term_levels),
                         Level2 = factor(Level2, levels = term_levels))
  table <- dplyr::arrange(table, Level1, Level2)

  table <- knitr::kable(table, digits = digits, format = "html",
                        caption = paste("Post-hoc Comparisons: ", term, sep = ""),
                        row.names = FALSE,
                        align = c("l", "l", rep("c", 7)))
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
