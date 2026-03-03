#' Create Coefficients Data Frame
#'
#' @param x an lm model object
#' @param standardized see regression_coeff()
#' @param ci_level see regression_coeff()
#' @param ci_method see regression_coeff()
#' @param bootstrap see regression_coeff()
#' @param iterations see regression_coeff()
#' @param effects "fixed" or "all" fixed and random effects.
#'     default is "all"
#' @export
#'

get_coeff <- function(x, standardized = TRUE,
                      ci_level = 0.95, ci_method = NULL,
                      bootstrap = FALSE, iterations = NULL, effects = "all") {

  model_type <- insight::model_name(x)

  # fixed-effects table from lmerTest::summary() with df-matched t-CIs
  lmer_summary_ci_table <- function(mod, ci_level = 0.95) {
    if (!requireNamespace("lmerTest", quietly = TRUE)) {
      stop("Package 'lmerTest' is required to compute Satterthwaite df from summary().")
    }

    if (!inherits(mod, "lmerModLmerTest")) {
      mod <- lmerTest::as_lmerModLmerTest(mod)
    }
    s <- summary(mod)
    tab <- as.data.frame(coef(s))
    tab$Term <- rownames(tab)
    rownames(tab) <- NULL

    alpha <- 1 - ci_level
    crit  <- stats::qt(1 - alpha / 2, df = tab$df)

    tab$CI_low  <- tab$Estimate - crit * tab$`Std. Error`
    tab$CI_high <- tab$Estimate + crit * tab$`Std. Error`

    dplyr::tibble(
      Term = tab$Term,
      Coefficient = tab$Estimate,
      SE = tab$`Std. Error`,
      t  = tab$`t value`,
      df = tab$df,
      p  = tab$`Pr(>|t|)`,
      CI_low  = tab$CI_low,
      CI_high = tab$CI_high
    )
  }

  # --- Unstandardized table
  if (stringr::str_detect(model_type, "lmer")) {

    # Use lmerTest summary for df/p + df-matched t-CIs
    table <- lmer_summary_ci_table(x, ci_level = ci_level) |>
      dplyr::transmute(
        Term,
        b = Coefficient,
        ci_low_unstd  = CI_low,
        ci_high_unstd = CI_high,
        SE, t, df, p
      )

  } else {
  table <- x |>
    parameters::parameters(effects = effects,
                           standardize = NULL,
                           ci = ci_level, ci_method = ci_method,
                           bootstrap = bootstrap, iterations = iterations) |>
    as.data.frame() |>
    dplyr::select(Term = Parameter, b = Coefficient, CI_low, CI_high,
                  SE, t, df = df_error, p) |>
    dplyr::rename(ci_low_unstd = CI_low, ci_high_unstd = CI_high)
  }
  if (standardized == TRUE) {
    if (stringr::str_detect(model_type, "lmer")) {

      if (!requireNamespace("effectsize", quietly = TRUE)) {
        stop("Package 'effectsize' is required for standardize='refit' models.")
      }

      # Create refit-standardized model, then use the same lmerTest summary + df-matched t-CIs
      x_std <- effectsize::standardize(x, method = "refit")

      table_std <- lmer_summary_ci_table(x_std, ci_level = ci_level) |>
        dplyr::transmute(
          Term,
          B = Coefficient,
          ci_low_std  = CI_low,
          ci_high_std = CI_high,
          SE_B = SE
        )

    } else {
    table_std <- x |>
      parameters::parameters(effects = effects,
                             standardize = "refit",
                             ci = ci_level, ci_method = ci_method,
                             bootstrap = bootstrap,
                             iterations = iterations) |>
      as.data.frame() |>
      dplyr::select(Term = Parameter, B = Coefficient,
                    ci_low_std = CI_low, ci_high_std = CI_high, SE_B = SE)
    }
    table <- dplyr::full_join(table, table_std, by = "Term")
    # Keep your column ordering convention: put unstd test stats after SE_B
    if (all(c("SE_B", "t", "df", "p") %in% names(table))) {
      table <- dplyr::relocate(table, t, df, p, .after = SE_B)
    }
  }

  return(table)
}
