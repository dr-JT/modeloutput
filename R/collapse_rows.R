#' Collapse rows
#'
#' Taken from https://gist.github.com/jmclawson/6852c14023d7d9b7e91bbcfa419adeb8
#'
#' @param x a table
#' @export
#'

collapse_rows <- function(df_g, col, lookleft = TRUE){
  col_num <- grep(deparse(substitute(col)), colnames(df_g$`_data`))

  collapse_style <- gt::css(visibility = "hidden",
                        border_top = "0px")

  test_rows <- function(x) ifelse(is.na(x == lag(x)), FALSE, x == lag(x))

  if (col_num > 1 & lookleft) {
    col_left <- as.name(colnames(df_g$`_data`)[col_num - 1])
    df_g |>
      gt::tab_style(
        style = collapse_style,
        locations = gt::cells_body(columns = {{ col }},
                               rows = test_rows({{ col }}) & test_rows({{ col_left }})))
  } else {
    df_g |>
      gt::tab_style(
        style = collapse_style,
        locations = gt::cells_body(columns = {{ col }},
                               rows = test_rows({{ col }})))
  }
}
