#' Table style
#'
#' @param x a table
#' @export
#'

table_styling <- function(x) {
  x <- x |>
    gt::tab_options(
      table.align = "left",
      table.border.top.width = 0,
      table.border.bottom.width = 0,
      table_body.border.bottom.color = "black",
      heading.align = "center",
      heading.padding = 8,
      heading.border.bottom.color = "black",
      heading.title.font.size = 14,
      heading.title.font.weight = "bolder",
      column_labels.border.top.width = 0,
      column_labels.border.bottom.color = "black",
      column_labels.font.weight = "bold",
      column_labels.padding = 6,
      footnotes.font.size = 13
    ) |>
    gt::tab_style(style = gt::cell_text(align = 'center'),
                  locations = gt::cells_column_labels()) |>
    gt::opt_horizontal_padding(scale = 2) |>
    gt::cols_align_decimal(columns = dplyr::everything()) |>
    gt::sub_missing(missing_text = "")
  return(x)
}
