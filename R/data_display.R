#' A Data Output Function
#'
#' This function will display a scrollable data table
#' @param x dataframe
#' @param round How many decimal places to round to?
#' @param height The height of the displayed table in px
#' @param left.freeze How many columns (starting from the left) to freeze
#' @export
#' @examples
#' data_display(x)

data_display <- function(x, round = 2, height = "800px", left.freeze = 1){
  left.freeze <- left.freeze + 1
  numcols <- sapply(x, is.numeric)
  x[which(numcols==TRUE)] <- round(x[which(numcols==TRUE)], round)
  table <- DT::datatable(x, extensions = 'FixedColumns',
                         options = list(
                           pageLength = length(x[[1]]),
                           dom = 't',
                           scrollX = TRUE,
                           scrollY = height,
                           fixedColumns = list(leftColumns = left.freeze)))
  return(table)
}
