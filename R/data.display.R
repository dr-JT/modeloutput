#' A Data Output Function
#'
#' This function will display a scrollable data table
#' @param x dataframe
#'
#' @templateVar fun data.display
#' @template template-depr_fun
NULL

#' @templateVar old data.display
#' @templateVar new data_display
#' @template template-depr_pkg
#'
#' @export
#' @examples
#' datadisplay(x)

data.display <- function(x){
  .Deprecated("data_display")
  numcols <- sapply(x, is.numeric)
  x[which(numcols==TRUE)] <- round(x[which(numcols==TRUE)], 2)
  table <- DT::datatable(x, extensions = 'FixedColumns',
                         options = list(
                           pageLength = length(x[[1]]),
                           dom = 't',
                           scrollX = TRUE,
                           scrollY = "800px",
                           fixedColumns = list(leftColumns = 2)))
  return(table)
}
