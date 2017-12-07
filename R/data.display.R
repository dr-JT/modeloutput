#' A Data Output Function
#'
#' This function will display a scrollable data table
#' @param x dataframe
#' @export
#' @examples
#' data.display(x)

data.display <- function(x){
  numcols <- sapply(x, is.numeric)
  x[which(numcols==TRUE)] <- round(x[which(numcols==TRUE)], 2)
  table <- DT::datatable(x, extensions = 'FixedColumns',
                         options = list(
                           pageLength = length(x[[1]]),
                           dom = 't',
                           scrollX = TRUE,
                           scrollY = "400px",
                           fixedColumns = list(leftColumns = 2)))
  return(table)
}
