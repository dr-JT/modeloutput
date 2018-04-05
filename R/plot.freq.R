#' A Data Output Function
#'
#' This function will print out frequency distribution historgrams for each variable in a dataframe
#' @param x dataframe
#' @examples
#' plot.freq(data, x = "x.variable", y = "y.variable", se = TRUE, x.lim = c(NA,3), y.lim = c(0,100))

plot.freq <- function(x){
  set_theme(geom.outline.color = "black", geom.outline.size = .15)
  variableList <- colnames(x)
  i <- 1
  plot <- list()
  for (variable in variableList){
    if (range(x[[variable]], na.rm = TRUE)[1]<1 & range(x[[variable]], na.rm = TRUE)[1]>=0 & range(x[[variable]], na.rm = TRUE)[2]<=1 & range(x[[variable]], na.rm = TRUE)[2]>0) {
      data.hist <- x[[variable]]
      plot[[i]] <- sjp.frq(data.hist,
                           type = "hist",
                           geom.size = .10,
                           axis.title = variable,
                           xlim = c(0,1))
    } else {
      data.hist <- x[[variable]]
      plot[[i]] <- sjp.frq(data.hist,
                           type = "hist",
                           axis.title = variable,
                           xlim = c(min(x[[variable]], na.rm = TRUE), max(x[[variable]], na.rm = TRUE)))
    }
    plot[[i]]
    i <- i + 1
  }
  return(plot)
}
