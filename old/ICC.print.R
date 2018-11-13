#' A Results Output Function
#'
#' This function will display the ICC value of an HLM model
#' @param x lmer object
#' @param random_effects random effects variable name (default = "Subject")
#' @export
#' @examples
#' ICC.print(x)

ICC.print <- function(x, random_effects = "Subject"){
  variance <- data.frame(lme4::VarCorr(x))[c("grp", "vcov")]
  grp.var <- variance[which(variance$grp==random_effects),]$vcov
  resid.var <- variance[which(variance$grp=="Residual"),]$vcov
  table <- data.frame(ICC = grp.var/(grp.var+resid.var))
  table <- knitr::kable(table, digits=3, format="html")
  table <- kableExtra::kable_styling(table, full_width = FALSE, position = "left")
  return(table)
}
