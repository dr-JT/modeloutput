#' A Results Output Function
#'
#' This function will display the results of an lmer() HLM model
#' @param x lmer object
#' @param random_effects random effects variable name (default = "Subject")
#' @export
#' @examples
#' lmer.output(x)

lmer.output <- function(x, random_effects = "Subject"){
  x.summary <- summary(x)
  title <- x.summary$methTitle
  grps <- x.summary$ngrps[[random_effects]]
  obs <- x.summary$devcomp$dims[["n"]]
  n <- data.frame(Groups = grps, Observations = obs)
  colnames(n) <- c(paste("Groups ", "(", random_effects, ")", sep = ""), "Observations")
  n <- knitr::kable(n, digits=3, format="html")
  n <- kableExtra::kable_styling(n, full_width=FALSE, position="left")
  random <- as.data.frame(VarCorr(x))
  colnames(random) <- c("Group", "Variable 1", "Variable 2", "Variance", "Std Deviation")
  random <- knitr::kable(random, digits=3, format="html")
  random <- kableExtra::kable_styling(random, full_width=FALSE, position="left")
  fixed <- as.data.frame(x.summary$coefficients)
  colnames(fixed) <- c("Estimate", "Std.Error", "df", "t", "p-value")
  fixed <- knitr::kable(fixed, digits=3, format="html")
  fixed <- kableExtra::kable_styling(fixed, full_width=FALSE, position="left")


  print(title)
  print(n)
  print(random)
  print(fixed)
}
