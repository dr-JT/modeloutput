#' Insert ANOVA Section
#'
#' @export
#'

add_anova <- function() {
  anova_url <- "https://raw.githubusercontent.com/dr-JT/modeloutput/main/inst/extdata/_extensions/anova.qmd"

  # Make a GET request to retrieve the raw content of the file
  anova_text <- httr::content(httr::GET(anova_url), as = "text")
  rstudioapi::insertText(anova_text)
}
