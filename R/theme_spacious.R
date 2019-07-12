#' Spacious ggplot2 theme
#'
#' This will increase the distance between axis titles and axis labels
#' @param font.size font size
#' @param bold bold labels?
#' @export theme_spacious
#'

theme_spacious <- function(font.size = 14, bold = TRUE){
  key.size <- trunc(font.size*.8)
  if (bold == TRUE) {
    face.type <- "bold"
  } else {
    face.type <- "plain"
  }

  theme(text = element_text(size = font.size),
        axis.title.x = element_text(margin = margin(t = 20, r = 0,
                                                    b = 0, l = 0),
                                    face = face.type),
        axis.title.y = element_text(margin = margin(t = 0, r = 20,
                                                    b = 0, l = 0),
                                    face = face.type),
        legend.title = element_text(face = face.type),
        legend.spacing = unit(20, "pt"),
        legend.text = element_text(size = key.size))
}
