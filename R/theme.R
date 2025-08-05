
#' Pretty theme
#'
#' @export
theme_spruce <- function(rotate = TRUE, rotate_angle_x = 45, rotate_angle_y = 0, frac = 0.01) {
  ggplot2::theme(
    axis.text.x  = element_text_spruce(rotate = rotate, rotate_angle = rotate_angle_x, frac = frac),
    axis.text.y  = element_text_spruce(rotate = rotate, rotate_angle = rotate_angle_y, frac = frac),
    axis.title.x = element_text_spruce(rotate = FALSE, frac = frac),
    axis.title.y = element_text_spruce(rotate = FALSE, frac = frac),
    legend.text  = element_text_spruce(rotate = FALSE, frac = frac),
    strip.text   = element_text_spruce(rotate = FALSE, frac = frac),
    plot.title   = element_text_spruce(rotate = FALSE, frac = frac)
  )
}

