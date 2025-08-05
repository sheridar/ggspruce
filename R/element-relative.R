
#' Relative text element
#'
#' @param family Font family
#' @param face Font face ("plain", "italic", "bold", "bold.italic")
#' @param colour Font colour.
#' @param size Relative font size for a 7"x5" plot area.
#'   Actual font size will be adjusted to appear consistent regardless of plot
#'   dimensions.
#' @param hjust Horizontal justification (in \eqn{[0, 1]})
#' @param vjust Vertical justification (in \eqn{[0, 1]})
#' @param angle Angle (in \eqn{[0, 360]})
#' @param lineheight Line height
#' @param margin Margins around the text. See [margin()] for more
#'   details. When creating a theme, the margins should be placed on the
#'   side of the text facing towards the center of the plot.
#' @param debug If `TRUE`, aids visual debugging by drawing a solid
#'   rectangle behind the complete text area, and a point where each label
#'   is anchored.
#' @export
element_text_relative <- function(family = NULL, face = NULL, colour = NULL,
                                  size = NULL, hjust = NULL, vjust = NULL, angle = NULL,
                                  lineheight = NULL, color = NULL, margin = NULL,
                                  debug = NULL, inherit.blank = FALSE
                                 ) {

  if (!is.null(color))  colour <- color

  n <- max(
    length(family), length(face), length(colour), length(size),
    length(hjust), length(vjust), length(angle), length(lineheight)
  )

  if (n > 1) {
    cli::cli_warn(c(
      "Vectorized input to {.fn element_text} is not officially supported.",
      "i" = "Results may be unexpected or may change in future versions of ggplot2."
    ))
  }

  structure(
    list(
      family = family, face = face, colour = colour,
      size = size, hjust = hjust, vjust = vjust,
      angle = angle, lineheight = lineheight, margin = margin,
      debug = debug, inherit.blank = inherit.blank
    ),
    class = c("element_text_relative", "element_text", "element")
  )
}

#' Create element_text_relative Grob
#'
#' @importFrom grid gpar
#' @method element_grob element_text_relative
#' @export
element_grob.element_text_relative <- function(element, label = "", x = NULL,
                                               y = NULL, family = NULL, face = NULL,
                                               colour = NULL, size = NULL, hjust = NULL,
                                               vjust = NULL, angle = NULL, lineheight = NULL,
                                               margin = NULL, margin_x = FALSE,
                                               margin_y = FALSE, ...
                                              ) {

  if (is.null(label)) return(zeroGrob())

  vj     <- vjust  %||% element$vjust
  hj     <- hjust  %||% element$hjust
  margin <- margin %||% element$margin
  angle  <- angle  %||% element$angle %||% 0

  # The gp settings can override element_gp
  gp <- grid::gpar(
    fontsize   = size,
    col        = colour,
    fontfamily = family,
    fontface   = face,
    lineheight = lineheight
  )

  element_gp <- grid::gpar(
    fontsize   = element$size,
    col        = element$colour,
    fontfamily = element$family,
    fontface   = element$face,
    lineheight = element$lineheight
  )

  element_gp$fontsize <- calc_rel_size(element$size)

  # Create final grob
  titleGrob(
    label, x, y,
    gp       = modify_list(element_gp, gp),
    hjust    = hj,
    vjust    = vj,
    angle    = angle,
    margin   = margin,
    margin_x = margin_x,
    margin_y = margin_y,
    debug    = element$debug,
    ...
  )
}

calc_rel_size <- function(ref_size, ref_width = 7, ref_height = 7) {  
  win_size    <- dev.size("in")
  size_factor <- ref_size / sqrt(ref_width * ref_height)
  new_size    <- sqrt(prod(win_size)) * size_factor
}
