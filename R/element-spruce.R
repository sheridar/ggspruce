
#' Spruce up text element
#'
#' @param family Font family
#' @param face Font face ("plain", "italic", "bold", "bold.italic")
#' @param colour,color Text colour. Color is an alias for colour.
#' @param size Text size in pts.
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
#' @param inherit.blank Should this element inherit the existence of an
#'   element_blank among its parents?
#'   If TRUE the existence of a blank element among its parents will cause this
#'   element to be blank as well.
#'   If FALSE any blank parent element will be ignored when calculating final
#'   element state.
#' @param property Vector of text properties to adjust (angle, size, hjust, vjust).
#'   Properties will be adjusted in the order they are given, e.g. `c("angle", "size")`.
#' @param range A vector containing the minimum and maximum values to use when
#'   adjusting text.
#'   If multiple properties will be adjusted, provide a named list with
#'   ranges for each property, e.g. `list(angle = c(0, 90), size = c(6, 12))`.
#'   If ranges are not provided, they will be set automatically based on the starting
#'   values.
#' @export
element_text_spruce <- function(family = NULL, face = NULL, colour = NULL,
                                size = NULL, hjust = NULL, vjust = NULL, angle = NULL,
                                lineheight = NULL, color = NULL, margin = NULL,
                                debug = NULL, inherit.blank = FALSE,
                                property = c("angle", "size"),
                                padding = grid::unit(10, "pt"), overhang = TRUE, 
                                range = list(), fixed = TRUE
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
      debug = debug, inherit.blank = inherit.blank,
      property = property, padding = padding, overhang = overhang,
      range = range, fixed = fixed
    ),
    class = c("element_text_spruce", "element_text", "element")
  )
}
