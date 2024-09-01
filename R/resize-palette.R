#' Expand color palette
#'
#' Add additional colors to color palette
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @export
expand_colors <- function(colors, n, ...) {

  # Calculate x for interpolation while preserving original colors
  # * for interpolation x is the position of the color in the palette and y
  #   is the r, b, or g value for each color
  # * need to keep x from original color palette so these colors will be
  #   present in the new color palette.
  clrs_n <- length(colors)

  if (n <= clrs_n) return(colors)

  x     <- seq.int(0, 1, length.out = clrs_n)
  new_x <- seq.int(0, 1, length.out = n)
  ovlp  <- new_x[new_x %in% x]
  new_x <- new_x[!new_x %in% ovlp]
  dif_n <- length(new_x) - sum(!x %in% ovlp)

  # Calculate difference between original and new x values
  # * for the new colors select the ones that differ most
  difs <- new_x %>%
    map_dbl(~ min(abs(.x - x)))

  names(difs) <- seq_along(new_x)

  difs <- sort(difs)
  difs <- as.numeric(names(difs))

  new_x <- new_x[tail(difs, dif_n)]
  new_x <- sort(c(x, new_x))

  # Get colorRamp function
  # * this creates a function that can interpolate new colors using the original
  #   palette
  ramp <- grDevices::colorRamp(colors, ...)

  # Generate expanded color palette using the x values
  new_clrs <- ramp(new_x)

  # Convert new colors to hex codes
  if (ncol(new_clrs) == 4L) {
    res <- rgb(
      new_clrs[, 1L], new_clrs[, 2L],
      new_clrs[, 3L], new_clrs[, 4L],
      maxColorValue = 255
    )

  } else {
    res <- rgb(
      new_clrs[, 1L], new_clrs[, 2L], new_clrs[, 3L],
      maxColorValue = 255
    )
  }

  res
}

#' Collapse color palette
#'
#' Reduce the number of colors in palette by selecting the colors that
#' are most distinct
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include, "none", "deutan", "protan", and "tritan".
#' @export
collapse_colors <- function(colors, n, filter = NULL) {

  if (n >= length(colors)) return(colors)

  # Always also test colors without a filter
  filter <- filter %||% "none"
  filter <- unique(c("none", filter))

  .get_dist <- function(clrs) {
    dst <- .compare_clrs(clrs, filt = filter)
    dst <- .get_min_dist(dst)

    dst
  }

  clrs <- combn(colors, m = n, simplify = FALSE)
  dst  <- purrr::map_dbl(clrs, .get_dist)

  res <- clrs[[which.max(dst)]]

  res
}

#' Resize color palette
#'
#' Reduce or expand the number of colors in palette
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include, "none", "deutan", "protan", and "tritan".
#' @param ... Additional arguments to pass to `grDevices::colorRamp()`
#' @export
resize_palette <- function(colors, n, filter = NULL, ...) {

  n_clrs <- length(colors)

  if (n_clrs == n) return(colors)

  if (n > n_clrs) {
    res <- expand_colors(colors, n, ...)

  } else {
    res <- collapse_colors(colors, n, filter = filter)
  }

  res
}

#' Assign colors to provided names
#'
#' Reduce or expand color palette and assign names
#'
#' @param colors Vector of colors
#' @param names Vector of names
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include, "none", "deutan", "protan", and "tritan".
#' @param ... Additional arguments to pass to `grDevices::colorRamp()`
#' @export
assign_colors <- function(colors, names, filter = NULL, ...) {

  n <- length(names)

  clrs <- resize_palette(colors, n, filter = filter, ...)
  res  <- purrr::set_names(clrs, names)

  res
}
