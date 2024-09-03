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
  difs <- purrr::map_dbl(new_x, ~ min(abs(.x - x)))

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
    res <- grDevices::rgb(
      new_clrs[, 1L], new_clrs[, 2L],
      new_clrs[, 3L], new_clrs[, 4L],
      maxColorValue = 255
    )

  } else {
    res <- grDevices::rgb(
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

  # Use exhaustive approach for smaller number of possible combinations
  dst <- .compare_clrs(colors, filt = filter)

  combns <- combn(seq_along(colors), m = n, simplify = FALSE)

  exact <- length(combns) < 1e5

  if (exact) {
    res <- purrr::map_dbl(combns, ~ {
      .get_min_dist(dst, .x, comparison = "idx_vs_idx")
    })

    res <- combns[[which.max(res)]]
    res <- colors[res]

    return(res)
  }

  # Use quick approximate approach for larger number of possible combinations
  # get most distinct starting color
  .get_most_distinct <- function(dist) {
    res <- purrr::map_dbl(dist, ~ {
      if (all(is.na(.x))) return(NA)

      min(.x, na.rm = TRUE)
    })

    res
  }

  min_diff <- purrr::map_dfr(dst, ~ {
    .x[!upper.tri(.x)] <- NA

    .get_most_distinct(as.data.frame(.x))
  })

  min_diff <- .get_most_distinct(min_diff)
  min_diff <- which.max(min_diff)

  # Approximate best combination of colors
  clrs <- farver::decode_colour(colors, to = "lab")

  new_clrs      <- matrix(nrow = n, ncol = 3)
  new_clrs[1, ] <- clrs[min_diff, ,  drop = FALSE]
  clrs          <- clrs[-min_diff, , drop = FALSE]

  if (n > 1) {
    for (i in 2:n) {
      clrs_dec     <- farver::encode_colour(clrs, from = "lab")
      new_clrs_dec <- farver::encode_colour(new_clrs, from = "lab")

      new_clrs_dec <- stats::na.omit(new_clrs_dec)

      # .compare_clrs returns a list of distance matrices, one for each filter
      dst <- .compare_clrs(new_clrs_dec, clrs_dec, filt = filter)

      min_diff <- purrr::map_dfr(dst, ~ purrr::map_dbl(as.data.frame(.x), min))
      min_diff <- purrr::map_dbl(min_diff, min)

      min_diff <- which.max(min_diff)

      new_clrs[i, ] <- clrs[min_diff, ,  drop = FALSE]
      clrs          <- clrs[-min_diff, , drop = FALSE]
    }
  }

  # Return selected colors in same order as input colors
  res <- farver::encode_colour(new_clrs, from = "lab")
  res <- res[na.omit(match(colors, res))]

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
