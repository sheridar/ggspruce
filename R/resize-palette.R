#' Resize color palette
#'
#' Reduce or expand the number of colors in palette
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param order If `TRUE`, when colors are expanded new colors are interspersed,
#' if `FALSE`, new colors are added to the end.
#' @param ... Additional arguments to pass to `collapse_colors()`
#' @export
resize_palette <- function(colors, n, order = TRUE, ...) {

  n_clrs <- length(colors)

  if (n_clrs == n) return(colors)

  if (n > n_clrs) {
    res <- interpolate_colors(colors, n, ...)

  } else {
    res <- collapse_colors(colors, n, ...)
  }

  res
}

#' Interpolate colors
#'
#' Insert additional colors into palette, this behaves similar to
#' `grDevices::colorRampPalette()`, but keeps all of the original colors in the
#' final palette.
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param order If `TRUE` colors are ordered with new colors interspersed,
#' if `FALSE` new colors are added to the end.
#' @param ... Additional arguments to pass to `grDevices::colorRamp()`
#' @export
interpolate_colors <- function(colors, n, order = TRUE, ...) {

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

  if (!order) {
    res <- c(res[res %in% colors], res[!res %in% colors])
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
#' @param exact When `FALSE` the most distinct colors will be determined using
#' a simulated annealing approach.
#' This provides an approximate solution but is quick and generally performs
#' well.
#' When `TRUE` all possible solutions will be checked to identify
#' an exact solution. This will be slow when there is a large number of possible
#' solutions.
#' If `NULL` the approximate approach will be used except when the number of
#' possible solutions is relatively small.
#' @param ... Additional parameters to control the simulated annealing
#' algorithm implemented with `GenSA::GenSA()`.
#' @export
collapse_colors <- function(colors, n, filter = NULL, exact = NULL, ...) {

  if (n >= length(colors)) return(colors)

  # Always also test colors without a filter
  filter <- filter %||% "none"
  filter <- unique(c("none", filter))

  # Calculate pairwise differences
  dst <- .compare_clrs(colors, filt = filter)

  # Use exhaustive approach for smaller number of possible combinations
  max_combns <- 1e5
  n_combns   <- choose(length(colors), n)
  exact      <- exact %||% (n_combns < max_combns)

  if (exact) {
    if (n_combns >= max_combns) {
      cli::cli_alert_warning(
        "There is a large number of possible solutions ({n_combns}),
       set `exact` to `FALSE` to return an approximate solution."
      )
    }

    combns <- combn(seq_along(colors), m = n, simplify = FALSE)

    res <- purrr::map_dbl(combns, ~ {
      .get_min_dist(dst, .x, comparison = "idx_vs_idx")
    })

    res <- combns[[which.max(res)]]
    res <- colors[res]

    return(res)
  }

  # Using GenSA
  .obj_fn <- function(values, dist_lst) {
    values <- floor(values)

    if (any(duplicated(values))) {
      min_diff <- 0

    } else {
      min_diff <- .get_min_dist(
        dist_lst   = dist_lst,
        clr_idx    = values,
        comparison = "idx_vs_idx"
      )
    }

    -min_diff
  }

  init_vals <- as.numeric(seq_len(n))
  lower     <- as.numeric(rep(1, n))
  upper     <- as.numeric(rep(length(colors), n))

  gensa_params       <- list(...)
  gensa_params$maxit <- gensa_params$maxit %||% 1000
  gensa_params$seed  <- gensa_params$seed %||% 42

  gensa_res <- GenSA::GenSA(
    par      = as.numeric(init_vals),
    fn       = .obj_fn,
    lower    = lower,
    upper    = upper,
    control  = gensa_params,
    dist_lst = dst
  )

  res <- colors[sort(floor(gensa_res$par))]

  res
}

#' Create nested color palette
#'
#' @param colors Vector of starting colors
#' @param n_expand Vector the same length as `colors` indicating the number
#' of expanded colors to generate for each starting color.
#' Alternatively, a list of vectors the same length as `colors` can be provided.
#' Each vector should contain names to assign to the expanded colors.
#' @param keep_original If `TRUE` original colors will be included in final
#' palette
#' @param ... Additional arguments to pass to `spruce_up_colors()`.
#' @export
expand_colors <- function(colors, n_expand, keep_original = FALSE, ...) {

  # Expand colors based on n_expand
  nms <- NULL

  if (is.list(n_expand)) {
    nms <- c(n_expand, recursive = TRUE)

    n_expand <- purrr::map_dbl(n_expand, length)
  }

  clrs <- purrr::map2(colors, n_expand, rep)
  clrs <- c(clrs, recursive = TRUE)

  idx <- purrr::map2(seq_along(colors), n_expand, rep)
  idx <- c(idx, recursive = TRUE)

  # Set index to keep original colors
  adj_clrs <- NULL

  if (keep_original) {
    adj_clrs <- seq_along(idx)[duplicated(idx)]
  }

  # Adjust lightness for expanded colors
  spr_args <- list(colors = clrs, ...)

  spr_args$adjust        <- spr_args$adjust %||% "lightness"
  spr_args$adjust_colors <- spr_args$adjust_colors %||% adj_clrs

  new_clrs <- .lift(spruce_up_colors)(spr_args)

  # Keep in original order with new colors sorted by lightness
  l <- farver::decode_colour(new_clrs, to = "lab")

  res <- data.frame(
    color = new_clrs,
    idx   = idx,
    l     = l[, "l"]
  )

  res <- res[order(res$idx, res$l), ]

  res <- setNames(res$color, nms)

  res
}

#' Assign colors to provided names
#'
#' The color palette will be resized to accommodate the number of provided
#' names.
#'
#' @param colors Vector of colors
#' @param names Vector of names
#' @param select_best Select the most distinct colors when collapsing the
#' palette.
#' If `FALSE` colors will be selected starting with the first.
#' @param order If `TRUE`, when colors are expanded new colors are interspersed.
#' If `FALSE` new colors are added to the end.
#' @param ... Additional arguments to pass to `resize_palette()`
#' @export
assign_colors <- function(colors, names, select_best = TRUE,
                          order = TRUE, ...) {

  n <- length(names)

  if (!select_best && length(colors) > n) {
    colors <- colors[seq_len(n)]
  }

  clrs <- resize_palette(colors, n, order = order, ...)

  res  <- purrr::set_names(clrs, names)

  res
}
