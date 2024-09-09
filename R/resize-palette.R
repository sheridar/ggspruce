#' Resize color palette
#'
#' Reduce or expand the number of colors in palette
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param order If `TRUE`, when colors are expanded new colors are interspersed,
#' if `FALSE`, new colors are added to the end.
#' @param ... Additional arguments to pass to `collapse_colors()` or
#' `ramp_colors()`
#' @export
resize_palette <- function(colors, n, order = TRUE, ...) {

  n_clrs <- length(colors)

  if (n_clrs == n) return(colors)

  if (n > n_clrs) {
    res <- ramp_colors(colors, n, ...)

  } else {
    res <- collapse_colors(colors, n, ...)
  }

  res
}

#' Interpolate colors
#'
#' Insert additional colors into palette, this behaves similar to
#' `grDevices::colorRampPalette()`, but will keep the original colors in the
#' final palette.
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param keep_original If `TRUE` original colors will be included in final
#' palette
#' @param order If `TRUE` colors are ordered with new colors interspersed,
#' if `FALSE` new colors are added to the end.
#' @param method Method to use when interpolating colors, can be "linear" or
#' "spline".
#' @export
ramp_colors <- function(colors, n, keep_original = TRUE, order = TRUE,
                        method = "linear") {

  property = c("red", "green", "blue")

  # Calculate x for interpolation while preserving original colors
  # * for interpolation x is the position of the color in the palette and y
  #   is the r, b, or g value for each color
  # * need to keep x from original color palette so these colors will be
  #   present in the new color palette.
  clrs_n <- length(colors)

  if (n <= clrs_n) return(colors)

  x     <- seq.int(0, 1, length.out = clrs_n)
  new_x <- seq.int(0, 1, length.out = n)

  if (keep_original) {
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
  }

  # Generate functions to interpolate values
  method <- switch(
    method,
    linear = stats::approxfun,
    spline = stats::splinefun
  )

  if (clrs_n == 1) {
    dec_clrs <- dec_clrs[c(1, 1), ]
    clrs_n   <- 2
  }

  dec_clrs <- get_property(colors, property)
  property <- purrr::set_names(property)

  palette <- purrr::map(property, ~ method(x, dec_clrs[, .x][[1]]))

  # Generate new color values using x values
  new_vals <- purrr::map(palette, ~ .x(new_x))

  # Generate new colors
  # * use last color once
  res <- new_x[-length(new_x)]
  res <- split(res, cut(res, x, include.lowest = TRUE))
  res <- purrr::map_dbl(res, length)
  res <- unname(c(res, 1))
  res <- purrr::map2(colors, res, ~ rep(.x, .y))
  res <- c(res, recursive = TRUE)

  for (prop in property) {
    prop_params <- SPACE_PARAMS[[prop]]

    dec <- farver::decode_colour(res, to = prop_params[[1]])

    dec[, prop_params[[2]]] <- new_vals[[prop]]

    res <- farver::encode_colour(dec, from = prop_params[[1]])
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
#' @param n The number of expanded colors to generated for each starting colors,
#' can be one of the following:
#' - A single integer
#' - Vector the same length as `colors` indicating the number
#'   of expanded colors to generate for each starting color
#' - A list of vectors the same length as `colors`, with each vector containing
#'   names to assign to the expanded colors.
#' @param keep_original If `TRUE` original colors will be included in final
#' palette
#' @param property Color property to adjust when expanding colors.
#' @param direction Direction to use for expanding colors when `keep_original`
#' is `TRUE`, can be `1` or `-1` to increase or decrease values of `property`.
#' If `NULL` this will be automatically selected based on the starting colors.
#' @param range A vector containing the minimum and maximum values to use when
#' adjusting colors.
#' @param ... Additional arguments to pass to `spruce_up_colors()`.
#' @export
expand_colors <- function(colors, n, keep_original = FALSE,
                          property = "lightness", direction = NULL,
                          range = NULL, ...) {

  if (length(property) > 1) {
    cli::cli_abort("`property` must be length 1.")
  }

  range <- range %||% SPACE_PARAMS[[property]][[3]]

  # Expand colors based on n
  nms <- NULL

  if (is.list(n)) {
    nms <- c(n, recursive = TRUE)

    n <- purrr::map_dbl(n, length)

  } else if (length(n) == 1) {
    n <- rep(n, length(colors))
  }

  if (length(n) != length(colors)) {
    cli::cli_abort(
      "`n` must be a single integer or
       a vector or list the same length as `colors`."
    )
  }

  clrs <- purrr::map2(colors, n, rep)
  clrs <- c(clrs, recursive = TRUE)

  idx <- purrr::map2(seq_along(colors), n, rep)
  idx <- c(idx, recursive = TRUE)

  # Set adjustment range for each color
  # * set index to keep original colors
  adj_clrs <- NULL

  if (keep_original) {
    adj_clrs <- seq_along(idx)[duplicated(idx)]

    if (is.null(direction)) {
      direction <- vector("list", length(clrs))

    } else if (length(direction) == 1) {
      direction <- rep(direction, length(clrs))

    } else if (length(direction) != length(clrs)) {
      cli::cli_abort(
        "`direction` must be a single value or
         a vector the same length as `colors`."
      )
    }

    vals <- get_property(clrs, property)
    vals <- vals[[property]]

    vals <- purrr::map2(vals, direction, .set_range, rng = range)

    # Return separate vector for lower and upper bounds
    range <- list(
      purrr::map_dbl(vals, ~ .x[1]),
      purrr::map_dbl(vals, ~ .x[2])
    )
  }

  # Adjust lightness for expanded colors
  spr_args <- list(colors = clrs, range = range, ...)

  spr_args$property      <- property
  spr_args$adjust_colors <- spr_args$adjust_colors %||% adj_clrs

  new_clrs <- .lift(spruce_up_colors)(spr_args)

  # Keep in original order with new colors sorted by lightness
  res <- get_property(new_clrs, property = property)
  res <- tibble::add_column(res, idx = idx)
  res <- res[order(res$idx, res[[property]]), ]
  res <- setNames(res$color, nms)

  res
}

.set_range <- function(val, direction = NULL, rng) {

  if (is.null(direction)) {
    med <- floor(median(rng))

    direction <- ifelse(val <= med, 1, -1)
  }

  if (!direction %in% c(1, -1)) {
    cli::cli_abort("`direction` must be either 1 or -1.")
  }

  if (direction == 1) {
    res <- c(val, rng[2])

  } else {
    res <- c(rng[1], val)
  }

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
