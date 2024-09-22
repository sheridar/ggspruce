#' Resize color palette
#'
#' Reduce or expand the number of colors in palette, colors are removed with
#' `collapse_colors()` and added with `interp_colors()`.
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param order If `TRUE`, when colors are expanded new colors are interspersed,
#' if `FALSE`, new colors are added to the end.
#' @param ... Additional arguments to pass to `collapse_colors()`.
#' @export
resize_colors <- function(colors, n, order = TRUE, ...) {

  n_clrs <- length(colors)

  if (n_clrs == n) return(colors)

  if (n > n_clrs) {
    res <- interp_colors(colors, n)

  } else {
    res <- collapse_colors(colors, n, ...)
  }

  res
}

#' Interpolate colors
#'
#' Insert additional colors into palette, this behaves similar to
#' `scales::colour_ramp()`, but keeps all of the original colors in the
#' final palette.
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param keep_original If `TRUE` original colors will be included in final
#' palette
#' @param order If `TRUE` colors are ordered with new colors interspersed,
#' if `FALSE` new colors are added to the end.
#' @param ... Additional arguments to pass to `scales::colour_ramp()`
#' @export
interp_colors <- function(colors, n, keep_original = TRUE, order = TRUE, ...) {

  # Calculate x for interpolation while preserving original colors
  # * for interpolation x is the position of the color in the palette and y
  #   is the l, a, or b value for each color
  # * need to keep x from original color palette so these colors will be
  #   present in the new color palette.
  clrs_n <- length(colors)

  if (n <= clrs_n) return(colors)

  if (clrs_n < 2) cli::cli_abort("Must provide at least two colors.")

  x     <- seq.int(0, 1, length.out = clrs_n)
  new_x <- seq.int(0, 1, length.out = n)

  if (keep_original) {
    new_n <- (n - clrs_n) + 2

    new_idx <- seq(1, n, length.out = new_n)
    new_idx <- floor(new_idx[-c(1, new_n)] + 0.5)  # want 0.5 to always round up

    x <- new_x[-new_idx]
  }

  # Generate ramp function
  # * this creates a function that can interpolate new colors using the original
  #   palette
  # * modified version of scales::colour_ramp() that accepts input values for x
  # * this allows new colors to be interspersed evenly between the original
  #   colors
  ramp <- .get_ramp_fn(colors, x)

  # Generate expanded color palette using x values
  res <- ramp(new_x)

  if (keep_original && !order) {
    res <- c(res[res %in% colors], res[!res %in% colors])
  }

  res
}

#' Modified version of scales::colour_ramp() that accepts input values for `x`
#' This allows new colors to be interspersed evenly between the original colors
.get_ramp_fn <- function(colors, x = NULL, na.color = NA, alpha = TRUE) {

  if (length(colors) == 0) {
    cli::cli_abort("Must provide at least one colour to create a colour ramp")
  }

  if (length(colors) == 1) {
    return(
      structure(
        function(x) {
          ifelse(is.na(x), na.color, colors)
        },
        safe_palette_func = TRUE
      )
    )
  }

  colors <- tolower(colors)

  lab_in <- farver::decode_colour(
    colour   = colors,
    alpha    = TRUE,
    to       = "lab",
    na_value = "transparent"
  )

  # Set x values for interpolation
  x_in <- seq(0, 1, length.out = length(colors))

  if (!is.null(x)) {
    if (length(x) != length(colors)) {
      cli::cli_abort("`x` must be the same length as `colors`")
    }

    x_in <- x
  }

  l_interp <- stats::approxfun(x_in, lab_in[, 1])
  u_interp <- stats::approxfun(x_in, lab_in[, 2])
  v_interp <- stats::approxfun(x_in, lab_in[, 3])

  if (!alpha || all(lab_in[, 4] == 1)) {
    alpha_interp <- function(x) NULL

  } else {
    alpha_interp <- stats::approxfun(x_in, lab_in[, 4])
  }

  structure(
    function(x) {
      lab_out <- cbind(l_interp(x), u_interp(x), v_interp(x))

      out <- farver::encode_colour(
        lab_out,
        alpha = alpha_interp(x),
        from = "lab"
      )

      out[is.na(out)] <- na.color
      out
    },
    safe_palette_func = TRUE
  )
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
#' Colors will be compared before and after applying the filter(s).
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include,
#' - "colorblind", use deutan, protan, and tritan color blindness simulation
#'   filters
#' - "deutan"
#' - "protan"
#' - "tritan"
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

  .chk_spruce_args(colors = colors, n = n, exact = exact)

  if (n >= length(colors)) return(colors)

  filter <- .chk_filt_args(filter, multi = TRUE)

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

    combns <- utils::combn(seq_along(colors), m = n, simplify = FALSE)

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
#' @param n The number of expanded colors to generate for each starting color,
#' can be one of the following:
#' - A single integer
#' - A vector the same length as `colors` indicating the number
#'   of expanded colors to generate for each starting color
#' @param names A list of vectors the same length as `colors`,
#' with each vector containing names to assign to the expanded colors.
#' Each original color is expanded based on the number of provided names.
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
expand_colors <- function(colors, n = NULL, names = NULL, keep_original = FALSE,
                          property = "lightness", direction = NULL,
                          range = NULL, ...) {

  .chk_spruce_args(
    colors = colors,
    keep_original = keep_original,
    range = range
  )

  property <- .chk_prop_args(property, multi = FALSE)

  if (is.null(n) && is.null(names)) return(colors)

  range <- range %||% SPACE_PARAMS[[property]][[3]]

  # Expand colors based on n
  nms <- NULL

  if (!is.null(names)) {
    if (length(names) != length(colors)) {
      cli::cli_abort("`names` must be a list the same length as `colors`.")
    }

    nms <- c(names, recursive = TRUE)

    n <- purrr::map_dbl(names, length)

  } else if (length(n) == 1) {
    n <- rep(n, length(colors))
  }

  if (length(n) != length(colors)) {
    cli::cli_abort(
      "`n` must be a single integer or a vector the same length as `colors`."
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
  res <- purrr::set_names(res$color, nms)

  res
}

.set_range <- function(val, direction = NULL, rng) {

  if (is.null(direction)) {
    med <- floor(stats::median(rng))

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
#' @param ... Additional arguments to pass to `resize_colors()`
#' @export
assign_colors <- function(colors, names, select_best = TRUE,
                          order = TRUE, ...) {

  .chk_spruce_args(colors = colors)

  purrr::walk(list(select_best, order), ~ {
    if (!rlang::is_bare_logical(.x) || !rlang::has_length(.x, 1)) {
      cli::cli_abort("`select_best` and `order` must be `TRUE` or `FALSE`.")
    }
  })

  if (!rlang::is_bare_character(names)) {
    cli::cli_abort("`names` must be a character vector.")
  }

  n <- length(names)

  if (!select_best && length(colors) > n) {
    colors <- colors[seq_len(n)]
  }

  clrs <- resize_colors(colors, n, order = order, ...)

  res  <- purrr::set_names(clrs, names)

  res
}

#' Sort colors based on property
#'
#' @param colors Vector of colors
#' @param property Color properties to use for sorting
#' @param desc Sort in descending order
#' @export
sort_colors <- function(colors, property = "hue", desc = FALSE) {

  .chk_spruce_args(colors = colors)

  property <- .chk_prop_args(property, multi = TRUE)

  prop <- get_property(colors, property)
  vals <- as.list(prop[property])
  idx  <- .lift(order, decreasing = desc)(vals)
  res  <- prop$color[idx]

  res
}
