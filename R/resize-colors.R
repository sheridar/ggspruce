#' Resize color palette
#'
#' Remove or add colors to the palette, colors are removed with
#' `collapse_colors()` and added with `interp_colors()`.
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param difference Color difference threshold to use for
#' selecting distinct `colors`.
#' @param method Method to use for comparing colors, can be one of:
#' - "euclidian"
#' - "CIE1976"
#' - "CIE94"
#' - "CIE2000"
#' - "CMC"
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include,
#' - "colorblind", use deutan, protan, and tritan color blindness simulation
#'   filters
#' - "deutan"
#' - "protan"
#' - "tritan"
#' @param order If `TRUE` colors are ordered with new colors interspersed,
#' if `FALSE` new colors are added to the end.
#' @param maxit Maximum number of iterations to use when optimizing the color
#' palette.
#' Higher values will result in more optimal adjustments and a reduction in
#' speed.
#' @param ... Additional arguments to pass to `collapse_colors()` or
#' `spruce_colors()`.
#' @export
resize_colors <- function(colors, n, difference = 15, method = "CIE2000",
                          filter = NULL, order = TRUE, maxit = 500, ...) {

  n_clrs <- length(colors)

  if (n_clrs == n) return(colors)

  if (n > n_clrs) {
    res <- interp_colors(colors, n, order = TRUE)

    ex_idx <- match(colors, res)

    res <- spruce_colors(
      res,
      difference     = difference,
      property       = "interp",
      method         = method,
      filter         = filter,
      exclude_colors = ex_idx,
      order          = TRUE,
      maxit          = maxit,
      ...
    )

    if (!order) res <- c(colors, res[!res %in% colors])

  } else {
    res <- collapse_colors(
      colors, n,
      difference = difference,
      method     = method,
      filter     = filter,
      maxit      = maxit,
      ...
    )
  }

  res
}

#' Interpolate colors
#'
#' Insert additional colors into palette, this behaves similar to
#' `scales::colour_ramp()`, but keeps the original colors in the
#' final palette.
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param keep_original If `TRUE` original colors will be included in final
#' palette
#' @param order If `TRUE` colors are ordered with new colors interspersed,
#' if `FALSE` new colors are added to the end.
#' @param before,after Numeric vector indicating the position in the original
#' palette where new colors should be inserted.
#' A position must be provided for each new color added.
#' The default is to evenly distribute new colors throughout the original
#' palette.
#' @export
interp_colors <- function(colors, n, keep_original = TRUE, order = TRUE,
                          before = NULL, after = NULL) {

  if (!rlang::is_null(before) || !rlang::is_null(after)) {
    keep_original <- TRUE

    if (sum(length(before), length(after)) != (n - length(colors))) {
      cli::cli_abort(
        "When `before` and/or `after` are provided, a value must be specified
         for each new color, i.e. the combined length of `before` and `after`
         must be equal to `n - length(colors)`."
      )
    }
  }

  clrs_n <- length(colors)

  if (n <= clrs_n) return(colors)

  if (length(unique(colors)) < 2) {
    cli::cli_abort("Must provide at least two unique colors.")
  }

  # Calculate x for interpolation while preserving original colors
  # * for interpolation x is the position of the color in the palette and y
  #   is the l, a, or b value for each color
  # * need to keep x from original color palette so these colors will be
  #   present in the new color palette.
  x     <- seq.int(0, 1, length.out = clrs_n)
  new_x <- seq.int(0, 1, length.out = n)

  if (keep_original) {
    if (!rlang::is_null(before) || !rlang::is_null(after)) {
      new_pos <- sort(c(before - 0.5, after + 0.5))
      new_idx <- sort(c(seq_len(clrs_n), new_pos))

      new_idx <- purrr::map(new_pos, ~ which(new_idx == .x))
      new_idx <- unique(c(new_idx, recursive = TRUE))

    } else {
      new_n   <- (n - clrs_n) + 2
      new_idx <- seq(1, n, length.out = new_n)
      new_idx <- floor(new_idx[-c(1, new_n)] + 0.5)  # want 0.5 to always round up
    }

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

#' Collapse color palette
#'
#' Reduce the number of colors in palette by selecting the colors that
#' are most distinct
#'
#' @param colors Vector of colors
#' @param n Number of colors to include in final color palette
#' @param difference Color difference threshold to use for
#' selecting distinct `colors`.
#' @param method Method to use for comparing colors, can be one of:
#' - "euclidian"
#' - "CIE1976"
#' - "CIE94"
#' - "CIE2000"
#' - "CMC"
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
#' @param maxit Maximum number of iterations to use when optimizing the color
#' palette.
#' Higher values will result in more optimal adjustments and a reduction in
#' speed.
#' @param ... Additional parameters to control the simulated annealing
#' algorithm implemented with `GenSA::GenSA()`.
#' @export
collapse_colors <- function(colors, n, difference = 15, method = "CIE2000",
                            filter = NULL, exact = NULL, maxit = 500, ...) {

  # Check arguments
  .chk_spruce_args(colors = colors, n = n, exact = exact)

  if (n >= length(colors)) return(colors)

  if (n == 1) cli::cli_abort("`n` must be greater than 1.")

  filter <- .chk_filt_args(filter, multi = TRUE)

  # Check for duplicated colors
  # * identify best duplicated colors to add
  uniq_clrs  <- unique(colors)
  uniq_n     <- min(length(uniq_clrs), n)
  needs_dups <- n > uniq_n

  if (length(uniq_clrs) == 1) {
    cli::cli_abort("Must provide at least two unique colors.")
  }

  # Calculate pairwise differences
  dst <- .compare_clrs(uniq_clrs, filt = filter, method = method)

  # Use exhaustive approach for smaller number of possible combinations
  max_combns <- 1e5
  n_combns   <- choose(length(uniq_clrs), uniq_n)
  exact      <- exact %||% (n_combns < max_combns)

  if (exact) {
    if (n_combns >= max_combns) {
      cli::cli_alert_warning(
        "There is a large number of possible solutions ({n_combns}),
         set `exact` to `FALSE` to return an approximate solution."
      )
    }

    combns <- utils::combn(seq_along(uniq_clrs), m = uniq_n, simplify = FALSE)

    res <- purrr::map_dbl(combns, ~ {
      .get_min_dist(dst, .x, comparison = "idx_vs_idx")
    })

    res <- combns[[which.max(res)]]
    res <- uniq_clrs[res]

  # Using GenSA
  } else {
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

    init_vals <- as.numeric(seq_len(uniq_n))
    lower     <- as.numeric(rep(1, uniq_n))
    upper     <- as.numeric(rep(length(uniq_clrs), uniq_n))

    gensa_params                <- list(...)
    gensa_params$maxit          <- maxit
    gensa_params$threshold.stop <- -difference
    gensa_params$seed           <- gensa_params$seed %||% 42

    gensa_res <- GenSA::GenSA(
      par      = as.numeric(init_vals),
      fn       = .obj_fn,
      lower    = lower,
      upper    = upper,
      control  = gensa_params,
      dist_lst = dst
    )

    res <- uniq_clrs[sort(floor(gensa_res$par))]
  }

  # Add duplicate colors if needed
  if (needs_dups) {
    dups <- colors[duplicated(colors)]

    dups_n <- n - uniq_n

    if (any(!uniq_clrs %in% dups)) {
      dups <- compare_colors(
        dups,
        uniq_clrs[!uniq_clrs %in% dups],
        filter = filter,
        method = method
      )

    } else {
      dups <- compare_colors(dups, filter = filter, method = method)
    }

    dups <- names(sort(dups, decreasing = TRUE))
    dups <- dups[seq_len(dups_n)]

    res <- c(res, dups)

    # Sort final colors in order of input colors
    sort_idx <- purrr::map(purrr::set_names(res), ~ {
      idx <- which(colors == .x)
      idx[seq_len(sum(res == .x))]
    })

    sort_idx <- sort_idx[!duplicated(names(sort_idx))]

    res <- vector("character")

    for (clr in names(sort_idx)) {
      res[sort_idx[[clr]]] <- clr
    }

    res <- res[!is.na(res)]
  }

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
#' @param ... Additional arguments to pass to `spruce_colors()`.
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

  range <- range %||% PROP_PARAMS[[property]][[3]]

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

  new_clrs <- .lift(spruce_colors)(spr_args)

  # Keep in original order with new colors sorted by lightness
  res <- get_property(new_clrs, property = property)
  res <- tibble::add_column(res, idx = idx)
  res <- res[order(res$idx, res[[property]]), ]
  res <- purrr::set_names(res$color, nms)

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
#' @param order If `TRUE` colors are ordered with new colors interspersed,
#' if `FALSE` new colors are added to the end.
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

#' Modified version of scales::colour_ramp() that accepts input values for `x`
#' This allows new colors to be interspersed evenly between the original colors
#' @noRd
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

#' Set ranges for expand_colors
#' @noRd
.set_range <- function(val, direction = NULL, rng) {

  if (is.null(direction)) {
    med <- floor(stats::median(rng))

    direction <- ifelse(val <= med, 1, -1)
  }

  if (!direction %in% c(1, -1)) {
    cli::cli_abort("`direction` must be either 1 or -1.")
  }

  if (direction == 1) {
    res <- c(max(val, rng[1]), rng[2])

  } else {
    res <- c(rng[1], min(val, rng[2]))
  }

  res
}
