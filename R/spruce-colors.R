#' Adjust colors based on similarity
#'
#' @param colors Character vector of colors to adjust
#' @param difference Color difference threshold (CIE200 score) to use for
#' adjusting `colors`.
#' Colors will be adjusted so the minimum pairwise difference
#' is greater than this threshold.
#' @param property Vector of color properties to adjust, can include any of:
#' - "interp", before adjusting color properties, first try interpolating new
#'   colors.
#'   This is the best approach for preserving the overall appearance of the
#'   original palette.
#' - "lightness", from LAB colorspace
#' - "a", from LAB colorspace
#' - "b", from LAB colorspace
#' - "hue", from HSL colorspace
#' - "saturation", from HSL colorspace
#' - "red", from RGB colorspace
#' - "green", from RGB colorspace
#' - "blue", from RGB colorspace
#' @param method Method to use for comparing colors, can be one of:
#' - "euclidian"
#' - "CIE1976"
#' - "CIE94"
#' - "CIE2000"
#' - "CMC"
#' @param range A vector containing the minimum and maximum values to use when
#' adjusting colors.
#' If multiple color properties will be adjusted, provide a named list with
#' ranges for each property.
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' Colors will be adjusted to minimize the pairwise difference before and after
#' applying the filter.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include,
#' - "colorblind", use deutan, protan, and tritan color blindness simulation
#'   filters
#' - "deutan"
#' - "protan"
#' - "tritan"
#' @param adjust_colors Index indicating color(s) to specifically adjust.
#' Should be an integer vector, or a character vector containing names matching
#' those provided for `colors`.
#' @param exclude_colors Index indicating color(s) to exclude when adjusting
#' palette.
#' Should be an integer vector, or a character vector containing names matching
#' those provided for `colors`.
#' @param order If `TRUE`, when `property` is "interp" colors will be ordered
#' based on the original palette.
#' If `FALSE`, adjusted colors will remain in their original positions.
#' @param maxit Maximum number of iterations to use when optimizing the color
#' palette.
#' Higher values will result in more optimal adjustments and a reduction in
#' speed.
#' @param ... Additional control parameters to pass to `GenSA::GenSA()`.
#' @export
spruce_colors <- function(colors, difference = 10,
                          property = "interp",
                          method = "CIE2000", range = NULL,
                          filter = NULL, adjust_colors = NULL,
                          exclude_colors = NULL, order = FALSE, maxit = 500,
                          ...) {

  .chk_spruce_args(
    colors         = colors,
    difference     = difference,
    method         = method,
    range          = range,
    adjust_colors  = adjust_colors,
    exclude_colors = exclude_colors,
    maxit          = maxit
  )

  # Set property arguments
  interp      <- "interp" %in% property
  property    <- .chk_prop_args(property, multi = TRUE)
  prop_params <- PROP_PARAMS[property]

  adj_prop <- length(prop_params) > 0

  # Set ranges for each property
  # * structure of range is checked earlier by .chk_spruce_args()
  # * if not named, assume length 2 and contains range for single color property
  # * if not named, apply range to first color property
  if (adj_prop && !rlang::is_null(range)) {
    if (rlang::is_list(range) && !rlang::is_null(names(range))) {
      range <- range[names(prop_params)]

    } else {
      range <- purrr::set_names(
        list(range),
        names(prop_params)[[1]]
      )
    }

    for (prop in names(range)) {
      prop_params[[prop]][[3]] <- range[[prop]]
    }
  }

  # Set color filters to test
  clr_filts <- .chk_filt_args(filter, multi = TRUE)

  # Identify initial colors to adjust
  # * for initial attempt optimization is based on comparison between the index
  #   colors and the unadjusted colors
  # * the difference between unadjusted colors is not considered by the
  #   objective function
  # * so the min_diff returned by the objective function might differ from the
  #   minimum difference calculated for the entire palette by compare_colors()
  # * this is okay since the unadjusted colors have already been identified as
  #   meeting the difference threshold
  ex_idx <- .get_clr_idx(colors, exclude_colors)

  full_idx <- seq_along(colors)
  full_idx <- full_idx[!full_idx %in% ex_idx]

  clr_idx <- compare_colors(
    colors     = colors,
    filter     = clr_filts,
    method     = method,
    return_mat = TRUE
  )

  clr_idx <- which(
    clr_idx < difference & upper.tri(clr_idx),
    arr.ind = TRUE
  )

  clr_idx <- sort(unique(clr_idx[!clr_idx %in% ex_idx]))

  check_full <- is.null(adjust_colors) && any(!full_idx %in% clr_idx)

  # If adjust_colors are specified use intersection with clr_idx
  if (!is.null(adjust_colors)) {
    adj_idx <- .get_clr_idx(colors, adjust_colors)

    if (all(adj_idx %in% ex_idx)) {
      cli::cli_abort(
        "`adjust_colors` and `exclude_colors` must specify different colors."
      )
    }

    clr_idx <- intersect(clr_idx, adj_idx)
  }

  if (length(clr_idx) == 0) return(colors)

  # SA parameters
  sa_params <- list(
    maxit = maxit,
    threshold.stop = -difference,
    ...
  )

  sa_params$seed <- sa_params$seed %||% 42

  # Interpolate new colors
  # * first only change similar colors then try changing all colors
  # * always order adjusted colors when all colors are adjusted
  res <- colors

  if (interp) {
    optim_res <- .run_gensa_interp(
      clrs         = res,
      clr_idx      = clr_idx,
      method       = method,
      filts        = clr_filts,
      order        = order || all(full_idx %in% clr_idx),
      scale        = FALSE,
      gensa_params = sa_params
    )

    res      <- optim_res[[1]]
    min_diff <- optim_res[[2]]

    if (check_full && min_diff < difference) {
      full_res <- .run_gensa_interp(
        clrs         = res,
        clr_idx      = full_idx,
        method       = method,
        filts        = clr_filts,
        order        = TRUE,
        scale        = FALSE,
        gensa_params = sa_params
      )

      full_diff <- full_res[[2]]

      if (full_diff > min_diff) {
        res      <- full_res[[1]]
        min_diff <- full_diff
      }
    }

    if (min_diff >= difference || !adj_prop) {
      .warn_diff(min_diff, difference)

      return(res)
    }
  }

  # Adjust color properties
  optim_res <- .run_gensa_prop(
    clrs         = res,
    clr_idx      = clr_idx,
    method       = method,
    prop_params  = prop_params,
    filts        = clr_filts,
    gensa_params = sa_params
  )

  res      <- optim_res[[1]]
  min_diff <- optim_res[[2]]

  # Check difference for optimized colors
  # * if difference not met, rerun and allow all colors to be adjusted
  # * do not do this when adjust_colors colors are passed
  # * use best solution from previous attempt
  if (check_full && min_diff < difference) {
    full_idx  <- seq_along(colors)
    full_idx  <- full_idx[!full_idx %in% ex_idx]

    full_res <- .run_gensa_prop(
      clrs         = res,
      clr_idx      = full_idx,  # update to adjust all colors
      method       = method,
      prop_params  = prop_params,
      filts        = clr_filts,
      gensa_params = sa_params
    )

    full_diff <- full_res[[2]]

    if (full_diff > min_diff) {
      res      <- full_res[[1]]
      min_diff <- full_diff
    }
  }

  # Return optimized colors
  .warn_diff(min_diff, difference)

  res
}

#' Optimize colors using GenSA
#'
#' @param clrs Vector of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param prop_params List of vectors containing parameters for color properties.
#' Each vector must include in this order, the colorspace, numeric index for
#' column containing the color property to adjust, range of values to adjust.
#' @param method Method for comparing colors
#' @param filts Color filters to apply when calculating color differences.
#' @param order Should the final colors be ordered based on the provided x
#' values, this orders colors in a similar way as the original colors
#' @param scale Should `values` be scaled so the minimum and maximum values are
#' 0 and 1
#' @param gensa_params Named list with control parameters to pass to
#' `GenSA::GenSA()`
#' @noRd
.run_gensa_interp <- function(clrs, clr_idx, method, filts = "none",
                              order = FALSE, scale = FALSE,
                              gensa_params = list()) {

  # Set initial values
  # * use random starting values for colors to be adjusted
  clrs_x <- seq(0, 1, length.out = length(clrs))

  init_vals <- clrs_x[clr_idx]

  optim <- GenSA::GenSA(
    par     = init_vals,
    fn      = .sa_obj_interp,
    lower   = rep(0, length(init_vals)),
    upper   = rep(1, length(init_vals)),
    control = gensa_params,

    clrs     = clrs,
    clr_idx  = clr_idx,
    method   = method,
    clr_filt = filts,
    clrs_x   = clrs_x,
    scale    = scale
  )

  # Color ramp function
  # * color ramp is set using original colors
  ramp <- .get_ramp_fn(clrs, clrs_x)

  # Get colors using adjusted x values
  clrs_x[clr_idx] <- optim$par

  if (scale) clrs_x <- (clrs_x - min(clrs_x)) / diff(range(clrs_x))

  res <- ramp(clrs_x)

  names(res) <- names(clrs)

  # Return vector of hex colors and minimum difference
  if (order) res <- res[order(clrs_x, res)]

  min_diff <- -optim$value

  list(res, min_diff)
}

.run_gensa_prop <- function(clrs, clr_idx, prop_params, method, filts = "none",
                            gensa_params = list()) {

  .gensa <- function(clrs, clr_idx, method, filts, space, val_idx, range) {
    dec_clrs  <- farver::decode_colour(clrs, to = space)
    init_vals <- dec_clrs[clr_idx, val_idx]

    range <- .chk_range_args(range, clrs, clr_idx)

    res <- GenSA::GenSA(
      par     = as.numeric(init_vals),
      fn      = .sa_obj_prop,
      lower   = range[[1]],
      upper   = range[[2]],
      control = gensa_params,

      clrs     = clrs,
      clr_idx  = clr_idx,
      method   = method,
      clr_filt = filts,
      space    = space,
      val_idx  = val_idx
    )

    res
  }

  min_diff <- 0

  for (params in prop_params) {
    optim <- .gensa(
      clrs    = clrs,
      clr_idx = clr_idx,
      method  = method,
      filts   = filts,
      space   = params[[1]],
      val_idx = params[[2]],
      range   = params[[3]]
    )

    if (-optim$value > min_diff) {
      min_diff <- -optim$value

      dec_clrs <- farver::decode_colour(clrs, to = params[[1]])

      dec_clrs[clr_idx, params[[2]]] <- optim$par

      clrs <- farver::encode_colour(dec_clrs, from = params[[1]])
    }

    if (min_diff > -gensa_params$threshold.stop) break()
  }

  # Return vector of hex colors and minimum difference
  list(clrs, min_diff)
}

#' Objective function for optimizing colors
#'
#' @param values Vector of values to adjust
#' @param clrs Vector of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param method Method for comparing colors
#' @param clr_filt Vector of color filters to apply when calculating color
#' differences
#' @param clrs_x x values to use for interpolating new colors
#' @param scale Should `values` be scaled so the minimum and maximum values are
#' 0 and 1
#' @param space Colorspace to use for decoding colors and adjusting color
#' properties, e.g. "lab" for lightness
#' @param val_idx Single numerical index indicating the column of the decoded
#' color matrix containing the values to modify, e.g. 1 for lightness
#' @noRd
.sa_obj_interp <- function(values, clrs, clr_idx, method, clr_filt = "none",
                           clrs_x, scale = FALSE) {

  if (any(duplicated(values))) return(0)

  # Format x for linear model
  # * color ramp is set using original colors
  ramp <- .get_ramp_fn(clrs, clrs_x)

  clrs_x[clr_idx] <- values

  if (scale) clrs_x <- (clrs_x - min(clrs_x)) / diff(range(clrs_x))

  clrs <- ramp(clrs_x)

  # Calculate pairwise CIEDE2000 differences
  dist_lst <- .compare_clrs(clrs, method = method, filt = clr_filt)
  min_diff <- .get_min_dist(dist_lst, clr_idx, comparison = "idx_vs_all")

  -min_diff
}

.sa_obj_prop <- function(values, clrs, clr_idx, method, clr_filt = "none",
                         space = "lab", val_idx = 1) {

  clrs <- farver::decode_colour(clrs, to = space)

  clrs[clr_idx, val_idx] <- values

  clrs <- farver::encode_colour(clrs, from = space)

  # Calculate pairwise CIEDE2000 differences
  dist_lst <- .compare_clrs(clrs, method = method, filt = clr_filt)
  min_diff <- .get_min_dist(dist_lst, clr_idx, comparison = "idx_vs_all")

  -min_diff
}

#' Determine minimum color difference from list of distance matrices
#'
#' @param dist_lst List of distance matrices
#' @param clr_idx Numeric index indicating which colors to use when identifying
#' minimum color difference for distance matrix
#' @param comparison How should colors provided in `clr_idx` be compared when
#' identifying minimum color differences, one of:
#' - "all_vs_all", select minimum distance using all comparisons.
#' - "idx_vs_all", select minimum distance for colors in `clr_idx` when compared
#'   to all other colors
#' - "idx_vs_idx", select minimum distance for colors in `clr_idx` when compared
#'   to all other colors in `clr_idx`
#' @param only_upper_tri Only consider the upper triangle when calculating the
#' minimum difference
#' @noRd
.get_min_dist <- function(dist_lst, clr_idx = NULL, comparison = "idx_vs_all",
                          only_upper_tri = TRUE) {

  tri_fn <- upper.tri

  if (!only_upper_tri) {
    tri_fn <- function(x) rep(TRUE, length(x))
  }

  if (is.null(clr_idx) || identical(comparison, "all_vs_all")) {
    res <- purrr::map_dbl(dist_lst, ~ min(.x[tri_fn(.x)]))

  } else if (identical(comparison, "idx_vs_all")) {
    res <- purrr::map_dbl(dist_lst, ~ {
      .x[!tri_fn(.x)] <- NA

      rws <- .x[clr_idx, ]
      cls <- .x[, clr_idx]

      min(rws, cls, na.rm = TRUE)
    })

  } else if (identical(comparison, "idx_vs_idx")) {
    res <- purrr::map_dbl(dist_lst, ~ {
      .x[!tri_fn(.x)] <- NA

      min(.x[clr_idx, clr_idx], na.rm = TRUE)
    })
  }

  res <- min(res)

  res
}

#' Get color index from vector of indices or names
#'
#' @param clrs Vector of colors
#' @param idx Vector with numeric indices or names to match with clrs vector
#' @noRd
.get_clr_idx <- function(clrs, idx) {

  if (is.null(idx)) return(NULL)

  res <- idx

  if (is.character(idx)) {
    if (is.null(names(clrs))) {
      cli::cli_abort(
        "Names must be provided for `colors` when `adjust_colors` or
         `exclude_colors` is a character vector."
      )
    }

    res <- match(idx, names(clrs))

    if (any(is.na(res))) {
      cli::cli_abort(
        "Not all values for `adjust_colors` and `exclude_colors`
         are present in `colors`."
      )
    }
  }

  res
}

#' Colorspace parameters for color properties
#'
#' For each property list colorspace, column number where values are located in
#' matrix, and default range.
#' @noRd
PROP_PARAMS <- list(
  lightness  = list("lab", 1, c(20, 80)),
  a          = list("lab", 2, c(-128, 127)),
  b          = list("lab", 3, c(-128, 127)),
  hue        = list("hsl", 1, c(0, 360)),
  saturation = list("hsl", 2, c(0, 100)),
  red        = list("rgb", 1, c(0, 255)),
  green      = list("rgb", 2, c(0, 255)),
  blue       = list("rgb", 3, c(0, 255))
)

.properties <- names(PROP_PARAMS)

#' Check arguments
#' @noRd
.chk_spruce_args <- function(colors = NULL, difference = NULL, method = NULL,
                             range = NULL,
                             adjust_colors = NULL, exclude_colors = NULL,
                             maxit = NULL, n = NULL, exact = NULL,
                             keep_original = NULL) {

  # Check colors
  if (!missing(colors) && !rlang::is_null(colors)) {
    if (!rlang::is_character(colors) || !rlang::has_length(colors)) {
      cli::cli_abort("`colors` must be a character vector.")
    }
  }

  # Check difference
  if (!missing(difference)) {
    if (!rlang::is_bare_numeric(difference) || !rlang::has_length(difference, 1)) {
      cli::cli_abort("`difference` must be a single numeric value.")
    }
  }

  # Check method
  if (!missing(method)) {
    pos_mets <- c(
      "euclidean", "CMC",
      "CIE1976", "CIE94", "CIE2000"
    )

    res <- rlang::arg_match(method, pos_mets, multiple = FALSE)
  }

  # Check range
  # * if not named, then range is for a single property and should be length 2
  # * could still be a list of vectors with upper and lower bound for each color
  if (!missing(range) && !rlang::is_null(range)) {
    err <- function() {
      cli::cli_abort("Provided ranges must include two numeric values.")
    }

    if (rlang::is_null(names(range)) || (rlang::is_vector(range) && !rlang::is_list(range))) {
      range <- list(range)
    }

    colors_provided <- !missing(colors)

    purrr::walk(range, ~ {
      if (!rlang::has_length(.x, 2)) err()

      # each upper/lower bound must be length 1, or a value for each color
      if (colors_provided) {
        purrr::walk(.x, ~ {
          if (!rlang::has_length(.x, 1) && length(.x) != length(colors)) err
        })

      } else {
        purrr::walk(.x, ~ if (!rlang::has_length(.x, 1)) err())
      }
    })
  }

  # Check adjust_colors
  if (!missing(adjust_colors) || !missing(exclude_colors)) {
    adj_ex_clrs <- list(adjust_colors, exclude_colors)

    purrr::walk(adj_ex_clrs, ~ {
      if (!rlang::is_null(.x)) {
        if (!rlang::is_bare_numeric(.x) && !rlang::is_character(.x)) {
          cli::cli_abort(
            "`adjust_colors` and `exclude_colors` must
            be a numeric or character vector."
          )
        }
      }
    })
  }

  # Check maxit
  if (!missing(maxit)) {
    if (!rlang::is_bare_numeric(maxit) || !rlang::has_length(maxit, 1)) {
      cli::cli_abort("`maxit` must be a single numeric value.")
    }
  }

  # Check n
  if (!missing(n)) {
    if (!rlang::is_bare_numeric(n) || !rlang::has_length(n, 1)) {
      cli::cli_abort("`n` must be a single numeric value.")
    }
  }

  # Check exact
  if (!missing(exact) && !rlang::is_null(exact)) {
    if (!rlang::is_bare_logical(exact) || !rlang::has_length(exact, 1)) {
      cli::cli_abort("`exact` must be `TRUE` or `FALSE`.")
    }
  }

  # Check keep_original
  if (!missing(keep_original)) {
    if (!rlang::is_bare_logical(keep_original) || !rlang::has_length(keep_original, 1)) {
      cli::cli_abort("`keep_original` must be `TRUE` or `FALSE`.")
    }
  }

  invisible(NULL)
}

.chk_filt_args <- function(filter, multi = TRUE) {

  # Set color filters to test
  # * always include "none" since this tests without a filter
  filter   <- filter %||% "none"
  pos_cb   <- c("deutan", "protan", "tritan")
  pos_filt <- c("none", pos_cb)

  if (multi) {
    filter   <- unique(c(filter, "none"))
    pos_filt <- c(pos_filt, "colorblind")
  }

  filter <- rlang::arg_match(filter, pos_filt, multiple = multi)

  if ("colorblind" %in% filter) {
    filter <- filter[filter != "colorblind"]
    filter <- c(filter, pos_cb)
  }

  res <- purrr::set_names(unique(filter))

  res
}

.chk_prop_args <- function(property, multi = TRUE) {
  res <- rlang::arg_match(
    property,
    c("interp", .properties),
    multiple = multi
  )

  res <- res[res != "interp"]

  res
}

.chk_range_args <- function(range, colors, idx) {

  if (rlang::is_list(range)) {
    res <- purrr::map(range, ~ {
      if (length(.x) != length(colors)) {
        cli::cli_abort(
          "If a list of vectors is provided for `range`,
           each vector must be the same length as `colors`."
        )
      }

      .x[idx]
    })

  } else {
    if (!rlang::has_length(range, 2)) {
      cli::cli_abort(
        "If a single vector is provided for `range`,
         it should include two values, the lower and upper bounds."
      )
    }

    if (range[1] >= range[2]) {
      cli::cli_abort(
        "The lower bound must be smaller than the upper bound."
      )
    }

    # For GenSA make sure final values are not integers
    res <- list(
      rep(as.numeric(range[1]), length(idx)),
      rep(as.numeric(range[2]), length(idx))
    )
  }

  res
}

#' Print warning if difference threshold is not met
#' @noRd
.warn_diff <- function(min_diff, difference) {
  if (min_diff < difference) {
    cli::cli_alert_info(c(
      "The minimum color difference for the adjusted palette ",
      "is {round(min_diff, 1)}, increase `maxit` to improve optimization."
    ))
  }
}
