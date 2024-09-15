#' Adjust colors based on similarity
#'
#' @param colors Character vector of colors to adjust
#' @param difference Color difference threshold (CIE200 score) to use for
#' adjusting `colors`.
#' Colors will be adjusted so the minimum pairwise difference
#' is greater than this threshold.
#' @param property Vector of color properties to adjust, can include any of:
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
#' @param maxit Maximum number of iterations to use when optimizing the color
#' palette.
#' Higher values will result in more optimal adjustments and a reduction in
#' speed.
#' @param ... Additional control parameters to pass to `GenSA::GenSA()`.
#' @export
spruce_up_colors <- function(colors, difference = 10,
                             property = c("lightness", "hue"),
                             method = "CIE2000", range = NULL,
                             filter = NULL, adjust_colors = NULL,
                             exclude_colors = NULL, maxit = 500,
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
  property <- .chk_prop_args(property, multi = TRUE)

  prop_params <- SPACE_PARAMS[property]

  # Set ranges for each property
  # * structure of range is checked earlier by .chk_spruce_args()
  # * if not named, assume length 2 and contains range for single color property
  # * if not named, apply range to first color property
  if (!rlang::is_null(range)) {
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

  if (!is.null(adjust_colors)) {
    clr_idx <- .get_clr_idx(colors, adjust_colors)
    clr_idx <- clr_idx[!clr_idx %in% ex_idx]

  } else {
    clr_idx <- purrr::map(clr_filts, ~ {
      d <- .compare_clrs(colors, filt = .x)[[1]]

      # Check if all colors are identical
      if (all(d == 0)) return(seq_along(colors))

      # Identify pairs of colors that are similar
      idx <- which(d < difference & upper.tri(d), arr.ind = TRUE)
      idx <- idx[!idx %in% ex_idx]

      unique(idx)
    })

    clr_idx <- purrr::reduce(clr_idx, unique)

    if (length(clr_idx) == 0) return(colors)
  }

  # Optimize colors
  sa_params <- list(
    maxit = maxit,
    threshold.stop = -difference,
    ...
  )

  sa_params$seed <- sa_params$seed %||% 42

  optim_res <- .run_gensa(
    clrs         = colors,
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
  if (min_diff < difference && is.null(adjust_colors)) {
    full_idx  <- seq_along(colors)
    full_idx  <- full_idx[!full_idx %in% ex_idx]

    full_res <- .run_gensa(
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

  if (min_diff < difference) {
    cli::cli_alert_info(c(
      "The minimum color difference for the adjusted palette ",
      "is {round(min_diff, 1)}, increase `maxit` to improve optimization."
    ))
  }

  # Return optimized colors
  res
}

#' Get color property
#'
#' @param colors Character vector of colors
#' @param property Vector of color properties to return, can include any of:
#' - "lightness", from LAB colorspace
#' - "a", from LAB colorspace
#' - "b", from LAB colorspace
#' - "hue", from HSL colorspace
#' - "saturation", from HSL colorspace
#' - "red", from RGB colorspace
#' - "green", from RGB colorspace
#' - "blue", from RGB colorspace
#' @export
get_property <- function(colors, property) {

  property <- .chk_prop_args(property = property, multi = TRUE)

  params <- SPACE_PARAMS[property]

  props <- purrr::imap(params, ~ {
    p <- farver::decode_colour(colors, to = .x[[1]])

    p[, .x[[2]]]
  })

  res <- tibble::as_tibble(props)

  res <- tibble::add_column(
    res,
    names   = names(colors),
    color   = colors,
    .before = 1
  )

  res
}

#' Plot color palette
#'
#' @param colors Character vector of colors to plot.
#' @param filter Filter to apply when plotting colors,
#' possible values include,
#' - "deutan"
#' - "protan"
#' - "tritan"
#' @param label_size Size of labels
#' @param label_color Color of labels
#' @param ... Additional arguments to pass to `ggplot2::geom_bar()`
#' @export
plot_colors <- function(colors, filter = NULL, label_size = 14,
                        label_color = "white", ...) {

  .chk_spruce_args(colors = colors)

  filter <- .chk_filt_args(filter = filter, multi = FALSE)

  colors <- .filter_clrs(colors, filter = unname(filter))

  x <- names(colors) %||% as.character(seq_along(colors))

  dat <- tibble::tibble(
    x   = factor(x, x),
    lab = colors
  )

  res <- ggplot2::ggplot(dat, ggplot2::aes(x, fill = x)) +
    ggplot2::geom_bar(...) +
    ggplot2::geom_text(
      ggplot2::aes(y = 0.5, label = .data$lab),
      angle = 90,
      color = label_color,
      size  = label_size / ggplot2::.pt
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0.03, 0.03))) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x     = ggplot2::element_text(size = 14)
    )

  res
}

#' Compare colors in palette
#'
#' @param colors Vector of colors
#' @param y Vector of colors to compare,
#' if `NULL`, `colors` will be compared with itself.
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
#' @param method Method to use for comparing colors, can be one of:
#' - "euclidian"
#' - "CIE1976"
#' - "CIE94"
#' - "CIE2000"
#' - "CMC"
#' @export
compare_colors <- function(colors, y = NULL, filter = NULL,
                           method = "CIE2000") {

  .chk_spruce_args(colors = colors, method = method)
  .chk_spruce_args(colors = y)

  filter <- .chk_filt_args(filter, multi = TRUE)

  dst <- .compare_clrs(
    colors,
    clrs2  = y,
    filt   = filter,
    method = method
  )

  min_diff <- .get_min_dist(dst, only_upper_tri = is.null(y))

  min_diff
}

#' Colorspace parameters for colors properties
#'
#' For each property list colorspace, column number where values are located in
#' matrix, and default range.
#' @noRd
SPACE_PARAMS <- list(
  lightness  = list("lab", 1, c(20, 80)),
  a          = list("lab", 2, c(-128, 127)),
  b          = list("lab", 3, c(-128, 127)),
  hue        = list("hsl", 1, c(0, 360)),
  saturation = list("hsl", 2, c(0, 100)),
  red        = list("rgb", 1, c(0, 255)),
  green      = list("rgb", 2, c(0, 255)),
  blue       = list("rgb", 3, c(0, 255))
)

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
    names(SPACE_PARAMS),
    multiple = multi
  )

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

#' Optimize colors using GenSA
#'
#' @param clrs Vector of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param prop_params List of vectors containing parameters for color properties.
#' Each vector must include in this order, the colorspace, numeric index for
#' column containing the color property to adjust, range of values to adjust.
#' @param method Method for comparing colors
#' @param filts Color filters to apply when calculating color differences.
#' @param gensa_params Named list with control parameters to pass to
#' `GenSA::GenSA()`
#' @noRd
.run_gensa <- function(clrs, clr_idx, prop_params, method, filts = "none",
                       gensa_params = list()) {

  .gensa <- function(clrs, clr_idx, method, filts, space, val_idx, range) {
    dec_clrs  <- farver::decode_colour(clrs, to = space)
    init_vals <- dec_clrs[clr_idx, val_idx]

    range <- .chk_range_args(range, clrs, clr_idx)

    res <- GenSA::GenSA(
      par     = as.numeric(init_vals),
      fn      = .sa_obj_fn,
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
#' @param values Vector of values to adjust, e.g. lightness values
#' @param clrs Vector of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param method Method for comparing colors
#' @param clr_filt Vector of color filters to apply when calculating color
#' differences
#' @param space Colorspace to use for decoding colors and adjusting color
#' properties, e.g. "lab" for lightness
#' @param val_idx Single numerical index indicating the column of the decoded
#' color matrix containing the values to modify, e.g. 1 for lightness
#' @noRd
.sa_obj_fn <- function(values, clrs, clr_idx, method, clr_filt = "none",
                       space = "lab", val_idx = 1) {

  clrs <- farver::decode_colour(clrs, to = space)

  clrs[clr_idx, val_idx] <- values

  clrs <- farver::encode_colour(clrs, from = space)

  # Calculate pairwise CIEDE2000 differences
  dist_lst <- .compare_clrs(clrs, method = method, filt = clr_filt)
  min_diff <- .get_min_dist(dist_lst, clr_idx, comparison = "idx_vs_all")

  -min_diff
}

#' Calculate pairwise color differences
#'
#' @param clrs Vector of colors
#' @param clrs2 Vector of colors to compare to `clrs`
#' @param filt Vector of color filters to apply when calculating
#' differences
#' @param method Method to use for comparing colors
#' @noRd
.compare_clrs <- function(clrs, clrs2 = NULL, filt = "none",
                          method = "CIE2000") {

  res <- purrr::map(filt, ~ {
    filt_clrs <- .filter_clrs(clrs, filter = .x)
    filt_clrs <- farver::decode_colour(filt_clrs, to = "lab")

    filt_clrs2 <- NULL

    if (!is.null(clrs2)) {
      filt_clrs2 <- .filter_clrs(clrs2, filter = .x)
      filt_clrs2 <- farver::decode_colour(filt_clrs2, to = "lab")
    }

    farver::compare_colour(
      from       = filt_clrs,
      to         = filt_clrs2,
      from_space = "lab",
      method     = method
    )
  })

  res
}

#' Function to apply color filter
#'
#' @param clrs Vector of colors
#' @param filter Type of color filter to apply
#' @noRd
.filter_clrs <- function(clrs, filter) {

  if (rlang::is_null(filter) || identical(filter, "none")) return(clrs)

  fn_lst <- list(
    deutan = colorspace::deutan,
    protan = colorspace::protan,
    tritan = colorspace::tritan
  )

  fn_nms <- names(fn_lst)

  err <- function() {
    cli::cli_abort(
      "`filter` must be a function or {.or {.str {fn_nms}}}."
    )
  }

  if (rlang::is_function(filter)) {
    fn <- filter

  } else if (rlang::is_character(filter) && rlang::has_length(filter, 1)) {
    if (!filter %in% fn_nms) err()

    fn <- fn_lst[[filter]]

  } else {
    err()
  }

  res <- fn(clrs)

  res
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
