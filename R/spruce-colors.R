#' Adjust colors based on similarity
#'
#' @param colors Character vector of colors to adjust
#' @param difference Color difference threshold (CIE200 score) to use for
#' adjusting `colors`.
#' Colors will be adjusted so the minimum pairwise difference
#' is greater than this threshold.
#' @param adjust Color property to adjust, a vector of multiple properties can
#' also be provided, possible values include:
#'   - "lightness"
#'   - "a"
#'   - "b"
#'   - "hue"
#'   - "saturation
#' @param range A vector containing the minimum and maximum values to use when
#' adjusting colors.
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' Colors will be adjusted to minimize the pairwise difference before and after
#' applying the filter.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include, "deutan", "protan", and "tritan".
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
#' @export
spruce_up_colors <- function(colors, difference = 10, adjust = c("lightness", "hue"),
                             range = NULL, filter = NULL,
                             adjust_colors = NULL, exclude_colors = NULL,
                             maxit = 500) {

  adj_params <- list(
    lightness  = list("lab", 1, c(3, 85)),
    a          = list("lab", 2, c(-128, 127)),
    b          = list("lab", 3, c(-128, 127)),
    hue        = list("hsl", 1, c(0, 360)),
    saturation = list("hsl", 2, c(0, 100))
  )

  adj_params <- adj_params[adjust]

  if (!is.null(range)) {
    adj_params[[adjust[1]]][[3]] <- range
  }

  # Set color filters to test
  filter    <- filter %||% "none"
  clr_filts <- unique(c("none", filter))
  clr_filts <- purrr::set_names(clr_filts)

  # Identify colors to adjust
  ex_idx <- .get_clr_idx(colors, exclude_colors)

  if (!is.null(adjust_colors)) {
    clr_idx <- .get_clr_idx(colors, adjust_colors)
    clr_idx <- clr_idx[!clr_idx %in% ex_idx]

  } else {
    clr_idx <- purrr::map(clr_filts, ~ {
      d <- .compare_clrs(colors, filt = .x)[[1]]

      # Exclude ex_idx before identifying most similar pairs
      # * extra colors could be excluded if this is done after
      d[ex_idx, ex_idx] <- NA

      # Check if all colors are identical
      if (all(d == 0)) return(seq_along(colors))

      # Identify pairs of colors that are similar
      idx <- which(d < difference & upper.tri(d), arr.ind = TRUE)

      unique(idx[, 1])
    })

    clr_idx <- purrr::reduce(clr_idx, unique)

    if (length(clr_idx) == 0) return(colors)
  }

  # Optimize colors
  optim_res <- .run_gensa(
    clrs       = colors,
    clr_idx    = clr_idx,
    adj_params = adj_params,
    filts      = clr_filts,
    threshold  = -difference,
    maxit      = maxit
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
      clrs       = res,
      clr_idx    = full_idx,  # update to adjust all colors
      adj_params = adj_params,
      filts      = clr_filts,
      threshold  = -difference,
      maxit      = maxit
    )

    full_diff <- full_res[[2]]

    if (full_diff > min_diff) {
      res      <- full_res[[1]]
      min_diff <- full_diff
    }
  }

  if (min_diff < difference) {
    cli::cli_warn(
      "The minimum color difference for the adjusted color palette is {round(min_diff, 1)},
       increase `maxit` to improve optimization."
    )
  }

  # Return optimized colors
  res
}

#' Plot color palette
#'
#' @param colors Character vector of colors to adjust
#' @param label_size Size of labels
#' @param label_color Color of labels
#' @param ... Additional arguments to pass to `ggplot2::geom_bar()`
#' @export
plot_colors <- function(colors, label_size = 14, label_color = "white", ...) {

  x <- names(colors) %||% as.character(seq_along(colors))

  dat <- data.frame(
    x   = factor(x, x),
    lab = colors
  )

  res <- ggplot2::ggplot(dat, aes(x, fill = x)) +
    ggplot2::geom_bar(...) +
    ggplot2::geom_text(
      aes(y = 0.5, label = lab),
      angle = 90,
      color = label_color,
      size  = label_size / .pt
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
#' Possible values include, "deutan", "protan", and "tritan".
#' @param method Method to use for comparison,
#' refer to `farver::compare_colour()`.
#' @export
compare_colors <- function(colors, y = NULL, filter = NULL,
                           method = "CIE2000") {

  filter <- filter %||% "none"
  filter <- unique(c("none", filter))

  dst <- .compare_clrs(
    colors,
    clrs2  = y,
    filt   = filter,
    method = method
  )

  min_diff <- .get_min_dist(dst, only_upper_tri = is.null(y))

  min_diff
}

#' Optimize colors using GenSA
#'
#' @param clrs Vector of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param adj_params List of vectors containing parameters to use for adjusting
#' colors.
#' Each vector must include in this order, the colorspace, numeric index for
#' column containing the color property to adjust, range of values to adjust.
#' @param filts Color filters to apply when calculating color differences.
#' @param threshold Threshold at which optimization should stop.
#' @param maxit Maximum number of iterations of GenSA.
#' @param comparison How should colors provided in `clr_idx` be compared when
#' identifying minimum color differences, one of:
#' - "all_vs_all", select minimum distance using all comparisons.
#' - "idx_vs_all", select minimum distance for colors in `clr_idx` when compared
#'   to all other colors
#' - "idx_vs_idx", select minimum distance for colors in `clr_idx` when compared
#'   to all other colors in `clr_idx`
#' @noRd
.run_gensa <- function(clrs, clr_idx, adj_params, filts = "none",
                       threshold, maxit, comparison = "idx_vs_all") {

  sa_params <- list(
    maxit          = maxit,
    threshold.stop = threshold,
    seed           = 42
  )

  .gensa <- function(clrs, clr_idx, space, val_idx, range) {
    dec_clrs     <- farver::decode_colour(clrs, to = space)
    init_vals    <- dec_clrs[clr_idx, val_idx]
    lower_bounds <- rep(range[1], length(init_vals))
    upper_bounds <- rep(range[2], length(init_vals))

    res <- GenSA::GenSA(
      par     = init_vals,
      fn      = .sa_obj_fn,
      lower   = lower_bounds,
      upper   = upper_bounds,
      control = sa_params,

      clrs     = clrs,
      clr_idx  = clr_idx,
      clr_filt = filts,
      space    = space,
      val_idx  = val_idx
    )

    res
  }

  min_diff <- 0

  for (params in adj_params) {
    optim <- .gensa(
      clrs, clr_idx,
      params[[1]], params[[2]], params[[3]]
    )

    if (-optim$value > min_diff) {
      min_diff <- -optim$value

      dec_clrs <- farver::decode_colour(clrs, to = params[[1]])

      dec_clrs[clr_idx, params[[2]]] <- optim$par

      clrs <- farver::encode_colour(dec_clrs, from = params[[1]])
    }

    if (min_diff > -threshold) break()
  }

  # Return vector of hex colors and minimum difference
  list(clrs, min_diff)
}

#' Objective function for optimizing colors
#'
#' @param values Vector of values to adjust, e.g. lightness values
#' @param clrs Vector of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param clr_filt Vector of color filters to apply when calculating color
#' differences
#' @param space Colorspace to use for decoding colors and adjusting color
#' properties, e.g. "lab" for lightness
#' @param val_idx Single numerical index indicating the column of the decoded
#' color matrix containing the values to modify, e.g. 1 for lightness
#' @param comparison How should colors provided in `clr_idx` be compared when
#' identifying minimum color differences, one of:
#' - "all_vs_all", select minimum distance using all comparisons.
#' - "idx_vs_all", select minimum distance for colors in `clr_idx` when compared
#'   to all other colors
#' - "idx_vs_idx", select minimum distance for colors in `clr_idx` when compared
#'   to all other colors in `clr_idx`
#' @noRd
.sa_obj_fn <- function(values, clrs, clr_idx, clr_filt = "none",
                       space = "lab", val_idx = 1) {

  clrs <- farver::decode_colour(clrs, to = space)

  clrs[clr_idx, val_idx] <- values

  clrs <- farver::encode_colour(clrs, from = space)

  # Calculate pairwise CIEDE2000 differences
  dist_lst <- .compare_clrs(clrs, filt = clr_filt)
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

  fn_lst <- list(
    none   = (function(x) x),
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

  } else if (is.character(filter) && length(filter) == 1) {
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
