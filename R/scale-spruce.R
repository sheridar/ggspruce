
#' Tidy eval helpers
#'
#' @description
#'
#' * \code{\link[rlang]{sym}()} creates a symbol from a string and
#'   \code{\link[rlang:sym]{syms}()} creates a list of symbols from a
#'   character vector.
#'
#' * \code{\link[rlang:nse-defuse]{enquo}()} and
#'   \code{\link[rlang:nse-defuse]{enquos}()} delay the execution of one or
#'   several function arguments. \code{enquo()} returns a single quoted
#'   expression, which is like a blueprint for the delayed computation.
#'   \code{enquos()} returns a list of such quoted expressions.
#'
#' * \code{\link[rlang:nse-defuse]{expr}()} quotes a new expression _locally_. It
#'   is mostly useful to build new expressions around arguments
#'   captured with [enquo()] or [enquos()]:
#'   \code{expr(mean(!!enquo(arg), na.rm = TRUE))}.
#'
#' * \code{\link[rlang]{as_name}()} transforms a quoted variable name
#'   into a string. Supplying something else than a quoted variable
#'   name is an error.
#'
#'   That's unlike \code{\link[rlang]{as_label}()} which also returns
#'   a single string but supports any kind of R object as input,
#'   including quoted function calls and vectors. Its purpose is to
#'   summarise that object into a single label. That label is often
#'   suitable as a default name.
#'
#'   If you don't know what a quoted expression contains (for instance
#'   expressions captured with \code{enquo()} could be a variable
#'   name, a call to a function, or an unquoted constant), then use
#'   \code{as_label()}. If you know you have quoted a simple variable
#'   name, or would like to enforce this, use \code{as_name()}.
#'
#' To learn more about tidy eval and how to use these tools, visit
#' \url{https://tidyeval.tidyverse.org} and the
#' \href{https://adv-r.hadley.nz/metaprogramming.html}{Metaprogramming
#' section} of \href{https://adv-r.hadley.nz}{Advanced R}.
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang %||%
#' @aliases %||%
#' @export %||%
NULL

#' Adjust colors based on similarity
#'
#' @param colors Character vector of colors to adjust
#' @param difference Color difference threshold (CIE200 score) to use for
#' adjusting `colors`.
#' Colors will be adjusted so the minimum pairwise difference
#' is greater than this threshold.
#' @param adjust Color attribute to adjust
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' Colors will be adjusted to minimize the pairwise difference before and after
#' applying the filter.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include, "none", "deutan", "protan", and "tritan".
#' @param adjust_colors Index indicating color(s) to specifically adjust.
#' Should be an integer vector, or a character vector containing names matching
#' those provided for `colors`.
#' @param range A vector containing the minimum and maximum values to use when
#' adjusting colors.
#' @param maxit Maximum number of iterations to use when optimizing the color
#' palette.
#' Higher values will result in more optimal adjustments and a reduction in
#' speed.
#' @export
spruce_up_colors <- function(colors, difference = 10, adjust = "lightness",
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

  # Set color filters to test
  filter    <- filter %||% "none"
  clr_filts <- unique(c("none", filter))
  clr_filts <- purrr::set_names(clr_filts)

  # Identify colors to adjust
  ex_idx <- .get_clr_idx(colors, exclude_colors)

  if (!is.null(adjust_colors)) {
    clr_idx <- .get_clr_idx(colors, adjust_colors)

  } else {
    clr_idx <- purrr::map(clr_filts, ~ {
      d <- .compare_clrs(colors, .x)[[1]]

      # Check if colors are identical
      if (all(d == 0)) return(seq_along(colors))

      # Identify pairs of colors that are similar
      idx <- which(d < difference & upper.tri(d), arr.ind = TRUE)

      unique(idx[, 1])
    })

    clr_idx <- purrr::reduce(clr_idx, unique)

    clr_idx <- clr_idx[!clr_idx %in% ex_idx]

    if (length(clr_idx) == 0) return(colors)
  }

  # Optimize colors
  optim_res <- .run_gensa(
    clrs         = colors,
    clr_idx      = clr_idx,
    adj_params   = adj_params,
    filts        = clr_filts,
    threshold    = -difference,
    maxit        = maxit,
    only_clr_idx = !is.null(adjust_colors)
  )

  res      <- optim_res[[1]]
  min_diff <- optim_res[[2]]

  # Check difference for optimized colors
  # * if difference not met, rerun and allow all colors to be adjusted
  # * do not do this when adjust_colors colors are passed
  if (min_diff < difference && is.null(adjust_colors)) {
    full_idx  <- seq_along(colors)
    full_idx  <- full_idx[!full_idx %in% ex_idx]

    full_res <- .run_gensa(
      clrs         = colors,
      clr_idx      = full_idx,  # update to adjust all colors
      adj_params   = adj_params,
      filts        = clr_filts,
      threshold    = -difference,
      maxit        = maxit,
      only_clr_idx = !is.null(adjust_colors)
    )

    full_diff <- full_res[[2]]

    if (full_diff > min_diff) {
      res       <- full_res[[1]]
      min_diff  <- full_diff
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

#' Automatically adjust color values
#'
#' @export
scale_color_spruce <- function(..., values, difference = 10, adjust = "lightness",
                               filter = NULL, adjust_colors = NULL,
                               range = NULL, maxit = 500,
                               aesthetics = "colour", breaks = ggplot2::waiver(),
                               na.value = "grey50") {

  values <- spruce_up_colors(
    colors        = values,
    difference    = difference,
    adjust        = adjust,
    filter        = filter,
    adjust_colors = adjust_colors,
    range         = range,
    maxit         = maxit
  )

  ggplot2::scale_color_manual(
    ...,
    values     = values,
    aesthetics = aesthetics,
    breaks     = breaks,
    na.value   = na.value
  )
}

#' Automatically adjust fill values
#'
#' @export
scale_fill_spruce <- function(..., values, difference = 10, adjust = "lightness",
                              filter = NULL, adjust_colors = NULL,
                              range = NULL, maxit = 500,
                              aesthetics = "fill", breaks = ggplot2::waiver(),
                              na.value = "grey50") {

  values <- spruce_up_colors(
    colors        = values,
    difference    = difference,
    adjust        = adjust,
    filter        = filter,
    adjust_colors = adjust_colors,
    range         = range,
    maxit         = maxit
  )

  ggplot2::scale_fill_manual(
    ...,
    values     = values,
    aesthetics = aesthetics,
    breaks     = breaks,
    na.value   = na.value
  )
}

#' Plot color palette
#'
#' @param colors Character vector of colors to adjust
#' @param label_size Size of labels
#' @param label_color Color of labels
#' @param ... Additional arguments to pass to `ggplot2::geom_bar`
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

#' Optimize colors using GenSA
#'
#' @param clrs Vecotr of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param adj_params List of vectors containing parameters to use for adjusting
#' colors.
#' Each vector must include in this order, the colorspace, numeric index for
#' column containing the color property to adjust, range of values to adjust.
#' @param filts Color filters to apply when calculating color differences.
#' @param threshold Threshold at which optimization should stop.
#' @param maxit Maximum number of iterations of GenSA.
#' @param only_clr_idx When adjusting colors, only assess distance for colors
#' specified by clr_idx, i.e. compare clr_idx colors with all other colors.
#' @noRd
.run_gensa <- function(clrs, clr_idx, adj_params, filts = "none",
                       threshold, maxit, only_clr_idx = FALSE) {

  gensa_params <- list(
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
      fn      = .obj_fn,
      lower   = lower_bounds,
      upper   = upper_bounds,
      control = gensa_params,

      clrs         = clrs,
      clr_idx      = clr_idx,
      clr_filt     = filts,
      space        = space,
      val_idx      = val_idx,
      only_clr_idx = only_clr_idx
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
#' attributes, e.g. "lab" for lightness
#' @param val_idx Single numerical index indicating the column of the decoded
#' color matrix containing the values to modify, e.g. 1 for lightness
#' @param only_clr_idx When adjusting colors, only assess distance for colors
#' specified by clr_idx, i.e. compare clr_idx colors with all other colors.
#' @noRd
.obj_fn <- function(values, clrs, clr_idx, clr_filt = "none", space = "lab",
                    val_idx = 1, only_clr_idx = FALSE) {

  clrs <- farver::decode_colour(clrs, to = space)

  clrs[clr_idx, val_idx] <- values

  clrs <- farver::encode_colour(clrs, from = space)

  # Calculate pairwise CIEDE2000 differences
  dist_lst <- .compare_clrs(clrs, filt = clr_filt)
  min_diff <- .get_min_dist(dist_lst, clr_idx, only_clr_idx)

  -min_diff
}

#' Calculate pairwise color differences
#'
#' @param clrs Vector of colors
#' @param filt Vector of color filters to apply when calculating
#' differences
#' @noRd
.compare_clrs <- function(clrs, filt = "none") {

  res <- purrr::map(filt, ~ {
    filt_clrs <- .filter_clrs(clrs, filter = .x)
    filt_clrs <- farver::decode_colour(filt_clrs, to = "lab")

    farver::compare_colour(
      from       = filt_clrs,
      from_space = "lab",
      method     = "CIE2000"
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
#' @noRd
.get_min_dist <- function(dist_lst, clr_idx, only_clr_idx) {

  if (only_clr_idx) {
    res <- purrr::map_dbl(dist_lst, ~ {
      .x[!upper.tri(.x)] <- NA

      rws <- .x[clr_idx, ]
      cls <- .x[, clr_idx]

      min(rws, cls, na.rm = TRUE)
    })

  } else {
    res <- purrr::map_dbl(dist_lst, ~ min(.x[upper.tri(.x)]))
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
