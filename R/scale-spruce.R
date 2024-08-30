
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

#' Objective function for optimizing colors
#'
#' @param values Vector of values to adjust, e.g. lightness values
#' @param clrs Vector of colors
#' @param clr_idx Numerical index indicating which colors to adjust
#' @param clr_filt Vector of colorblind filters to apply when calculating color
#' differences
#' @param space Colorspace to use for decoding colors and adjusting color
#' attributes, e.g. "lab" for lightness
#' @param val_idx Single numerical index indicating the column of the decoded
#' color matrix containing the values to modify, e.g. 1 for lightness
#' @param only_clr_idx Only assess distance for colors specified by clr_idx
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
#' @param filt Vector of colorblind filters to apply when calculating
#' differences
#' @noRd
.compare_clrs <- function(clrs, filt = "none") {

  res <- purrr::map(filt, ~ {
    filt_clrs <- .filter_clrs(clrs, filt = .x)
    filt_clrs <- farver::decode_colour(filt_clrs, to = "lab")

    farver::compare_colour(
      from       = filt_clrs,
      from_space = "lab",
      method     = "CIE2000"
    )
  })

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

#' Function to apply colorblind filter
#'
#' @param clrs Vector of colors
#' @param filt_type Type of colorblind filter to apply
#' @noRd
.filter_clrs <- function(clrs, filt_type) {
  if (identical(filt_type, "none")) return(clrs)

  clrs <- dichromat::dichromat(clrs, type = filt_type)

  clrs
}

#' Adjust colors based on similarity
#'
#' @param colors Colors to adjust
#' @param difference Color difference threshold (CIE200 score) to use for
#' adjusting `colors`.
#' Colors will be adjusted so the minimum pairwise difference
#' is greater than this threshold.
#' @param adjust Color attribute to adjust
#' @param colorblind Colorblind filter to apply to color palette when
#' calculating pairwise differences.
#' Colors will be adjusted to minimize the pairwise difference before and after
#' applying the filter.
#' A vector can be passed to adjust based on multiple colorblind filters.
#' Possible values include, "none", "deutan", "protan", and "tritan".
#' @param highlight Index indicating color(s) to specifically adjust.
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
                             colorblind = "none", highlight = NULL,
                             range = NULL, maxit = 500) {

  adj_params <- switch(
    adjust,
    lightness  = list("lab", 1, c(0, 80)),
    a          = list("lab", 2, c(-128, 127)),
    b          = list("lab", 3, c(-128, 127)),
    hue        = list("hsl", 1, c(0, 360)),
    saturation = list("hsl", 2, c(0, 100))
  )

  space   <- adj_params[[1]]
  val_idx <- adj_params[[2]]
  range   <- range %||% adj_params[[3]]

  # Decode colors
  dec_clrs <- farver::decode_colour(colors, to = space)

  # Set color filters to test
  clr_filts <- unique(c("none", colorblind))
  clr_filts <- purrr::set_names(clr_filts)

  # Identify colors to adjust
  if (!is.null(highlight)) {
    clr_idx <- highlight

    if (is.character(highlight)) {
      if (is.null(names(colors))) {
        cli::cli_abort(
          "Names must be provided for `colors` when `highlight` is a character
           vector."
        )
      }

      clr_idx <- match(highlight, names(colors))

      if (any(is.na(clr_idx))) {
        cli::cli_abort(
          "Not all values for `highlight` are present in `colors`."
        )
      }
    }

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

    if (length(clr_idx) == 0) return(colors)
  }

  # Optimize colors
  gensa_params <- list(
    maxit          = maxit,
    threshold.stop = -difference,
    seed           = 42
  )

  .run_gensa <- function(clr_idx) {
    init_vals    <- dec_clrs[clr_idx, val_idx]
    lower_bounds <- rep(range[1], length(init_vals))
    upper_bounds <- rep(range[2], length(init_vals))

    res <- GenSA::GenSA(
      par     = init_vals,
      fn      = .obj_fn,
      lower   = lower_bounds,
      upper   = upper_bounds,
      control = gensa_params,

      clrs         = colors,
      clr_idx      = clr_idx,
      clr_filt     = clr_filts,
      space        = space,
      val_idx      = val_idx,
      only_clr_idx = !is.null(highlight)
    )

    res
  }

  optim_res <- .run_gensa(clr_idx)
  min_diff  <- -optim_res$value

  # Check difference for optimized colors
  # * if difference not met, rerun and allow all colors to be adjusted
  # * do not do this when highlight colors are passed
  if (min_diff < difference && is.null(highlight)) {
    full_idx  <- seq_along(colors)
    full_res  <- .run_gensa(full_idx)
    full_diff <- -full_res$value

    if (full_diff > min_diff) {
      optim_res <- full_res
      min_diff  <- full_diff
      clr_idx   <- full_idx
    }
  }

  if (min_diff < difference) {
    cli::cli_warn(
      "The minimum color difference for the adjusted color palette is {round(min_diff, 1)},
       increase `maxit` to improve optimization."
    )
  }

  # Return optimized colors
  dec_clrs[clr_idx, val_idx] <- optim_res$par

  # Convert back to hex
  res <- farver::encode_colour(dec_clrs, from = space)

  res
}

#' Automatically adjust color values
#'
#' @export
scale_color_spruce <- function(..., values, difference = 10, adjust = "lightness",
                               colorblind = "none", highlight = NULL,
                               range = NULL, maxit = 500,
                               aesthetics = "colour", breaks = ggplot2::waiver(),
                               na.value = "grey50") {

  values <- spruce_up_colors(
    colors     = values,
    difference = difference,
    adjust     = adjust,
    colorblind = colorblind,
    highlight  = highlight,
    range      = range,
    maxit      = maxit
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
                              colorblind = "none", highlight = NULL,
                              range = NULL, maxit = 500,
                              aesthetics = "fill", breaks = ggplot2::waiver(),
                              na.value = "grey50") {

  values <- spruce_up_colors(
    colors     = values,
    difference = difference,
    adjust     = adjust,
    colorblind = colorblind,
    highlight  = highlight,
    range      = range,
    maxit      = maxit
  )

  ggplot2::scale_fill_manual(
    ...,
    values     = values,
    aesthetics = aesthetics,
    breaks     = breaks,
    na.value   = na.value
  )
}
