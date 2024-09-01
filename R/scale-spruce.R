
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

#' Create your own discrete scale
#'
#' These functions allow you to specify your own set of mappings from levels in the
#' data to aesthetic values.
#'
#' The functions `scale_colour_manual()`, `scale_fill_manual()`, `scale_size_manual()`,
#' etc. work on the aesthetics specified in the scale name: `colour`, `fill`, `size`,
#' etc. However, the functions `scale_colour_manual()` and `scale_fill_manual()` also
#' have an optional `aesthetics` argument that can be used to define both `colour` and
#' `fill` aesthetic mappings via a single function call (see examples). The function
#' `scale_discrete_manual()` is a generic scale that can work with any aesthetic or set
#' of aesthetics provided via the `aesthetics` argument.
#'
#' @inheritParams ggplot2::scale_x_discrete
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics -palette -scale_name
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @param values a set of aesthetic values to map data values to. The values
#'   will be matched in order (usually alphabetical) with the limits of the
#'   scale, or with `breaks` if provided. If this is a named vector, then the
#'   values will be matched based on the names instead. Data values that don't
#'   match will be given `na.value`.
#' @param difference Color difference threshold (CIE200 score) to use for
#' adjusting `colors`.
#' Colors will be adjusted so the minimum pairwise difference
#' is greater than this threshold.
#' @param adjust Color property to adjust, a vector of multiple properties can
#' also be provided, possible values include:
#' - "lightness"
#' - "a"
#' - "b"
#' - "hue"
#' - "saturation
#'
#' @param range A vector containing the minimum and maximum values to use when
#' adjusting the specified color property.
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' Colors will be adjusted to minimize the pairwise difference before and after
#' applying the filter.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include, "none", "deutan", "protan", and "tritan".
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
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks (the scale limits)
#'   - A character vector of breaks
#'   - A function that takes the limits as input and returns breaks
#'     as output
#'
#' @param na.value The aesthetic value to use for missing (`NA`) values
#' @family colour scales
#' @seealso
#' The documentation for [differentiation related aesthetics][aes_linetype_size_shape].
#'
#' The documentation on [colour aesthetics][aes_colour_fill_alpha].
#'
#' The `r link_book(c("manual scales", "manual colour scales sections"), c("scales-other#sec-scale-manual", "scales-colour#sec-manual-colour"))`
#'
#' @section Color Blindness:
#' Many color palettes derived from RGB combinations (like the "rainbow" color
#' palette) are not suitable to support all viewers, especially those with
#' color vision deficiencies. Using `viridis` type, which is perceptually
#' uniform in both colour and black-and-white display is an easy option to
#' ensure good perceptive properties of your visualizations.
#' The colorspace package offers functionalities
#' - to generate color palettes with good perceptive properties,
#' - to analyse a given color palette, like emulating color blindness,
#' - and to modify a given color palette for better perceptivity.
#'
#' For more information on color vision deficiencies and suitable color choices
#' see the [paper on the colorspace package](https://arxiv.org/abs/1903.06490)
#' and references therein.
#' @name scale_manual
#' @aliases NULL
NULL

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
#' Possible values include, "none", "deutan", "protan", and "tritan".
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

#' @rdname scale_manual
#' @export
scale_color_spruce <- function(..., values, difference = 10, adjust = c("lightness", "hue"),
                               range = NULL, filter = NULL,
                               resize_palette = TRUE, adjust_colors = NULL,
                               exclude_colors = NULL, maxit = 500,
                               aesthetics = "colour", breaks = ggplot2::waiver(),
                               na.value = "grey50") {

  # spruce_up_colors() is called later when resizing palette
  if (!resize_palette || !is.null(names(values))) {
    values <- spruce_up_colors(
      colors         = values,
      difference     = difference,
      adjust         = adjust,
      range          = range,
      filter         = filter,
      adjust_colors  = adjust_colors,
      exclude_colors = exclude_colors,
      maxit          = maxit
    )
  }

  spruce_scale(
    aesthetics, values, breaks, ...,
    na.value = na.value,

    difference     = difference,
    adjust         = adjust,
    range          = range,
    filter         = filter,
    resize_palette = resize_palette,
    adjust_colors  = adjust_colors,
    exclude_colors = exclude_colors,
    maxit          = maxit
  )
}

#' @rdname scale_manual
#' @export
scale_fill_spruce <- function(..., values, difference = 10, adjust = c("lightness", "hue"),
                              range = NULL, filter = NULL,
                              resize_palette = TRUE, adjust_colors = NULL,
                              exclude_colors = NULL, maxit = 500,
                              aesthetics = "fill", breaks = ggplot2::waiver(),
                              na.value = "grey50") {

  # spruce_up_colors() is called later when resizing palette
  if (!resize_palette || !is.null(names(values))) {
    values <- spruce_up_colors(
      colors         = values,
      difference     = difference,
      adjust         = adjust,
      range          = range,
      filter         = filter,
      adjust_colors  = adjust_colors,
      exclude_colors = exclude_colors,
      maxit          = maxit
    )
  }

  spruce_scale(
    aesthetics, values, breaks, ...,
    na.value = na.value,

    difference     = difference,
    adjust         = adjust,
    range          = range,
    filter         = filter,
    resize_palette = resize_palette,
    adjust_colors  = adjust_colors,
    exclude_colors = exclude_colors,
    maxit          = maxit
  )
}

#' Modification of ggplot2::manual_scale to allow automatic expansion of colors
#'
#' https://github.com/tidyverse/ggplot2/blob/57ba97fa04dadc6fd73db1904e39a09d57a4fcbe/R/scale-manual.R#L146
#' @noRd
spruce_scale <- function(aesthetic, values = NULL, breaks = ggplot2::waiver(),
                         name = ggplot2::waiver(), ...,
                         limits = NULL, call = rlang::caller_call(),

                         difference = 10, adjust = c("lightness", "hue"),
                         range = NULL, filter = NULL,
                         resize_palette = TRUE, adjust_colors = NULL,
                         exclude_colors = NULL, maxit = 500
                         ) {

  # https://github.com/tidyverse/ggplot2/blob/57ba97fa04dadc6fd73db1904e39a09d57a4fcbe/R/utilities.R#L201
  is.waive <- function(x) inherits(x, "waiver")

  call <- call %||% rlang::current_call()

  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (rlang::is_missing(values)) {
    values <- NULL

  } else {
    force(values)
  }

  if (is.null(limits) && !is.null(names(values))) {

    # Limits as function to access `values` names later on (#4619)
    force(aesthetic)

    limits <- function(x) {
      x <- intersect(x, c(names(values), NA)) %||% character()

      if (length(x) < 1) {
        cli::cli_warn(paste0(
          "No shared levels found between {.code names(values)} of the manual ",
          "scale and the data's {.field {aesthetic}} values."
        ))
      }

      x
    }
  }

  # order values according to breaks
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks) && !is.function(breaks)) {

    if (length(breaks) <= length(values)) {
      names(values) <- breaks

    } else {
      names(values) <- breaks[seq_along(values)]
    }
  }

  # Adjust color palette provided by user
  # * only adjust palette if names are not provided
  resize_palette <- resize_palette && is.null(names(values))

  pal <- function(n) {
    n_vals <- length(values)

    if (n > n_vals && !resize_palette) {
      cli::cli_abort(
        "Insufficient number of colors provided,
         {n} needed but only {length(values)} provided."
      )
    }

    if (n != n_vals && resize_palette) {
      if (n > n_vals) {
        cli::cli_warn(
          "Insufficient number of colors provided,
           {n - length(values)} additional colors added,
           set `resize_palette` to `FALSE` to disable this behavior."
        )

      } else if (n < length(values)) {
        cli::cli_warn(
          "Too many colors provided,
           the {n} most distinct colors will be used,
           set `resize_palette` to `FALSE` to disable this behavior."
        )
      }

      values <- resize_palette(values, n, filter = filter)
    }

    if (resize_palette) {
      values <- spruce_up_colors(
        colors         = values,
        difference     = difference,
        adjust         = adjust,
        range          = range,
        filter         = filter,
        adjust_colors  = adjust_colors,
        exclude_colors = exclude_colors,
        maxit          = maxit
      )
    }

    values
  }

  ggplot2::discrete_scale(
    aesthetic, name = name,
    palette = pal, breaks = breaks, limits = limits,
    call = call, ...
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
#' properties, e.g. "lab" for lightness
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
.get_min_dist <- function(dist_lst, clr_idx = NULL, only_clr_idx = FALSE) {

  if (only_clr_idx) {
    if (is.null(clr_idx)) cli::cli_abort("`clr_idx` is required.")

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
