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
#' adjusting the specified color property.
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
#' Colors will be adjusted to minimize the pairwise difference before and after
#' applying the filter.
#' A vector can be passed to adjust based on multiple color filters.
#' Possible values include, "none", "deutan", "protan", and "tritan".
#' @param resize Should provided color palette be resized based on the
#' number of groups plotted.
#' If `TRUE`, when too few or too many colors are provided,
#' colors will be added using `interp_colors()`,
#' or removed using `collapse_colors()`.
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

#' @rdname scale_manual
#' @export
scale_colour_spruce <- function(..., values = NULL, difference = 10,
                                property = c("lightness", "hue"),
                                method = "CIE2000",
                                range = NULL, filter = NULL,
                                resize = TRUE, adjust_colors = NULL,
                                exclude_colors = NULL, maxit = 500,
                                aesthetics = "colour",
                                breaks = ggplot2::waiver(), na.value = "grey50") {

  pal_spruce <- .create_spruce_pal(
    values = values,
    resize = resize,
    filter = filter,

    difference     = difference,
    property       = property,
    method         = method,
    range          = range,
    adjust_colors  = adjust_colors,
    exclude_colors = exclude_colors,
    maxit          = maxit
  )

  if (!is.null(values)) {
    obj <- spruce_scale(
      aesthetics, values, breaks, ...,
      na.value = na.value,
      palette  = pal_spruce
    )

  } else {
    ScaleDiscreteSpruce <- ggplot2::ggproto(
      "ScaleDiscreteSpruce", ggplot2::ScaleDiscrete,
      spruce_palette = pal_spruce
    )

    obj <- ggplot2::scale_colour_discrete(
      ...,
      na.value = na.value,
      super    = ScaleDiscreteSpruce
    )
  }

  obj
}

#' @rdname scale_manual
#' @usage NULL
#' @export
scale_color_spruce <- scale_colour_spruce

#' @rdname scale_manual
#' @export
scale_fill_spruce <- function(..., values = NULL, difference = 10,
                              property = c("lightness", "hue"),
                              method = "CIE2000",
                              range = NULL, filter = NULL,
                              resize = TRUE, adjust_colors = NULL,
                              exclude_colors = NULL, maxit = 500,
                              aesthetics = "fill", breaks = ggplot2::waiver(),
                              na.value = "grey50") {

  pal_spruce <- .create_spruce_pal(
    values = values,
    resize = resize,
    filter = filter,

    difference     = difference,
    property       = property,
    method         = method,
    range          = range,
    adjust_colors  = adjust_colors,
    exclude_colors = exclude_colors,
    maxit          = maxit
  )

  if (!is.null(values)) {
    obj <- spruce_scale(
      aesthetics, values, breaks, ...,
      na.value = na.value,
      palette  = pal_spruce
    )

  } else {
    ScaleDiscreteSpruce <- ggplot2::ggproto(
      "ScaleDiscreteSpruce", ggplot2::ScaleDiscrete,
      spruce_palette = pal_spruce
    )

    obj <- ggplot2::scale_fill_discrete(
      ...,
      na.value = na.value,
      super    = ScaleDiscreteSpruce
    )
  }

  obj
}

#' Modification of ggplot2::manual_scale to allow a new palette to be passed to
#' ggplot2::discrete_scale()
#' * Ideally could just use ggplot2::scale_colour_manual(), but this will not
#'   pass a new palette to ggplot2::discrete_scale()
#' * manual_scale, which is used by ggplot2::scale_colour_manual() is an
#'   internal function not available to users
#'
#' https://github.com/tidyverse/ggplot2/blob/57ba97fa04dadc6fd73db1904e39a09d57a4fcbe/R/scale-manual.R#L146
#' @noRd
spruce_scale <- function(aesthetic, values = NULL, breaks = ggplot2::waiver(),
                         name = ggplot2::waiver(), ...,
                         limits = NULL, call = rlang::caller_call(),
                         palette) {

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

  # Return ggproto object
  ggplot2::discrete_scale(
    aesthetic, name = name,
    palette = palette, breaks = breaks, limits = limits,
    call = call, ...
  )
}

#' Create palette function to be used when modifying colors
#'
#' @param values Vector of colors
#' @param filter Color filter to pass to `spruce_up_colors()` and
#' `collapse_colors()`
#' @param resize Should vector of colors be resized, passed to
#' `spruce_up_colors()`
#' @param ... Additional arguments to pass to `spruce_up_colors()`
#' @noRd
.create_spruce_pal <- function(values = NULL, filter = "none",
                               resize = TRUE, ...) {

  .create_pal <- function(n) {
    if (is.null(values)) {
      cli::cli_abort(
        "Must provide colors, {n} needed, but none provided."
      )
    }

    n_vals <- length(values)

    if (n > n_vals && !resize) {
      cli::cli_abort(
        "Insufficient number of colors provided,
         {n} needed but only {length(values)} provided."
      )
    }

    if (n != n_vals && resize) {
      if (n > n_vals) {
        cli::cli_alert_info(c(
          "Insufficient number of colors provided, ",
           "{n - n_vals} additional colors added, ",
           "set `resize` to `FALSE` to disable this behavior."
        ))

      } else if (n < n_vals) {
        cli::cli_alert_info(
          "Too many colors provided,
           the {n} most distinct colors will be used,
           set `resize` to `FALSE` to disable this behavior."
        )
      }

      values <- resize_colors(values, n, filter = filter)
    }

    # Pull first `n` colors, if `resize` is `FALSE` all colors will
    # get passed to `spruce_up_colors`
    values <- spruce_up_colors(
      colors = values[seq_len(n)],
      filter = filter,
      ...
    )

    values
  }

  # Need to dynamically set the environment for .create_pal() so it has access
  # to the current function environment when the returned palette function is
  # called
  # * if the user does not provide input color values, a version of
  #   ggplot2::manual_scale() is used, and ggplot2 default colors are later
  #   passed to the palette function by ggplot_add.ScaleDiscreteSpruce()
  # * if the user does provide input colors, ggplot2::scale_colour_discrete()
  #   is used and the palette function needs access to the `values` variable
  #   present in the enclosing environment when the palette function is called
  if (is.null(values)) {
    function(n, values) {
      environment(.create_pal) <- environment()

      .create_pal(n)
    }

  } else {
    function(n) {
      environment(.create_pal) <- environment()

      .create_pal(n)
    }
  }
}

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add ScaleDiscreteSpruce
#' @export
ggplot_add.ScaleDiscreteSpruce <- function(object, plot, object_name) {

  # Use special add() function to pull colors from previous scales
  add_spruce <- function(self, scale) {
    if (is.null(scale)) return()

    prev_aes <- self$find(scale$aesthetics)

    # Need to check for correct class since once add_spruce() is added to the
    # object, it will be used for all other scale additions
    # * get error if original add function is added back to object
    is_spruce <- methods::is(scale, "ScaleDiscreteSpruce")

    # Adjust colors from previous scale
    # * only merge palettes if prev_scale is discrete
    # * first apply pal function from prev scale to generate color palette
    if (any(prev_aes) && is_spruce) {
      prev_scale <- self$scales[[prev_aes]]

      if (inherits(prev_scale, "ScaleDiscrete")) {
        prev_pal   <- prev_scale$palette
        spruce_pal <- scale$spruce_palette

        new_pal <- function(n) {
          values <- prev_pal(n)
          pal    <- spruce_pal(n, values)

          pal
        }

        self$scales[prev_aes][[1]]$palette <- new_pal

        scale <- NULL
      }

    } else if (any(prev_aes)) {
      # Get only the first aesthetic name in the returned vector -- it can
      # sometimes be c("x", "xmin", "xmax", ....)
      scalename <- self$scales[prev_aes][[1]]$aesthetics[1]

      cli::cli_inform(c(
        "Scale for {.field {scalename}} is already present.",
        "Adding another scale for {.field {scalename}},
           which will replace the existing scale."
      ))

      # Remove old scale for this aesthetic (if it exists)
      self$scales <- self$scales[!prev_aes]

    } else if (is_spruce) {
      prev_pal   <- scale$palette
      spruce_pal <- scale$spruce_palette

      new_pal <- function(n) {
        values <- prev_pal(n)
        pal    <- spruce_pal(n, values)

        pal
      }

      scale$palette <- new_pal
    }

    scales <- self$scales

    if (!is.null(scale)) scales <- c(scales, list(scale))

    self$scales <- scales
  }

  plot$scales$add <- add_spruce

  plot$scales$add(object)

  plot
}
