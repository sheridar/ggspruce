#' @rdname scale_manual
#' @export
scale_color_spruce <- function(..., values = NULL, difference = 10,
                               adjust = c("lightness", "hue"),
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
scale_fill_spruce <- function(..., values = NULL, difference = 10,
                              adjust = c("lightness", "hue"),
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
                         exclude_colors = NULL, maxit = 500) {

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

  pal <- function(n, clrs = values) {

    if (is.null(clrs)) {
      cli::cli_abort(
        "Must provide colors, {n} needed, but none provided."
      )
    }

    n_vals <- length(clrs)

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
           {n - n_vals} additional colors added,
           set `resize_palette` to `FALSE` to disable this behavior."
        )

      } else if (n < n_vals) {
        cli::cli_warn(
          "Too many colors provided,
           the {n} most distinct colors will be used,
           set `resize_palette` to `FALSE` to disable this behavior."
        )
      }

      clrs <- resize_palette(clrs, n, filter = filter)
    }

    if (resize_palette) {
      clrs <- spruce_up_colors(
        colors         = clrs,
        difference     = difference,
        adjust         = adjust,
        range          = range,
        filter         = filter,
        adjust_colors  = adjust_colors,
        exclude_colors = exclude_colors,
        maxit          = maxit
      )
    }

    clrs
  }

  # Return ggproto object
  # * use ScaleDiscreteSpruce to pull colors from previous scale
  # * only need to pull colors from previous scale when values are not provided
  if (is.null(values)) {
    sup <- ScaleDiscreteSpruce

  } else {
    sup <- ggplot2::ScaleDiscrete
  }

  ggplot2::discrete_scale(
    aesthetic, name = name,
    palette = pal, breaks = breaks, limits = limits,
    call = call, super = sup, ...
  )
}

#' @export
ScaleDiscreteSpruce <- ggplot2::ggproto("ScaleDiscreteSpruce", ggplot2::ScaleDiscrete)

#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add ScaleDiscreteSpruce
#' @export
ggplot_add.ScaleDiscreteSpruce <- function(object, plot, object_name) {

  add_spruce <- function(self, scale) {

    if (is.null(scale)) return()

    # if (is.null(scale$palette)) return()

    prev_aes <- self$find(scale$aesthetics)

    # Adjust colors from previous scale
    # * first apply pal function from prev scale to generate color palette
    if (any(prev_aes) && is(scale, "ScaleDiscreteSpruce")) {
      prev_pal   <- self$scales[prev_aes][[1]]$palette
      spruce_pal <- scale$palette

      new_pal <- function(n) {
        values <- prev_pal(n)
        pal    <- spruce_pal(n, values)

        pal
      }

      self$scales[prev_aes][[1]]$palette <- new_pal

    } else {
      if (any(prev_aes)) {
        # Get only the first aesthetic name in the returned vector -- it can
        # sometimes be c("x", "xmin", "xmax", ....)
        scalename <- self$scales[prev_aes][[1]]$aesthetics[1]

        cli::cli_inform(c(
          "Scale for {.field {scalename}} is already present.",
          "Adding another scale for {.field {scalename}}, which will replace the existing scale."
        ))
      }

      # Remove old scale for this aesthetic (if it exists)
      self$scales <- c(self$scales[!prev_aes], list(scale))
    }
  }

  # Use special add() function for spruce scale
  plot$scales$add <- add_spruce

  plot$scales$add(object)

  plot
}
