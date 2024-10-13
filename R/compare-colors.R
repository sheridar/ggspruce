#' Compare colors in palette
#'
#' @param colors Vector of colors
#' @param y Vector of colors to compare,
#' if `NULL`, `colors` will be compared with itself.
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences.
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
#' @param return_mat Return matrix of minimum pairwise color differences for
#' specified color filters.
#' @param names Should color names be included in output
#' @export
compare_colors <- function(colors, y = NULL, filter = NULL, method = "CIE2000",
                           return_mat = FALSE, names = TRUE) {

  .chk_spruce_args(colors = colors, method = method)
  .chk_spruce_args(colors = y)

  up_tri <- is.null(y)

  y <- y %||% colors

  filter <- .chk_filt_args(filter, multi = TRUE)

  dst <- .compare_clrs(
    colors,
    clrs2  = y,
    filt   = filter,
    method = method
  )

  if (return_mat) {
    dst <- purrr::reduce(dst, pmin)

    if (names) {
      rownames(dst) <- y
      colnames(dst) <- colors
    }

    return(dst)
  }

  # Calc min difference for each color
  if (up_tri) {
    min_diff <- purrr::map_dbl(seq_along(colors), ~ {
      .get_min_dist(
        dst,
        clr_idx = .x,
        only_upper_tri = TRUE
      )
    })

  } else {
    dst <- purrr::reduce(dst, pmin)

    min_diff <- purrr::map_dbl(seq_along(colors), ~ min(dst[, .x]))
  }

  if (names) names(min_diff) <- colors

  min_diff
}

#' Subset colors based on pairwise differences or color properties
#'
#' @param colors Vector of colors
#' @param ... Subsetting specification, colors can be subsetted based on
#' minimum pairwise differences or color properties (refer to [get_property()]).
#' To subset based on color differences, use the `difference` variable,
#' e.g. `difference > 10`.
#' To subset based on color properties, refer to them by name,
#' e.g. `lightness > 50`.
#' @param method Method to use for comparing colors, refer to [compare_colors()]
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences, refer to [compare_colors()]
#' @export
subset_colors <- function(colors, ..., method = "CIE2000", filter = NULL) {

  # Calculate pairwise differences for colors
  min_diff <- compare_colors(
    colors,
    method = method,
    filter = filter,
    names  = FALSE
  )

  # Get color properties
  props <- get_property(colors, property = .properties)

  props$difference <- min_diff

  # Subset colors
  res <- subset(props, ...)
  res <- res$color

  res
}

#' Sort colors based on property
#'
#' @param colors Vector of colors
#' @param ... Color attribute(s) to use for sorting, this can include
#' minimum pairwise color differences or color properties
#' (refer to [get_property()]).
#' Specify 'difference' to sort based on color differences.
#' @param method Method to use for comparing colors, refer to [compare_colors()]
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences, refer to [compare_colors()]
#' @param desc Sort in descending order
#' @export
sort_colors <- function(colors, ..., method = "CIE2000", filter = NULL,
                        desc = FALSE) {

  clmns <- c(...)

  if (purrr::is_empty(clmns)) {
    cli::cli_abort("Must provide at least one property to use for sorting.")
  }

  props <- tibble::tibble(color = colors)

  # Get color properties
  prop_clmns <- clmns[clmns != "difference"]

  if (!purrr::is_empty(prop_clmns)) {
    props <- get_property(colors, property = prop_clmns)
  }

  # Calculate pairwise differences for colors
  if ("difference" %in% clmns) {
    min_diff <- compare_colors(
      colors,
      method = method,
      filter = filter,
      names  = FALSE
    )

    props$difference <- min_diff
  }

  # Sort colors
  args <- as.list(props[, clmns])

  args$decreasing <- desc

  idx <- .lift(order)(args)

  res <- props$color[idx]

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
#' - "lightness-2", from HSL colorspace
#' - "red", from RGB colorspace
#' - "green", from RGB colorspace
#' - "blue", from RGB colorspace
#' @export
get_property <- function(colors, property = .properties) {

  property <- .chk_prop_args(property = property, multi = TRUE)

  params <- PROP_PARAMS[property]

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
      from       = filt_clrs2 %||% filt_clrs,
      to         = filt_clrs,
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
