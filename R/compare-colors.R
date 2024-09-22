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
#' @export
compare_colors <- function(colors, y = NULL, filter = NULL,
                           method = "CIE2000", return_mat = FALSE) {

  .chk_spruce_args(colors = colors, method = method)
  .chk_spruce_args(colors = y)

  filter <- .chk_filt_args(filter, multi = TRUE)

  dst <- .compare_clrs(
    colors,
    clrs2  = y,
    filt   = filter,
    method = method
  )

  if (return_mat) {
    dst <- purrr::reduce(dst, pmin)

    colnames(dst) <- rownames(dst) <- colors

    return(dst)
  }

  min_diff <- .get_min_dist(dst, only_upper_tri = is.null(y))

  min_diff
}

#' Subset colors based on pairwise differences or color properties
#'
#' @param colors Vector of colors
#' @param ... Subsetting specification, colors can be subsetted based on
#' minimum pairwise differences or color properties, refer to [get_property()].
#' To subset based color differences, use the `difference` variable,
#' e.g. `difference > 10`.
#' To subset based on color properties, refer to them by name,
#' e.g. `lightness > 50`.
#' @param method Method to use for comparing colors, refer to [compare_colors()]
#' @param filter Filter to apply to color palette when
#' calculating pairwise differences, refer to [compare_colors()]
#' @export
subset_colors <- function(colors, ..., method = "CIE2000", filter = NULL) {

  # Calculate pairwise differences for colors
  dst <- compare_colors(
    colors,
    method     = method,
    filter     = filter,
    return_mat = TRUE
  )

  # Calculate minimum difference for each color
  r_vals <- purrr::map_dbl(
    seq_len(nrow(dst) - 1),
    ~ min(dst[.x, (.x + 1):ncol(dst)])
  )

  r_vals <- c(r_vals, NA)

  c_vals <- purrr::map_dbl(
    seq_len(ncol(dst))[-1],
    ~ min(dst[1:(.x - 1), .x])
  )

  c_vals <- c(NA, c_vals)

  min_diff <- pmin(r_vals, c_vals, na.rm = TRUE)

  # Get color properties
  props <- get_property(colors, property = .properties)

  props$difference <- min_diff

  # Subset colors
  res <- subset(props, ...)
  res <- res$color

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
