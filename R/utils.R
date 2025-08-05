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
#' @importFrom rlang %||% .data
#' @aliases %||% .data
#' @export %||% .data
NULL

#' ggplot2 exports
#'
#' @name ggplot_exports
#' @importFrom ggplot2 element_grob merge_element
#' @aliases element_grob merge_element
#' @export element_grob merge_element
NULL

#' Lift the domain of a function
#'
#' To replace `purrr::lift_dl()`, which is deprecated in purrr v1.0.0
#'
#' @param ..f A function to lift.
#' @param ... Default arguments for `..f`. These will be
#' evaluated only once, when the lifting factory is called.
#' @param .unnamed This prevents matching of arguments by name, arguments will
#' be be matched by position instead.
#' @return A function.
#' @noRd
.lift <- function(..f, ..., .unnamed = FALSE) {
  force(..f)

  defaults <- list(...)

  function(.x = list(), ...) {
    if (.unnamed) .x <- unname(.x)

    do.call("..f", c(.x, defaults, list(...)))
  }
}

#' Encode colors as hex code
.chk_colors <- function(colors) {
  if (is.null(colors)) return(NULL)
  
  if (!rlang::is_character(colors) || !rlang::has_length(colors)) {
    cli::cli_abort("`colors` must be a character vector.")
  }
  
  colors <- rgb(t(col2rgb(colors)) / 255)

  colors
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

descent_cache <- new.env(parent = emptyenv())

#' Important: This function is not vectorized. Do not use to look up multiple
#' font descents at once.
#' @importFrom grid grobDescent
font_descent <- function(family = "", face = "plain", size = 12, cex = 1) {
  cur_dev <- names(grDevices::dev.cur())

  if (cur_dev == "null device") {
    cache <- FALSE   # don't cache if no device open
  } else {
    cache <- TRUE
  }
  key <- paste0(cur_dev, ':', family, ':', face, ":", size, ":", cex)
  # we only look up the first result; this function is not vectorized
  key <- key[1]

  descent <- descent_cache[[key]]

  if (is.null(descent)) {
    descent <- grid::convertHeight(grid::grobDescent(grid::textGrob(
      label = "gjpqyQ",
      gp = grid::gpar(
        fontsize = size,
        cex = cex,
        fontfamily = family,
        fontface = face
      )
    )), 'inches')

    if (cache) {
      descent_cache[[key]] <- descent
    }
  }

  descent
}
