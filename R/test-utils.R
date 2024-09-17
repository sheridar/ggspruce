
#' Helper to test all combinations of provided arguments
#'
#' @param arg_lst Named list of arguments to test
#' @param .fn Function to test
#' @param desc Description to pass to test_that
#' @param chk Function or expression to use for testing. If an expression is
#' passed, results from .fn can be referred to with .res.
#' @param dryrun Do not run tests, just return table of arguments that will be
#' tested
#' @return Output from test_that
#' @noRd
.test_all_args <- function(arg_lst, .fn, desc, chk, dryrun = FALSE) {
  arg_lst    <- expand.grid(arg_lst, stringsAsFactors = FALSE)
  arg_lst$.n <- seq_len(nrow(arg_lst))

  if (dryrun) {
    arg_lst <- tibble::as_tibble(arg_lst)

    return(arg_lst)
  }

  purrr::pwalk(arg_lst, ~ {
    test_args    <- list(...)
    n            <- test_args$.n
    test_args$.n <- NULL

    testthat::test_that(paste(desc, n), {
      if (is.call(chk)) {
        .res <- .lift(.fn)(test_args)

        return(eval(chk))
      }

      chk(.lift(.fn)(test_args))
    })
  })
}

.test_diff <- function(clrs, dif) {
  sim <- farver::decode_colour(clrs, to = "lab") |>
    farver::compare_colour(from_space = "lab")

  sim <- min(sim[upper.tri(sim)])

  testthat::expect_gt(sim, dif)
}
