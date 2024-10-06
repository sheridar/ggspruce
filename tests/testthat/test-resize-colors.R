
# resize_colors ----

test_that("resize_colors", {
  res <- clrs |>
    resize_colors(n = 2)

  expect_true(is.character(res))
  expect_true(length(res) == 2)

  res <- clrs |>
    resize_colors(n = 10)

  expect_true(is.character(res))
  expect_true(length(res) == 10)

  expect_identical(clrs, resize_colors(clrs, length(clrs)))
})


# interp_colors ----

test_that("interp_colors", {
  # output length
  res <- clrs |>
    interp_colors(n = 10, keep_original = TRUE)

  expect_true(sum(res %in% clrs) == length(clrs))

  res <- clrs |>
    interp_colors(n = 10, keep_original = FALSE)

  expect_true(sum(res %in% clrs) == 2)

  res <- clrs |>
    interp_colors(n = 2)

  expect_identical(clrs, res)

})

test_that("interp_colors order", {
  res <- clrs |>
    interp_colors(n = 10, keep_original = TRUE, order = FALSE)

  expect_identical(res[seq_len(length(clrs))], clrs)
  expect_true(all(!res[(length(clrs) + 1):length(res)] %in% clrs))
})

test_that("interp_colors before", {
  res <- clrs |>
    interp_colors(n = 10, before = c(2, 3))

  expect_identical(clrs, res[-c(2, 4)])

  res <- clrs |>
    interp_colors(n = 10, after = c(5, 6))

  expect_identical(clrs, res[-c(6, 8)])

  res <- clrs |>
    interp_colors(n = 10, before = 2, after = 5)

  expect_identical(clrs, res[-c(2, 7)])

  expect_error(interp_colors(clrs, n = 10, before = 1))
})

test_that("interp_colors errors", {
  expect_error(interp_colors("red", n = 5))
  expect_error(interp_colors(rep("red", 5), n = 10))
})


# collapse_colors ----

test_that("collapse_colors", {
  res <- clrs |>
    collapse_colors(n = 5, difference = 15)

  expect_true(length(res) == 5)
  expect_true(all(res %in% clrs))
})
