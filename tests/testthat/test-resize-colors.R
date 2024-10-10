
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

test_that("collapse_colors duplicates", {
  dups  <- c("yellow", rep("blue", 3), "purple", "yellow")
  answr <- c("yellow", "blue", "blue", "purple", "yellow")

  res <- dups |>
    collapse_colors(n = 5, difference = 15)

  expect_true(length(res) == 5)
  expect_identical(res, answr)

  res <- c("yellow", rep("blue", 3), "purple", "yellow") |>
    collapse_colors(n = 3, difference = 15)

  expect_true(length(res) == 3)
  expect_identical(res, c("yellow", "blue", "purple"))
})

# expand_colors ----

test_that("expand_colors", {
  res <- clrs |>
    expand_colors(
      n = c(rep(1, 4), 3, rep(1, 3)),
      keep_original = TRUE
    )

  expect_true(length(res) == 10)
  expect_identical(clrs, res[-c(6:7)])
  expect_true(all(clrs %in% res))

  res <- clrs |>
    expand_colors(
      n = c(rep(1, 4), 3, rep(1, 3)),
      keep_original = FALSE
    )

  expect_true(length(res) == 10)
  expect_true(!all(clrs %in% res))

  res <- clrs |>
    expand_colors(
      names = list("A", "B", "C", "D", c("E", "F", "G", "H"), "I", "J", "K"),
      keep_original = TRUE
    )

  expect_identical(names(res), LETTERS[1:11])
  expect_true(length(res) == 11)
  expect_identical(clrs, unname(res[-c(6:8)]))
  expect_true(all(clrs %in% res))
})

test_that("expand_colors range", {
  res <- clrs |>
    expand_colors(
      n = c(rep(1, 4), 3, rep(1, 3)),
      keep_original = TRUE,
      range = c(10, 15)
    )
})

test_that("expand_colors direction", {
  res <- clrs |>
    expand_colors(
      n = c(rep(1, 4), 3, rep(1, 3)),
      keep_original = TRUE,
      direction = 1
    )

  lt <- res[c(5, 6, 7)] |>
    get_property("lightness")

  lt <- lt$lightness

  expect_true(length(res) == 10)
  expect_identical(clrs, res[-c(6:7)])
  expect_true(all(clrs %in% res))
  expect_true(all(lt[2:3] > lt[1]))

  res <- clrs |>
    expand_colors(
      n = c(rep(1, 4), 3, rep(1, 3)),
      keep_original = TRUE,
      direction = -1
    )

  lt <- res[c(5, 6, 7)] |>
    get_property("lightness")

  lt <- lt$lightness

  expect_true(length(res) == 10)
  expect_identical(clrs, res[-c(5:6)])
  expect_true(all(clrs %in% res))
  expect_true(all(lt[1:2] < lt[3]))
})

test_that("assign_colors", {
  res <- clrs |>
    assign_colors(
      names = c("A", "B", "C")
    )

  expect_identical(unname(res), collapse_colors(clrs, 3))

  res <- clrs[1:3] |>
    assign_colors(
      names = c("A", "B", "C", "D"),
      difference = 5
    )

  res2 <- clrs[1:3] |>
    interp_colors(4, order = TRUE)

  ex_idx <- match(clrs[1:3], res2)

  res2 <- spruce_colors(
    res2,
    property       = "interp",
    difference     = 5,
    exclude_colors = ex_idx,
    order          = TRUE
  )

  expect_identical(unname(res), res2)

  res <- clrs[1:3] |>
    assign_colors(
      names = c("A", "B", "C", "D", "E", "F"),
      difference = 5,
      order = FALSE
    )

  expect_identical(unname(res[1:3]), clrs[1:3])
})

test_that("sort_colors", {
  res <- clrs |>
    sort_colors(property = "hue")

  hues <- clrs |>
    farver::decode_colour(to = "hsl")

  expect_identical(res, clrs[order(hues[, 1])])

  res <- clrs |>
    sort_colors(property = "lightness", desc = TRUE)

  lt <- clrs |>
    farver::decode_colour(to = "lab")

  expect_identical(res, clrs[order(lt[, 1], decreasing = TRUE)])
})
