
dat <- ggplot2::diamonds

clrs <- c(
  "#52271C", "#8A4334", "#B37A39", "#9A823A",
  "#6D6C38", "#445638", "#144443", "#022A2A"
)

test_that("check difference 1", {
  new_clrs <- clrs |>
    spruce_colors(difference = 10)

  sim <- .get_sim_clrs(clrs, diff = 10)

  expect_true(!identical(new_clrs[sim], clrs[sim]))
  expect_true(identical(new_clrs[-sim], clrs[-sim]))
  expect_equal(length(new_clrs), length(clrs))
  .test_diff(new_clrs, 10)
})

arg_lst <- list(
  colors         = list(clrs),
  difference     = 10,
  property       = list("a", c("lightness", "hue")),
  method         = c("CIE2000", "euclidean"),
  range          = list(NULL, c(20, 70)),
  filter         = list(NULL, c("deutan", "protan")),
  adjust_colors  = list(NULL, 1),
  exclude_colors = list(NULL, 2),
  maxit          = 500
)

.test_all_args(
  arg_lst = arg_lst,
  .fn     = spruce_colors,
  desc    = "spruce_colors args",
  chk     = rlang::expr(expect_true(is.character(.res)))
)

# Identical colors
test_that("identical colors", {
  idnt_clrs <- rep("#FF5C37", 5)

  new_clrs <- idnt_clrs |>
    spruce_colors(
      difference = 1,
      property = "lightness"
    )

  .basic_clr_check(idnt_clrs, new_clrs)
  expect_true(all(new_clrs != idnt_clrs[1]))

  expect_error(
    idnt_clrs |>
      spruce_colors(
        difference = 1,
        property = "interp"
      )
  )
})

# Distinct colors
test_that("distinct colors", {
  new_clrs <- clrs |>
    spruce_colors(difference = 0)

  expect_identical(new_clrs, clrs)
})


# RANGE ----

# Named range
test_that("check range 1", {
  rng <- c(0, 20)

  new_clrs <- clrs |>
    spruce_colors(
      difference = 10,
      property   = "lightness",
      range      = list(lightness = rng)
    )

  sim <- .get_sim_clrs(clrs, diff = 10)

  lt <- new_clrs |>
    get_property("lightness")

  lt <- range(lt[sim, 2])

  .basic_clr_check(clrs, new_clrs)
  expect_true(all(lt < rng[2] & lt > rng[1]))
  expect_identical(new_clrs[-sim], clrs[-sim])
})

# Vector range
test_that("check range 2", {
  rng <- c(0, 20)

  new_clrs <- clrs |>
    spruce_colors(
      difference = 10,
      property   = c("hue", "lightness"),
      range      = rng
    )

  sim <- .get_sim_clrs(clrs, diff = 10)

  lt <- new_clrs |>
    get_property("hue")

  lt <- range(lt[sim, 2])

  .basic_clr_check(clrs, new_clrs)
  expect_true(all(lt < rng[2] & lt > rng[1]))
  expect_identical(new_clrs[-sim], clrs[-sim])
})

# Errors
test_that("check range errors", {
  expect_error(
    clrs |>
      spruce_colors(
        difference = 10,
        property   = c("hue", "lightness"),
        range      = list(c(0, 20))
      )
  )

  expect_error(
    clrs |>
      spruce_colors(
        difference = 10,
        property   = c("hue", "lightness"),
        range      = c(0, 20, 5)
      )
  )

  expect_error(
    clrs |>
      spruce_colors(
        difference = 10,
        property   = c("hue", "lightness"),
        range      = list(0, 20)
      )
  )
})
