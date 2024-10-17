
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

# adjust_colors
test_that("adjust_colors", {
  new_clrs <- clrs |>
    spruce_colors(difference = 15, adjust_colors = 2)

  .basic_clr_check(clrs[-2], new_clrs[-2])
  expect_true(clrs[2] != new_clrs[2])

  .basic_clr_check(clrs[-2], new_clrs[-2])
  expect_true(clrs[2] != new_clrs[2])

  ex <- clrs |>
    subset_colors(difference < 10)

  ex <- match(ex[1], clrs)

  new_clrs <- clrs |>
    spruce_colors(difference = 10, exclude_colors = 7)

  expect_true(clrs[ex] == new_clrs[ex])
})

# Errors
test_that("spruce_colors errors", {
  expect_error(
    clrs |>
      spruce_colors(adjust_colors = 1, exclude_colors = 1),
    "different"
  )

  expect_error(
    clrs |>
      spruce_colors(adjust_colors = 100, exclude_colors = 1),
    "present in"
  )

  expect_error(
    setNames(clrs[1:3], LETTERS[1:3]) |>
      spruce_colors(adjust_colors = "A", exclude_colors = "E"),
    "present in"
  )

  expect_error(
    clrs |>
      spruce_colors(difference = c(1, 2)),
    "difference"
  )

  expect_error(
    clrs |>
      spruce_colors(difference = "10"),
    "difference"
  )

  expect_error(
    clrs |>
      spruce_colors(maxit = "10"),
    "maxit"
  )
})


# RANGE ----

# Named range
test_that("check range 1", {
  rng <- c(0, 10)

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

  rng[2] <- rng[2] + (rng[2] * 0.1) # final values change slightly when
                                    # converting between colorspaces
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
