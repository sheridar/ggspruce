
test_that("compare_colors", {
  diff <- clrs |>
    farver::decode_colour(to = "lab") |>
    farver::compare_colour(from_space = "lab", method = "CIE2000")

  diff <- min(diff[upper.tri(diff)])

  expect_identical(min(compare_colors(clrs)), diff)
})

test_that("subset_colors 1", {
  sim <- .get_sim_clrs(clrs, diff = 10)
  sim <- clrs[sim]

  expect_identical(sim, subset_colors(clrs, difference < 10))
})

test_that("subset_colors 2", {
  lt <- clrs |>
    farver::decode_colour(to = "lab")

  lt <- lt[, 1]
  lt <- which(lt > 10 & lt < 30)
  lt <- clrs[lt]

  expect_identical(lt, subset_colors(clrs, lightness > 10 & lightness < 30))
})

test_that("plot_colors", {
  plt <- plot_colors(clrs)

  expect_s3_class(plt, "ggplot")

  plt <- clrs |>
    plot_colors(filter = "deutan")

  expect_s3_class(plt, "ggplot")

  plt <- clrs |>
    plot_colors(
      filter = "deutan",
      label_color = "blue",
      linewidth = 3
    )

  expect_s3_class(plt, "ggplot")
  expect_identical(plt$layers[[1]]$aes_params$linewidth, 3)
})
