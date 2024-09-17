
dat <- ggplot2::diamonds

clrs <- c(
  "#52271C", "#8A4334", "#B37A39", "#9A823A",
  "#6D6C38", "#445638", "#144443", "#022A2A"
)

test_that("check difference 1", {
  new_clrs <- clrs |>
    spruce_up_colors(difference = 20)

  expect_true(!identical(new_clrs, clrs))
  expect_equal(length(new_clrs), length(clrs))
  .test_diff(new_clrs, 20)
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
  .fn     = spruce_up_colors,
  desc    = "spruce_colors args",
  chk     = rlang::expr(expect_true(is.character(.res)))
)
