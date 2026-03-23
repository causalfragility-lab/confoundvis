test_that("plot_robustness_curve returns a ggplot", {
  x <- new_confoundsens(lambda = seq(0, 0.2, 0.02),
                        theta  = seq(1, 0.8, length.out = 11),
                        se     = rep(0.05, 11))
  p <- plot_robustness_curve(x)
  expect_s3_class(p, "ggplot")
})

test_that("plot_robustness_curve works without se", {
  x <- new_confoundsens(lambda = seq(0, 0.2, 0.05),
                        theta  = seq(1, 0.9, length.out = 5))
  expect_s3_class(plot_robustness_curve(x, bands = FALSE), "ggplot")
})

test_that("plot_robustness_curve what='t' works", {
  x <- new_confoundsens(lambda = seq(0, 0.2, 0.05),
                        theta  = seq(1, 0.9, length.out = 5),
                        t      = seq(3, 1.5, length.out = 5))
  expect_s3_class(plot_robustness_curve(x, what = "t"), "ggplot")
})

test_that("plot_robustness_curve errors when t missing but requested", {
  x <- new_confoundsens(lambda = 1:5, theta = 5:1)
  expect_error(plot_robustness_curve(x, what = "t"), "missing or all NA")
})

test_that("plot_robustness_curve errors on non-confoundsens input", {
  expect_error(plot_robustness_curve(list()), "class 'confoundsens'")
})
