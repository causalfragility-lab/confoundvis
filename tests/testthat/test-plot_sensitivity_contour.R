test_that("plot_sensitivity_contour returns a ggplot", {
  p <- plot_sensitivity_contour(threshold = 0.02)
  expect_s3_class(p, "ggplot")
})

test_that("plot_sensitivity_contour with benchmarks returns ggplot", {
  b <- data.frame(r_yu = c(0.1, 0.2), r_du = c(0.15, 0.08),
                  label = c("SES", "BMI"))
  p <- plot_sensitivity_contour(threshold = 0.02, benchmarks = b)
  expect_s3_class(p, "ggplot")
})

test_that("plot_sensitivity_contour errors on non-positive threshold", {
  expect_error(plot_sensitivity_contour(threshold = -0.01), "positive")
  expect_error(plot_sensitivity_contour(threshold = 0),     "positive")
})

test_that("plot_sensitivity_contour errors on bad benchmarks", {
  expect_error(
    plot_sensitivity_contour(threshold = 0.02,
                             benchmarks = data.frame(x = 1)),
    "columns `r_yu` and `r_du`"
  )
})

test_that("plot_sensitivity_contour boundary has four groups", {
  p    <- plot_sensitivity_contour(threshold = 0.05)
  bdat <- p$data
  expect_true("group" %in% names(bdat))
  expect_equal(length(unique(bdat$group)), 4L)
})
