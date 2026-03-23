test_that("plot_sensitivity_love returns a ggplot", {
  df <- data.frame(covariate = c("SES","BMI","Age"),
                   impact    = c(0.12, 0.05, 0.03))
  p  <- plot_sensitivity_love(df, threshold = 0.10)
  expect_s3_class(p, "ggplot")
})

test_that("plot_sensitivity_love top argument subsets correctly", {
  df <- data.frame(covariate = letters[1:6],
                   impact    = c(0.01, 0.05, 0.12, 0.03, 0.08, 0.15))
  p  <- plot_sensitivity_love(df, threshold = 0.10, top = 3)
  expect_equal(nrow(p$data), 3L)
})

test_that("plot_sensitivity_love errors on missing columns", {
  df <- data.frame(name = "SES", val = 0.1)
  expect_error(plot_sensitivity_love(df, threshold = 0.05),
               "columns `covariate` and `impact`")
})

test_that("plot_sensitivity_love errors on non-numeric impact", {
  df <- data.frame(covariate = "SES", impact = "high")
  expect_error(plot_sensitivity_love(df, threshold = 0.05),
               "must be numeric")
})

test_that("plot_sensitivity_love errors on NA threshold", {
  df <- data.frame(covariate = "SES", impact = 0.1)
  expect_error(plot_sensitivity_love(df, threshold = NA), "non-missing")
})
