test_that("fit_local_quadratic returns expected structure", {
  df  <- data.frame(delta = seq(0, 0.3, length.out = 60),
                    theta = 0.4 - 0.7*seq(0,0.3,length.out=60) +
                            0.4*seq(0,0.3,length.out=60)^2)
  fit <- fit_local_quadratic(df, local_max_delta = 0.2)
  expect_type(fit, "list")
  expect_named(fit, c("coef","intercept","slope","quad","model",
                       "local_data","local_max_delta"))
  expect_s3_class(fit$model, "lm")
})

test_that("fit_local_quadratic recovers known coefficients", {
  d   <- seq(0, 0.5, length.out = 200)
  th  <- 0.4 - 0.7*d + 0.4*d^2
  fit <- fit_local_quadratic(delta = d, theta = th, local_max_delta = 0.5)
  expect_equal(fit$intercept, 0.4,  tolerance = 1e-6)
  expect_equal(fit$slope,    -0.7,  tolerance = 1e-6)
  expect_equal(fit$quad,      0.4,  tolerance = 1e-6)
})

test_that("fit_local_quadratic no-intercept model works", {
  d   <- seq(0.01, 0.2, length.out = 40)
  th  <- -0.7*d + 0.4*d^2
  fit <- fit_local_quadratic(delta = d, theta = th,
                             local_max_delta = 0.2,
                             include_intercept = FALSE)
  expect_true(is.na(fit$intercept))
  expect_false(is.na(fit$slope))
})

test_that("fit_local_quadratic errors when window empty", {
  expect_error(
    fit_local_quadratic(delta = c(0.5, 0.6, 0.7), theta = 1:3,
                        local_max_delta = 0.1),
    "No observations"
  )
})

test_that("fit_local_quadratic errors on bad local_max_delta", {
  expect_error(
    fit_local_quadratic(delta = 1:5, theta = 5:1, local_max_delta = -1),
    "positive"
  )
})
