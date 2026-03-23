test_that("as.data.frame.confoundsens returns correct columns", {
  x  <- new_confoundsens(lambda = 1:5, theta = 5:1, se = rep(0.1, 5))
  df <- as.data.frame(x)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("lambda", "theta", "se"))
  expect_equal(nrow(df), 5L)
})

test_that("as.data.frame.confoundsens includes level when present", {
  x  <- new_confoundsens(lambda = 1:4, theta = 4:1,
                         level = c("w","w","b","b"))
  df <- as.data.frame(x)
  expect_true("level" %in% names(df))
})

test_that("as.data.frame round-trips through as_confoundsens", {
  x   <- new_confoundsens(lambda = seq(0, 1, length.out = 10),
                          theta  = seq(1, 0.5, length.out = 10))
  df  <- as.data.frame(x)
  x2  <- as_confoundsens(df)
  expect_equal(x$lambda, x2$lambda)
  expect_equal(x$theta,  x2$theta)
})
