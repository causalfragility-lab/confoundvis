test_that("as_confoundsens converts data frame", {
  df <- data.frame(lambda = 1:5, theta = 5:1)
  x  <- as_confoundsens(df)
  expect_s3_class(x, "confoundsens")
  expect_equal(x$lambda, 1:5)
  expect_equal(x$theta,  5:1)
})

test_that("as_confoundsens handles optional columns", {
  df <- data.frame(lam = 1:4, th = 4:1, se = rep(0.1, 4),
                   lev = c("w","w","b","b"))
  x  <- as_confoundsens(df, lambda = "lam", theta = "th",
                        se = "se", level = "lev")
  expect_equal(x$se,    rep(0.1, 4))
  expect_equal(x$level, c("w", "w", "b", "b"))
})

test_that("as_confoundsens errors on missing column", {
  df <- data.frame(lambda = 1:3, theta = 3:1)
  expect_error(as_confoundsens(df, lambda = "nope"), "not found")
})

test_that("as_confoundsens errors on non-data.frame", {
  expect_error(as_confoundsens(list(lambda = 1, theta = 2)), "must be a data.frame")
})
