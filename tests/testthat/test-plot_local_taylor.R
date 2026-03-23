make_taylor_df <- function() {
  d <- seq(0, 0.2, length.out = 20)
  data.frame(
    delta  = rep(d, 3),
    series = rep(c("path", "tangent", "quadratic"), each = 20),
    value  = c(0.4 - 0.7*d - 0.4*d^2,
               0.4 - 0.7*d,
               0.4 - 0.7*d + 0.2*d^2)
  )
}

test_that("plot_local_taylor returns a ggplot", {
  p <- plot_local_taylor(make_taylor_df())
  expect_s3_class(p, "ggplot")
})

test_that("plot_local_taylor maps colour to series", {
  p      <- plot_local_taylor(make_taylor_df())
  mapped <- rlang::as_label(p$mapping$colour)
  expect_true(grepl("series", mapped))
})

test_that("plot_local_taylor maps linetype to series", {
  p      <- plot_local_taylor(make_taylor_df())
  mapped <- rlang::as_label(p$mapping$linetype)
  expect_true(grepl("series", mapped))
})

test_that("plot_local_taylor facet=TRUE adds facets", {
  p <- plot_local_taylor(make_taylor_df(), facet = TRUE)
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_local_taylor errors on missing columns", {
  bad <- data.frame(x = 1, y = 2, z = 3)
  expect_error(plot_local_taylor(bad), "columns `delta`, `series`, and `value`")
})

test_that("plot_local_taylor errors on non-numeric delta", {
  df <- data.frame(delta = "a", series = "s", value = 1)
  expect_error(plot_local_taylor(df), "must be numeric")
})
