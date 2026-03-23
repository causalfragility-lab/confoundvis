test_that("simulate_taylor_demo returns three confoundsens objects", {
  sims <- simulate_taylor_demo(delta_max = 1, step = 0.1,
                               theta0 = 0.4, slope = -0.7, kappa = 0.4)
  expect_named(sims, c("linear", "concave", "convex"))
  expect_s3_class(sims$linear,  "confoundsens")
  expect_s3_class(sims$concave, "confoundsens")
  expect_s3_class(sims$convex,  "confoundsens")
})

test_that("simulate_taylor_demo linear path is exact", {
  sims <- simulate_taylor_demo(delta_max = 1, step = 0.25,
                               theta0 = 0.5, slope = -0.6, kappa = 0.3)
  d  <- sims$linear$lambda
  th <- sims$linear$theta
  expect_equal(th, 0.5 - 0.6 * d, tolerance = 1e-12)
})

test_that("simulate_taylor_demo concave < linear at delta > 0", {
  sims <- simulate_taylor_demo(delta_max = 1, step = 0.1,
                               theta0 = 0.4, slope = -0.1, kappa = 0.5)
  idx  <- sims$linear$lambda > 0
  expect_true(all(sims$concave$theta[idx] < sims$linear$theta[idx]))
})

test_that("simulate_taylor_demo convex > linear at delta > 0", {
  sims <- simulate_taylor_demo(delta_max = 1, step = 0.1,
                               theta0 = 0.4, slope = -0.1, kappa = 0.5)
  idx  <- sims$linear$lambda > 0
  expect_true(all(sims$convex$theta[idx] > sims$linear$theta[idx]))
})

test_that("simulate_taylor_demo endpoint equals delta_max", {
  sims <- simulate_taylor_demo(delta_max = 0.7, step = 0.1)
  expect_equal(tail(sims$linear$lambda, 1), 0.7)
})

test_that("simulate_taylor_demo errors on bad inputs", {
  expect_error(simulate_taylor_demo(delta_max = -1), "positive")
  expect_error(simulate_taylor_demo(kappa = -0.1),   "non-negative")
})
