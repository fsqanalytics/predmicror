test_that("WeibullMM returns finite, monotone values with a residual tail", {
  time <- seq(0, 80, length.out = 41)
  y <- WeibullMM(time, Y0 = 6, Yres = 1.5, sigma = 8, alpha = 1.3)

  expect_length(y, length(time))
  expect_true(all(is.finite(y)))
  expect_equal(y[1], 6, tolerance = 1e-12)
  expect_true(all(diff(y) <= 1e-10))
  expect_gt(min(y), 1.5 - 1e-10)
  expect_lt(abs(tail(y, 1) - 1.5), 1e-6)
})

test_that("WeibullMM approaches its no-residual limit when Yres is negligible", {
  time <- seq(0, 20, length.out = 21)
  Y0 <- 5.6
  Yres <- -30
  sigma <- 12.5
  alpha <- 1.1

  y_modified <- WeibullMM(time, Y0 = Y0, Yres = Yres, sigma = sigma, alpha = alpha)

  # From the WeibullMM implementation:
  # log10((10^Y0 - 10^Yres) * 10^(-(time / sigma)^alpha) + 10^Yres).
  # When 10^Yres is negligible, this tends to Y0 - (time / sigma)^alpha.
  y_limit <- Y0 - (time / sigma)^alpha

  expect_equal(y_modified, y_limit, tolerance = 1e-8)
})

test_that("WeibullMM can be fitted when the residual tail is identifiable or fixed", {
  skip_if_not_installed("gslnls")

  time <- seq(0, 80, length.out = 41)
  dat <- data.frame(
    Time = time,
    logN = WeibullMM(time, Y0 = 6, Yres = 1.5, sigma = 8, alpha = 1.3) +
      0.002 * sin(time / 3)
  )

  fit <- gslnls::gsl_nls(
    logN ~ WeibullMM(Time, Y0, Yres = 1.5, sigma, alpha),
    data = dat,
    start = list(Y0 = 5.9, sigma = 8.5, alpha = 1.2)
  )

  estimated_params <- coef(fit)

  expect_true(all(is.finite(estimated_params)))
  expect_equal(unname(estimated_params["Y0"]), 6, tolerance = 0.05)
  expect_equal(unname(estimated_params["sigma"]), 8, tolerance = 0.5)
  expect_equal(unname(estimated_params["alpha"]), 1.3, tolerance = 0.1)
})
