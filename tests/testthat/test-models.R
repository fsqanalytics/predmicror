test_that("growth model helpers return expected values", {
  expect_equal(BaranyiRM(0, Y0 = 1, MUmax = 0.5, lag = 2), 1)
  expect_equal(
    BuchananRM(c(1, 3), Y0 = 1, MUmax = 2, lag = 2),
    c(1, 1 + 2 * (3 - 2))
  )

  t_vals <- c(2, 5)
  y0 <- 1
  ymax <- 5
  mu <- 0.4
  lag <- 3
  expected <- c(
    y0,
    ymax - log1p(expm1(ymax - y0) * exp(-mu * (t_vals[2] - lag)))
  )
  expect_equal(RossoFM(t_vals, Y0 = y0, Ymax = ymax, MUmax = mu, lag = lag), expected)

  t <- c(0, 2, 4)
  expect_equal(
    HuangNLM(t, Y0 = 0.2, Ymax = 3, MUmax = 0.6),
    FangNLM(t, Y0 = 0.2, Ymax = 3, MUmax = 0.6)
  )

  expect_true(
    abs(RichardsNLM(3, Y0 = 0, Ymax = 4, MUmax = 0.8, m = 2) -
      RichardsNLM(3, Y0 = 0, Ymax = 4, MUmax = 0.8, m = 1)) > 1e-6
  )
})

test_that("cardinal models return 0 outside their ranges", {
  expect_equal(CMTI(c(0, 100), Tmax = 40, Tmin = 5, MUopt = 1, Topt = 30), c(0, 0))
  expect_equal(CMAW(c(0.7, 1.1), AWmin = 0.8, MUopt = 1, AWopt = 0.95), c(0, 0))
  expect_equal(CMPH(c(2, 10), pHmax = 9, pHmin = 3, MUopt = 1, pHopt = 7), c(0, 0))
  expect_equal(CMInh(c(6, 10), MIC = 5, MUopt = 1, alpha = 1), c(0, 0))
})

test_that("inactivation models are anchored at t = 0", {
  expect_equal(WeibullPH(0, Y0 = 3, k = 0.2, alpha = 1.5), 3)
  expect_equal(WeibullM(0, Y0 = 3, sigma = 2, alpha = 1.5), 3)
  expect_equal(WeibullMM(0, Y0 = 3, Yres = 1, sigma = 2, alpha = 1.5), 3)
  expect_equal(GeeraerdST(0, Y0 = 3, Yres = 1, kmax = 0.2, Sl = 1), 3)
})
