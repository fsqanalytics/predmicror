test_that("dynamic profiles validate and sort input", {
  profile <- dynamic_profile(
    time = c(10, 0, 5),
    temperature = c(15, 4, 10)
  )

  expect_s3_class(profile, "predmicror_dynamic_profile")
  expect_equal(profile$time, c(0, 5, 10))
  expect_equal(profile$temperature, c(4, 10, 15))
})

test_that("dynamic Huang growth with constant rate increases population", {
  profile <- dynamic_profile(
    time = c(0, 10),
    temperature = c(20, 20)
  )

  pred <- predict_dynamic_growth(
    profile = profile,
    secondary = "constant",
    start = list(logN0 = 2, logNmax = 8, MUmax = 0.6, lag = 0),
    times = seq(0, 10, by = 1),
    dt = 0.1
  )

  expect_s3_class(pred, "predmicror_dynamic_prediction")
  expect_equal(pred$time[1], 0)
  expect_equal(pred$logN[1], 2, tolerance = 1e-8)
  expect_gt(pred$logN[nrow(pred)], pred$logN[1])
  expect_lte(max(pred$logN), 8.01)
})

test_that("dynamic Huang square-root model responds to temperature", {
  cold <- dynamic_profile(time = c(0, 10), temperature = c(4, 4))
  warm <- dynamic_profile(time = c(0, 10), temperature = c(20, 20))

  start <- list(logN0 = 2, logNmax = 8, a = 0.08, Tmin = 7, lag = 0)
  pred_cold <- predict_dynamic_growth(cold, start = start, times = 0:10, dt = 0.1)
  pred_warm <- predict_dynamic_growth(warm, start = start, times = 0:10, dt = 0.1)

  expect_equal(pred_cold$logN[nrow(pred_cold)], 2, tolerance = 1e-4)
  expect_gt(pred_warm$logN[nrow(pred_warm)], pred_cold$logN[nrow(pred_cold)])
})

test_that("dynamic Weibull-Peleg inactivation matches constant linear case", {
  profile <- dynamic_profile(time = c(0, 10), temperature = c(60, 60))

  pred <- predict_dynamic_inactivation(
    profile = profile,
    start = list(logN0 = 7, b = 0.2, n = 1),
    times = c(0, 5, 10),
    dt = 0.05
  )

  expect_equal(pred$logN, c(7, 6, 5), tolerance = 1e-3)
  expect_equal(pred$log_survival, c(0, -1, -2), tolerance = 1e-3)
})

test_that("dynamic inactivation z-value secondary increases loss at higher temperature", {
  low <- dynamic_profile(time = c(0, 10), temperature = c(55, 55))
  high <- dynamic_profile(time = c(0, 10), temperature = c(65, 65))
  start <- list(logN0 = 7, b_ref = 0.1, T_ref = 55, z = 10, n = 1)

  pred_low <- predict_dynamic_inactivation(low, secondary = "z_value", start = start, times = c(0, 10), dt = 0.1)
  pred_high <- predict_dynamic_inactivation(high, secondary = "z_value", start = start, times = c(0, 10), dt = 0.1)

  expect_lt(pred_high$logN[2], pred_low$logN[2])
})

test_that("dynamic sensitivity returns scaled coefficients", {
  profile <- dynamic_profile(time = c(0, 5), temperature = c(20, 20))
  sens <- dynamic_sensitivity(
    "growth",
    profile = profile,
    start = list(logN0 = 2, logNmax = 8, MUmax = 0.5, lag = 0),
    parameters = "MUmax",
    secondary = "constant",
    times = c(0, 2.5, 5),
    dt = 0.1
  )

  expect_true(all(c("time", "parameter", "scaled_sensitivity") %in% names(sens)))
  expect_equal(unique(sens$parameter), "MUmax")
  expect_equal(nrow(sens), 3)
})

test_that("fit_dynamic_growth estimates selected parameters", {
  profile <- dynamic_profile(time = c(0, 10), temperature = c(20, 20))
  truth <- list(logN0 = 2, logNmax = 8, MUmax = 0.45, lag = 0)
  obs_pred <- predict_dynamic_growth(
    profile,
    secondary = "constant",
    start = truth,
    times = c(0, 4, 8, 10),
    dt = 0.1
  )
  obs <- data.frame(time = obs_pred$time, logN = obs_pred$logN)

  fit <- fit_dynamic_growth(
    obs,
    profile = profile,
    time = "time",
    response = "logN",
    start = list(logN0 = 2, logNmax = 8, MUmax = 0.25, lag = 0),
    estimate = "MUmax",
    secondary = "constant",
    dt = 0.1
  )

  expect_s3_class(fit, "predmicror_dynamic_fit")
  expect_equal(unname(coef(fit)["MUmax"]), 0.45, tolerance = 0.02)
  expect_equal(length(fitted(fit)), nrow(obs))
  expect_lt(fit_metrics(fit)$RMSE, 0.02)
  expect_true(all(c(".fitted", ".resid", ".model", ".type") %in% names(predmicror_augment(fit))))
})

test_that("fit_dynamic_inactivation estimates selected parameters", {
  profile <- dynamic_profile(time = c(0, 10), temperature = c(60, 60))
  obs <- data.frame(time = c(0, 5, 10), logN = c(7, 6, 5))

  fit <- fit_dynamic_inactivation(
    obs,
    profile = profile,
    time = "time",
    response = "logN",
    start = list(logN0 = 7, b = 0.1, n = 1),
    estimate = "b",
    dt = 0.05
  )

  expect_s3_class(fit, "predmicror_dynamic_fit")
  expect_equal(unname(coef(fit)["b"]), 0.2, tolerance = 0.01)
  expect_equal(residuals(fit), rep(0, 3), tolerance = 0.02)
  pred <- predict(fit, times = c(0, 10))
  expect_s3_class(pred, "predmicror_dynamic_prediction")
  expect_equal(pred$logN, c(7, 5), tolerance = 0.02)
})

test_that("dynamic fitting accepts empty fixed list", {
  profile <- dynamic_profile(
    time = c(0, 5, 10),
    temperature = c(12, 12, 12)
  )

  truth <- predict_dynamic_growth(
    profile = profile,
    start = list(logN0 = 2, logNmax = 6, a = 0.08, Tmin = 6, lag = 1),
    times = seq(0, 10, by = 2)
  )

  obs <- data.frame(time = truth$time, logN = truth$response)

  fit <- fit_dynamic_growth(
    data = obs,
    profile = profile,
    time = "time",
    response = "logN",
    start = list(logN0 = 2, logNmax = 6, a = 0.08, Tmin = 6, lag = 1),
    fixed = list()
  )

  expect_s3_class(fit, "predmicror_dynamic_fit")
})
