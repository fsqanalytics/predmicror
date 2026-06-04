test_that("bias and accuracy factors validate inputs", {
  expect_equal(round(bias_factor(c(7, 6, 5), c(7.1, 5.9, 5.2)), 6), 1.012274)
  expect_equal(round(accuracy_factor(c(7, 6, 5), c(7.1, 5.9, 5.2)), 5), 1.02368)

  expect_error(bias_factor(c(1, 2), c(1)), "same length")
  expect_error(bias_factor(c(1, 0), c(1, 1)), "positive")
})

test_that("fit_omnibus_inactivation uses registered primary models", {
  set.seed(1)

  dat <- do.call(rbind, lapply(1:4, function(g) {
    Time <- c(1, 2, 4, 6, 8, 10)
    sigma <- 5 + 0.4 * g
    data.frame(
      Condition = g,
      Time = Time,
      Temp = 55 + g,
      logN = WeibullM(Time, Y0 = 7, sigma = sigma, alpha = 1.1) + rnorm(length(Time), 0, 0.03)
    )
  }))

  fit <- fit_omnibus_inactivation(
    data = dat,
    primary = "WeibullM",
    time = "Time",
    response = "logN",
    group = "Condition",
    secondary = list(
      sigma = ~ Temp
    ),
    random = Y0 ~ 1,
    start = c(
      Y0 = 7,
      sigma = 1,
      sigma.Temp = 0.08,
      alpha = 1
    )
  )

  expect_s3_class(fit, "predmicror_omnibus_fit")
  expect_equal(fit$type, "inactivation")
  expect_equal(fit$primary, "WeibullM")
  expect_equal(fit$parameters, c("Y0", "sigma", "alpha"))
  expect_true(inherits(fit$secondary$sigma, "formula"))
  expect_match(deparse(fit$secondary$sigma), "sigma ~ Temp", fixed = TRUE)

  aug <- predmicror_augment(fit)
  expect_true(all(c(".fitted", ".resid", ".model", ".type") %in% names(aug)))
  expect_equal(unique(aug$.model), "WeibullM")
  expect_equal(unique(aug$.type), "omnibus_inactivation")
  expect_lt(fit_metrics(fit)$RMSE, 0.05)
})

test_that("fit_omnibus_growth uses registered primary models", {
  set.seed(1)

  dat <- do.call(rbind, lapply(1:4, function(g) {
    Time <- c(0, 1, 2, 3, 4, 5, 6)
    mumax <- 0.6 + 0.04 * g
    data.frame(
      Condition = g,
      Time = Time,
      Temp = 20 + g,
      lnN = HuangNLM(Time, Y0 = 2, Ymax = 8, MUmax = mumax) + rnorm(length(Time), 0, 0.02)
    )
  }))

  fit <- fit_omnibus_growth(
    data = dat,
    primary = "HuangNLM",
    time = "Time",
    response = "lnN",
    group = "Condition",
    secondary = list(
      MUmax = ~ Temp
    ),
    random = Y0 ~ 1,
    start = c(
      Y0 = 2,
      Ymax = 8,
      MUmax = 0.1,
      MUmax.Temp = 0.025
    )
  )

  expect_s3_class(fit, "predmicror_omnibus_fit")
  expect_equal(fit$type, "growth")
  expect_equal(fit$primary, "HuangNLM")
  expect_equal(fit$parameters, c("Y0", "Ymax", "MUmax"))
  expect_match(deparse(fit$secondary$MUmax), "MUmax ~ Temp", fixed = TRUE)

  aug <- predmicror_augment(fit)
  expect_true(all(c(".fitted", ".resid", ".model", ".type") %in% names(aug)))
  expect_equal(unique(aug$.model), "HuangNLM")
  expect_equal(unique(aug$.type), "omnibus_growth")
  expect_lt(fit_metrics(fit)$RMSE, 0.04)
})
