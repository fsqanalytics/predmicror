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

test_that("omnibus_secondary allows registered secondary models inside primary parameters", {
  set.seed(2)

  dat <- do.call(rbind, lapply(1:5, function(g) {
    Time <- c(0, 1, 2, 3, 4, 5, 6)
    Temp <- 12 + 3 * g
    mu <- CMTI(Temp, Tmax = 45, Tmin = 5, MUopt = 0.9, Topt = 30)^2
    data.frame(
      Condition = g,
      Time = Time,
      Temp = Temp,
      lnN = HuangNLM(Time, Y0 = 2, Ymax = 8, MUmax = mu) + rnorm(length(Time), 0, 0.02)
    )
  }))

  fit <- fit_omnibus_growth(
    data = dat,
    primary = "HuangNLM",
    time = "Time",
    response = "lnN",
    group = "Condition",
    secondary = list(
      MUmax = omnibus_secondary("CMTI", x = "Temp", transform = "square")
    ),
    random = Y0 ~ 1,
    start = c(
      Y0 = 2,
      Ymax = 8,
      Tmax = 45,
      Tmin = 5,
      MUopt = 0.9,
      Topt = 30
    )
  )

  expect_s3_class(fit, "predmicror_omnibus_fit")
  formula_txt <- paste(deparse(fit$formula), collapse = " ")
  formula_txt <- gsub("\\s+", " ", formula_txt)
  expect_match(formula_txt, "CMTI", fixed = TRUE)
  expect_match(formula_txt, "HuangNLM", fixed = TRUE)
  expect_true(all(c("Tmax", "Tmin", "MUopt", "Topt") %in% names(nlme::fixef(fit$fit))))
  expect_lt(fit_metrics(fit)$RMSE, 0.04)
})

test_that("omnibus_secondary builds registered secondary model formulas", {
  spec <- omnibus_secondary("CMTI", x = "Temp", transform = "square")
  expect_s3_class(spec, "omnibus_secondary")
  expect_equal(spec$model, "CMTI")
  expect_equal(spec$x, "Temp")
  expect_equal(spec$params, c("Tmax", "Tmin", "MUopt", "Topt"))
  f <- predmicror:::.predmicror_omnibus_model_formula(
    response = "lnN",
    fun = "HuangNLM",
    time = "Time",
    params = c("Y0", "Ymax", "MUmax"),
    secondary = list(MUmax = spec)
  )
  txt <- paste(deparse(f), collapse = " ")
  txt <- gsub("\\s+", " ", txt)
  expect_match(txt, "HuangNLM", fixed = TRUE)
  expect_match(txt, "CMTI", fixed = TRUE)
  expect_match(txt, "I((CMTI(Temp, Tmax, Tmin, MUopt, Topt))^2)", fixed = TRUE)
  fixed <- predmicror:::.predmicror_omnibus_fixed_list(
    params = c("Y0", "Ymax", "MUmax"),
    secondary = list(MUmax = spec)
  )
  expect_true(all(c("Y0", "Ymax", "Tmax", "Tmin", "MUopt", "Topt") %in% names(fixed)))
  expect_false("MUmax" %in% names(fixed))
})
