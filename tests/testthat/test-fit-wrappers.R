test_that("predmicror_models lists wrapper-supported models", {
  models <- predmicror_models()

  expect_s3_class(models, "data.frame")
  expect_true(all(c("type", "model", "response_scale", "parameters") %in% names(models)))
  expect_true(all(c("growth", "inactivation", "cardinal") %in% models$type))
  expect_true(all(c("HuangFM", "WeibullM", "CMTI") %in% models$model))
})

test_that("fit wrappers validate data columns before fitting", {
  dat <- data.frame(Time = 0:3, lnN = 1:4)

  expect_error(
    fit_growth(
      dat,
      model = "HuangNLM",
      time = "Time",
      response = "missing",
      start = list(Y0 = 0, Ymax = 5, MUmax = 1)
    ),
    "missing column"
  )

  dat_bad <- data.frame(Time = c(0, 1, Inf), lnN = 1:3)
  expect_error(
    fit_growth(
      dat_bad,
      model = "HuangNLM",
      time = "Time",
      response = "lnN",
      start = list(Y0 = 0, Ymax = 5, MUmax = 1)
    ),
    "finite values"
  )
})

test_that("fit wrappers validate starting values before fitting", {
  dat <- data.frame(Time = 0:3, lnN = 1:4)

  expect_error(
    fit_growth(
      dat,
      model = "HuangNLM",
      time = "Time",
      response = "lnN",
      start = list(Y0 = 0, Ymax = 5)
    ),
    "Missing start"
  )

  expect_error(
    fit_growth(
      dat,
      model = "HuangNLM",
      time = "Time",
      response = "lnN",
      start = list(Y0 = 0, Ymax = 5, MUmax = 1, lag = 2)
    ),
    "Unknown start"
  )
})

test_that("fit_growth returns a predmicror_fit object", {
  skip_if_not_installed("gslnls")

  time <- seq(0, 8, length.out = 25)
  dat <- data.frame(
    Time = time,
    lnN = HuangNLM(time, Y0 = 1, Ymax = 5, MUmax = 0.8) + 0.001 * sin(time)
  )

  fit <- fit_growth(
    dat,
    model = HuangNLM,
    time = Time,
    response = lnN,
    start = list(Y0 = 0.9, Ymax = 5.1, MUmax = 0.7)
  )

  expect_s3_class(fit, "predmicror_fit")
  expect_equal(fit$model, "HuangNLM")
  expect_true(all(is.finite(predict(fit))))
  expect_named(coef(fit), c("Y0", "Ymax", "MUmax"))
  expect_length(fitted(fit), nrow(dat))
})

test_that("fit wrappers reject non-finite values", {
  dat <- data.frame(Time = c(0, 1, NA, 3), lnN = 1:4)
  expect_error(
    fit_growth(dat, model = "HuangNLM", time = "Time", response = "lnN",
      start = list(Y0 = 0, Ymax = 5, MUmax = 1)),
    "finite values"
  )
})

test_that("fit wrappers handle non-syntactic column names", {
  skip_if_not_installed("gslnls")
  dat <- data.frame("My Time" = seq(0, 5, length.out = 10),
    "log count" = as.numeric(1:10), check.names = FALSE)
  fit <- fit_growth(dat,
    model = "HuangNLM",
    time = "My Time",
    response = "log count",
    start = list(Y0 = 0, Ymax = 10, MUmax = 1)
  )
  expect_s3_class(fit, "predmicror_fit")
})

test_that("fit_growth accepts model argument as character string", {
  skip_if_not_installed("gslnls")
  time <- seq(0, 8, length.out = 25)
  dat <- data.frame(
    Time = time,
    lnN = HuangNLM(time, Y0 = 1, Ymax = 5, MUmax = 0.8) + 0.001 * sin(time)
  )
  fit <- fit_growth(dat,
    model = "HuangNLM",
    time = "Time",
    response = "lnN",
    start = list(Y0 = 0.9, Ymax = 5.1, MUmax = 0.7)
  )
  expect_s3_class(fit, "predmicror_fit")
})
