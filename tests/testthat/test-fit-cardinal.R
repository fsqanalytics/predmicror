test_that("fit_cardinal with CMTI returns expected parameters", {
  skip_if_not_installed("gslnls")
  data(salmonella)

  fit <- fit_cardinal(salmonella,
    model = CMTI,
    x = Temp,
    response = sqrtGR,
    start = list(Tmax = 42, Tmin = 1, MUopt = 1.0, Topt = 37)
  )

  expect_s3_class(fit, "predmicror_fit")
  expect_equal(fit$model, "CMTI")
  expect_equal(fit$type, "cardinal")
  expect_equal(fit$x_role, "x")

  est <- coef(fit)
  expect_true(est["Tmax"] > 45 && est["Tmax"] < 55)
  expect_true(est["Tmin"] > 4 && est["Tmin"] < 6)
  expect_true(est["MUopt"] > 0.6 && est["MUopt"] < 0.9)
  expect_true(est["Topt"] > 38 && est["Topt"] < 42)

  expect_true(all(is.finite(predict(fit))))
  expect_s3_class(predmicror_augment(fit), "data.frame")
  expect_true(is.finite(fit_metrics(fit)$RMSE))
})

test_that("fit_cardinal with CMAW returns expected parameters", {
  skip_if_not_installed("gslnls")
  data(aw)

  fit <- fit_cardinal(aw,
    model = CMAW,
    x = aw,
    response = sqrtGR,
    start = list(AWmin = 0.89, MUopt = 1.0, AWopt = 0.98)
  )

  expect_s3_class(fit, "predmicror_fit")
  expect_equal(fit$model, "CMAW")
  expect_equal(fit$type, "cardinal")

  est <- coef(fit)
  expect_true(est["AWmin"] > 0.7 && est["AWmin"] < 0.9)
  expect_true(est["MUopt"] > 1.0 && est["MUopt"] < 2.0)
  expect_true(est["AWopt"] > 0.9 && est["AWopt"] < 1.0)

  expect_true(all(is.finite(predict(fit))))
})

test_that("fit_cardinal with CMPH returns expected parameters", {
  skip_if_not_installed("gslnls")
  data(ph)

  fit <- fit_cardinal(ph,
    model = CMPH,
    x = pH,
    response = sqrtGR,
    start = list(pHmax = 9, pHmin = 3, MUopt = 1.0, pHopt = 7)
  )

  expect_s3_class(fit, "predmicror_fit")
  expect_equal(fit$model, "CMPH")

  est <- coef(fit)
  expect_true(est["pHmax"] > 8 && est["pHmax"] < 11)
  expect_true(est["pHmin"] > 3 && est["pHmin"] < 5)
  expect_true(est["MUopt"] > 0.8 && est["MUopt"] < 1.3)
  expect_true(est["pHopt"] > 6.5 && est["pHopt"] < 8)
})

test_that("fit_cardinal with CMInh returns expected parameters", {
  skip_if_not_installed("gslnls")
  data(inh)

  fit <- fit_cardinal(inh,
    model = CMInh,
    x = Conce,
    response = sqrtGR,
    start = list(MIC = 0.89, MUopt = 1.0, alpha = 1)
  )

  expect_s3_class(fit, "predmicror_fit")
  expect_equal(fit$model, "CMInh")

  est <- coef(fit)
  expect_true(est["MIC"] > 2 && est["MIC"] < 5)
  expect_true(est["MUopt"] > 0.5 && est["MUopt"] < 1.0)
  expect_true(est["alpha"] > 0.4 && est["alpha"] < 1.0)
})

test_that("fit_cardinal accepts unquoted model and column names", {
  skip_if_not_installed("gslnls")
  data(salmonella)

  fit <- fit_cardinal(salmonella,
    model = "CMTI",
    x = "Temp",
    response = "sqrtGR",
    start = list(Tmax = 42, Tmin = 1, MUopt = 1.0, Topt = 37)
  )

  expect_s3_class(fit, "predmicror_fit")
})

test_that("fit_cardinal validates missing start values", {
  data(salmonella)

  expect_error(
    fit_cardinal(salmonella,
      model = "CMTI",
      x = Temp,
      response = sqrtGR,
      start = list(Tmax = 42, Tmin = 1)
    ),
    "Missing start"
  )
})
