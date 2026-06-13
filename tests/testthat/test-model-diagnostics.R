make_diagnostic_fit <- function(offset = 0) {
  dat <- data.frame(Time = seq(0, 5, length.out = 12))
  dat$y <- 1.1 + offset + 0.7 * dat$Time + 0.02 * sin(dat$Time)

  fit <- stats::nls(
    y ~ a + b * Time,
    data = dat,
    start = list(a = 1, b = 1)
  )

  structure(
    list(
      fit = fit,
      model = "linear_test",
      type = "diagnostic",
      formula = stats::formula(fit),
      data = dat,
      x = "Time",
      x_role = "time",
      response = "y",
      response_scale = "test",
      start = list(a = 1, b = 1),
      parameters = c("a", "b")
    ),
    class = "predmicror_fit"
  )
}

test_that("predmicror_augment appends fitted values and residuals", {
  obj <- make_diagnostic_fit()
  out <- predmicror_augment(obj)

  expect_s3_class(out, "data.frame")
  expect_true(all(c(".fitted", ".resid", ".model", ".type") %in% names(out)))
  expect_equal(out$.resid, out$y - out$.fitted, tolerance = 1e-10)
  expect_equal(out$.model, rep("linear_test", nrow(out)))
})

test_that("predmicror_augment supports new data without a response column", {
  obj <- make_diagnostic_fit()
  out <- predmicror_augment(obj, newdata = data.frame(Time = c(6, 7)))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_true(".fitted" %in% names(out))
  expect_false(".resid" %in% names(out))
})

test_that("as.data.frame.predmicror_fit delegates to predmicror_augment", {
  obj <- make_diagnostic_fit()
  out <- as.data.frame(obj)

  expect_s3_class(out, "data.frame")
  expect_true(all(c(".fitted", ".resid") %in% names(out)))
})

test_that("fit_metrics returns a one-row diagnostics table", {
  obj <- make_diagnostic_fit()
  metrics <- fit_metrics(obj)

  expect_s3_class(metrics, "data.frame")
  expect_equal(nrow(metrics), 1)
  expect_true(all(c("model", "type", "n", "p", "SSE", "RMSE", "MAE", "AIC", "BIC") %in% names(metrics)))
  expect_equal(metrics$n, nrow(obj$data))
  expect_equal(metrics$p, 2)
  expect_true(is.finite(metrics$RMSE))
  expect_true(is.finite(metrics$AIC))
  expect_true(is.finite(metrics$BIC))
})

test_that("compare_models combines named fits and lists", {
  fit1 <- make_diagnostic_fit(offset = 0)
  fit2 <- make_diagnostic_fit(offset = 0.2)

  out <- compare_models(first = fit1, second = fit2, sort_by = "RMSE")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_true(all(c("fit", "model", "RMSE", "AIC", "BIC") %in% names(out)))
  expect_true(all(out$fit %in% c("first", "second")))

  out_list <- compare_models(list(a = fit1, b = fit2), sort_by = "none")
  expect_equal(out_list$fit, c("a", "b"))
})

test_that("diagnostic helpers reject non-predmicror objects", {
  expect_error(fit_metrics(list()), "predmicror_fit")
  expect_error(compare_models(list(a = list())), "predmicror_fit")
})

test_that("summary returns a list for predmicror_fit", {
  obj <- make_diagnostic_fit()
  s <- summary(obj)
  expect_type(s, "list")
  expect_true("sigma" %in% names(s) || "coefficients" %in% names(s))
})

test_that("predict with newdata returns correct length", {
  obj <- make_diagnostic_fit()
  new <- data.frame(Time = c(6, 7, 8))
  pred <- predict(obj, newdata = new)
  expect_length(pred, nrow(new))
  expect_true(all(is.finite(pred)))
})

test_that("predict returns fitted values when newdata is NULL", {
  obj <- make_diagnostic_fit()
  pred <- predict(obj)
  expect_length(pred, nrow(obj$data))
})
