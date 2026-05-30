#' Extract fitted values and residuals from a predmicror fit
#'
#' `predmicror_augment()` returns the original data, or optional new data, with
#' columns containing fitted values and residuals. It is intentionally lightweight
#' and does not require the `broom` package.
#'
#' @param object,x A `predmicror_fit` object.
#' @param newdata Optional data frame used for prediction. If `NULL`, the data
#'   stored in the fitted object are used.
#' @param row.names,optional Arguments required by the base `as.data.frame()`
#'   generic. They are accepted for method compatibility.
#' @param ... Additional arguments passed to `predict()`.
#'
#' @return A data frame containing the original columns plus `.fitted`, `.resid`
#'   when the response column is available, `.model`, and `.type`.
#' @export
#'
#' @examplesIf interactive()
#' data(growthfull)
#' fit <- fit_growth(
#'   data = growthfull,
#'   model = "HuangFM",
#'   time = "Time",
#'   response = "lnN",
#'   start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
#' )
#' head(predmicror_augment(fit))
predmicror_augment <- function(object, ...) {
  UseMethod("predmicror_augment")
}

#' @rdname predmicror_augment
#' @export
predmicror_augment.default <- function(object, ...) {
  stop("`object` must be a `predmicror_fit` object.", call. = FALSE)
}

#' @rdname predmicror_augment
#' @export
predmicror_augment.predmicror_fit <- function(object, newdata = NULL, ...) {
  .predmicror_check_fit_object(object)

  data <- if (is.null(newdata)) object$data else newdata

  if (!is.data.frame(data)) {
    stop("`newdata` must be a data frame.", call. = FALSE)
  }

  if (!object$x %in% names(data)) {
    stop(sprintf("`newdata` must contain column `%s`.", object$x), call. = FALSE)
  }

  out <- as.data.frame(data)
  fitted_values <- tryCatch(
    stats::predict(object$fit, newdata = out, ...),
    error = function(e) {
      stop(sprintf("Prediction failed: %s", conditionMessage(e)), call. = FALSE)
    }
  )

  out$.fitted <- as.numeric(fitted_values)

  if (object$response %in% names(out)) {
    out$.resid <- out[[object$response]] - out$.fitted
  }

  out$.model <- object$model
  out$.type <- object$type
  out
}

#' @rdname predmicror_augment
#' @export
as.data.frame.predmicror_fit <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- predmicror_augment(x, ...)

  if (!is.null(row.names)) {
    row.names(out) <- row.names
  }

  out
}

#' Calculate model diagnostics for a fitted predmicror model
#'
#' `fit_metrics()` summarizes goodness-of-fit and information criteria for a
#' `predmicror_fit` object. The metrics are calculated on the response scale used
#' in the fitted model.
#'
#' @param object A `predmicror_fit` object.
#' @param ... Currently unused. Included for future extension and S3
#'   compatibility.
#'
#' @return A one-row data frame with model metadata, residual diagnostics, and
#'   information criteria.
#' @export
#'
#' @examplesIf interactive()
#' data(growthfull)
#' fit <- fit_growth(
#'   data = growthfull,
#'   model = "HuangFM",
#'   time = "Time",
#'   response = "lnN",
#'   start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
#' )
#' fit_metrics(fit)
fit_metrics <- function(object, ...) {
  UseMethod("fit_metrics")
}

#' @rdname fit_metrics
#' @export
fit_metrics.default <- function(object, ...) {
  stop("`object` must be a `predmicror_fit` object.", call. = FALSE)
}

#' @rdname fit_metrics
#' @export
fit_metrics.predmicror_fit <- function(object, ...) {
  .predmicror_check_fit_object(object)

  residual_values <- as.numeric(stats::residuals(object$fit))
  fitted_values <- as.numeric(stats::fitted(object$fit))
  observed_values <- object$data[[object$response]]

  if (length(residual_values) != length(observed_values)) {
    residual_values <- observed_values - fitted_values
  }

  n <- length(residual_values)
  p <- length(stats::coef(object$fit))
  sse <- sum(residual_values^2)
  mse <- sse / n
  rmse <- sqrt(mse)
  mae <- mean(abs(residual_values))
  bias <- mean(residual_values)
  rse <- if (n > p) sqrt(sse / (n - p)) else NA_real_

  sst <- sum((observed_values - mean(observed_values))^2)
  r_squared <- if (is.finite(sst) && sst > 0) 1 - sse / sst else NA_real_
  adj_r_squared <- if (is.finite(r_squared) && n > p + 1) {
    1 - (1 - r_squared) * (n - 1) / (n - p - 1)
  } else {
    NA_real_
  }

  data.frame(
    model = object$model,
    type = object$type,
    response = object$response,
    response_scale = object$response_scale,
    n = n,
    p = p,
    SSE = sse,
    RMSE = rmse,
    MAE = mae,
    bias = bias,
    RSE = rse,
    R2 = r_squared,
    adj_R2 = adj_r_squared,
    logLik = .predmicror_safe_numeric(stats::logLik(object$fit)),
    AIC = .predmicror_safe_numeric(stats::AIC(object$fit)),
    BIC = .predmicror_safe_numeric(stats::BIC(object$fit)),
    converged = .predmicror_is_converged(object$fit),
    stringsAsFactors = FALSE
  )
}

#' Compare fitted predmicror models
#'
#' `compare_models()` combines the output of [fit_metrics()] for two or more
#' fitted models. It is useful when choosing between alternative primary growth,
#' inactivation, or cardinal models fitted to the same response scale.
#'
#' @param ... `predmicror_fit` objects, or a single list of `predmicror_fit`
#'   objects.
#' @param sort_by Character string. One of `"AIC"`, `"BIC"`, `"RMSE"`,
#'   `"MAE"`, or `"none"`.
#'
#' @return A data frame with one row per fitted model.
#' @export
#'
#' @examplesIf interactive()
#' data(growthfull)
#' huang <- fit_growth(
#'   growthfull,
#'   model = "HuangFM",
#'   time = "Time",
#'   response = "lnN",
#'   start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
#' )
#' baranyi <- fit_growth(
#'   growthfull,
#'   model = "BaranyiFM",
#'   time = "Time",
#'   response = "lnN",
#'   start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
#' )
#' compare_models(huang = huang, baranyi = baranyi)
compare_models <- function(..., sort_by = c("AIC", "BIC", "RMSE", "MAE", "none")) {
  sort_by <- match.arg(sort_by)
  fits <- list(...)

  if (length(fits) == 1L && is.list(fits[[1L]]) && !inherits(fits[[1L]], "predmicror_fit")) {
    fits <- fits[[1L]]
  }

  if (length(fits) == 0L) {
    stop("Supply at least one `predmicror_fit` object.", call. = FALSE)
  }

  fit_names <- names(fits)
  if (is.null(fit_names)) {
    fit_names <- rep("", length(fits))
  }

  for (i in seq_along(fits)) {
    if (!nzchar(fit_names[[i]])) {
      fit_names[[i]] <- paste0("fit", i)
    }
  }

  rows <- lapply(seq_along(fits), function(i) {
    .predmicror_check_fit_object(fits[[i]])
    metrics <- fit_metrics(fits[[i]])
    metrics$fit <- fit_names[[i]]
    metrics[, c(
      "fit", "model", "type", "response", "response_scale", "n", "p",
      "SSE", "RMSE", "MAE", "bias", "RSE", "R2", "adj_R2", "logLik",
      "AIC", "BIC", "converged"
    )]
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL

  if (!identical(sort_by, "none") && sort_by %in% names(out)) {
    sort_index <- order(is.na(out[[sort_by]]), out[[sort_by]])
    out <- out[sort_index, , drop = FALSE]
    row.names(out) <- NULL
  }

  out
}

.predmicror_check_fit_object <- function(object) {
  if (!inherits(object, "predmicror_fit")) {
    stop("`object` must be a `predmicror_fit` object.", call. = FALSE)
  }
  invisible(object)
}

.predmicror_safe_numeric <- function(expr) {
  value <- tryCatch(
    suppressWarnings(expr),
    error = function(e) NA_real_
  )

  if (length(value) == 0L) {
    return(NA_real_)
  }

  value <- as.numeric(value)[1L]
  if (is.finite(value)) value else NA_real_
}

.predmicror_is_converged <- function(fit) {
  conv <- tryCatch(fit$convInfo$isConv, error = function(e) NULL)

  if (!is.null(conv) && length(conv) == 1L) {
    return(isTRUE(conv))
  }

  conv <- tryCatch(
    suppressWarnings(summary(fit)$convInfo$isConv),
    error = function(e) NA
  )

  if (length(conv) == 1L && !is.na(conv)) {
    return(isTRUE(conv))
  }

  NA
}
