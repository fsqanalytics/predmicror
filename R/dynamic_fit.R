#' Fit dynamic microbial growth models
#'
#' `fit_dynamic_growth()` estimates selected parameters of a dynamic Huang-type
#' growth model by repeatedly solving the dynamic model and minimizing the
#' residual sum of squares against observed data.
#'
#' @param data Data frame with observed microbial counts.
#' @param profile A [dynamic_profile()] object or compatible data frame.
#' @param time,response Column names in `data` containing observation time and
#'   microbial response.
#' @param start Named list of starting parameter values passed to
#'   [predict_dynamic_growth()]. Numeric scalar entries are candidates for
#'   estimation.
#' @param estimate Character vector of parameter names to estimate. If `NULL`,
#'   all numeric scalar entries in `start` except those listed in `fixed` are
#'   estimated.
#' @param fixed Optional named list of parameters to keep fixed during fitting.
#' @param lower,upper Optional named numeric vectors or lists with lower and
#'   upper bounds for estimated parameters.
#' @param model,secondary,scale,dt,method,temperature Arguments passed to
#'   [predict_dynamic_growth()].
#' @param optimizer Optimization method passed to [stats::optim()].
#' @param control Optional control list passed to [stats::optim()].
#' @param ... Additional arguments passed to [predict_dynamic_growth()].
#'
#' @return A `predmicror_dynamic_fit` object.
#' @export
#'
#' @examples
#' profile <- dynamic_profile(time = c(0, 10), temperature = c(20, 20))
#' obs <- data.frame(time = c(0, 5, 10), logN = c(2, 3.3, 5.1))
#' fit <- fit_dynamic_growth(
#'   obs,
#'   profile = profile,
#'   time = "time",
#'   response = "logN",
#'   start = list(logN0 = 2, logNmax = 8, MUmax = 0.4, lag = 0),
#'   estimate = "MUmax",
#'   secondary = "constant",
#'   dt = 0.25
#' )
#' coef(fit)
fit_dynamic_growth <- function(data,
                               profile,
                               time,
                               response,
                               start,
                               estimate = NULL,
                               fixed = NULL,
                               lower = NULL,
                               upper = NULL,
                               model = "huang",
                               secondary = "huang_sqrt",
                               scale = c("log10", "ln"),
                               dt = 0.01,
                               method = "rk4",
                               temperature = "temperature",
                               optimizer = "L-BFGS-B",
                               control = list(),
                               ...) {
  scale <- match.arg(scale)
  obs <- .predmicror_dynamic_fit_data(data, time, response)
  profile <- .predmicror_as_dynamic_profile(profile)
  start <- .predmicror_dynamic_prepare_start(start, fixed)
  estimate <- .predmicror_dynamic_estimate_names(start, estimate, fixed)
  bounds <- .predmicror_dynamic_bounds(start, estimate, lower, upper)

  predict_call <- function(par_list, times) {
    predict_dynamic_growth(
      profile = profile,
      model = model,
      secondary = secondary,
      start = par_list,
      times = times,
      scale = scale,
      dt = dt,
      method = method,
      temperature = temperature,
      ...
    )
  }

  .predmicror_dynamic_fit_impl(
    obs = obs,
    profile = profile,
    start = start,
    estimate = estimate,
    bounds = bounds,
    predict_call = predict_call,
    type = "dynamic_growth",
    model = model,
    secondary = secondary,
    scale = scale,
    time = time,
    response = response,
    dt = dt,
    method = method,
    temperature = temperature,
    optimizer = optimizer,
    control = control,
    call = match.call()
  )
}

#' Fit dynamic microbial inactivation models
#'
#' `fit_dynamic_inactivation()` estimates selected parameters of a dynamic
#' Weibull-Peleg inactivation model by minimizing the residual sum of squares
#' against observed data.
#'
#' @inheritParams fit_dynamic_growth
#' @param model,secondary,dt,method,temperature Arguments passed to
#'   [predict_dynamic_inactivation()].
#'
#' @return A `predmicror_dynamic_fit` object.
#' @export
#'
#' @examples
#' profile <- dynamic_profile(time = c(0, 10), temperature = c(60, 60))
#' obs <- data.frame(time = c(0, 5, 10), logN = c(7, 6, 5))
#' fit <- fit_dynamic_inactivation(
#'   obs,
#'   profile = profile,
#'   time = "time",
#'   response = "logN",
#'   start = list(logN0 = 7, b = 0.15, n = 1),
#'   estimate = "b",
#'   dt = 0.25
#' )
#' coef(fit)
fit_dynamic_inactivation <- function(data,
                                     profile,
                                     time,
                                     response,
                                     start,
                                     estimate = NULL,
                                     fixed = NULL,
                                     lower = NULL,
                                     upper = NULL,
                                     model = "weibull_peleg",
                                     secondary = "constant",
                                     dt = 0.01,
                                     method = "rk4",
                                     temperature = "temperature",
                                     optimizer = "L-BFGS-B",
                                     control = list(),
                                     ...) {
  obs <- .predmicror_dynamic_fit_data(data, time, response)
  profile <- .predmicror_as_dynamic_profile(profile)
  start <- .predmicror_dynamic_prepare_start(start, fixed)
  estimate <- .predmicror_dynamic_estimate_names(start, estimate, fixed)
  bounds <- .predmicror_dynamic_bounds(start, estimate, lower, upper)

  predict_call <- function(par_list, times) {
    predict_dynamic_inactivation(
      profile = profile,
      model = model,
      secondary = secondary,
      start = par_list,
      times = times,
      dt = dt,
      method = method,
      temperature = temperature,
      ...
    )
  }

  .predmicror_dynamic_fit_impl(
    obs = obs,
    profile = profile,
    start = start,
    estimate = estimate,
    bounds = bounds,
    predict_call = predict_call,
    type = "dynamic_inactivation",
    model = model,
    secondary = secondary,
    scale = "log10",
    time = time,
    response = response,
    dt = dt,
    method = method,
    temperature = temperature,
    optimizer = optimizer,
    control = control,
    call = match.call()
  )
}

.predmicror_dynamic_fit_impl <- function(obs,
                                         profile,
                                         start,
                                         estimate,
                                         bounds,
                                         predict_call,
                                         type,
                                         model,
                                         secondary,
                                         scale,
                                         time,
                                         response,
                                         dt,
                                         method,
                                         temperature,
                                         optimizer,
                                         control,
                                         call) {
  par0 <- unlist(start[estimate], use.names = TRUE)

  objective <- function(par) {
    par_list <- .predmicror_dynamic_replace_start(start, par)
    pred <- tryCatch(
      predict_call(par_list, obs$time),
      error = function(e) NULL
    )
    if (is.null(pred) || any(!is.finite(pred$response))) {
      return(.Machine$double.xmax^0.5)
    }
    residuals <- obs$response - pred$response
    sum(residuals^2)
  }

  opt <- stats::optim(
    par = par0,
    fn = objective,
    method = optimizer,
    lower = bounds$lower,
    upper = bounds$upper,
    control = control
  )

  coefficients <- .predmicror_dynamic_replace_start(start, opt$par)
  prediction <- predict_call(coefficients, obs$time)
  fitted_values <- prediction$response
  residual_values <- obs$response - fitted_values

  out <- list(
    call = call,
    type = type,
    model = model,
    secondary = secondary,
    scale = scale,
    data = obs$data,
    profile = profile,
    time = time,
    response = response,
    coefficients = unlist(coefficients, use.names = TRUE),
    estimated = estimate,
    fixed = setdiff(names(coefficients), estimate),
    fitted = fitted_values,
    residuals = residual_values,
    prediction = prediction,
    sse = sum(residual_values^2),
    optimizer = opt,
    convergence = opt$convergence,
    message = opt$message,
    dt = dt,
    solver = method,
    temperature = temperature
  )
  class(out) <- c("predmicror_dynamic_fit", "predmicror_fit")
  out
}

.predmicror_dynamic_fit_data <- function(data, time, response) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(time) || length(time) != 1 || !time %in% names(data)) {
    stop("`time` must name a column in `data`.", call. = FALSE)
  }
  if (!is.character(response) || length(response) != 1 || !response %in% names(data)) {
    stop("`response` must name a column in `data`.", call. = FALSE)
  }
  if (!is.numeric(data[[time]]) || !is.numeric(data[[response]])) {
    stop("The `time` and `response` columns must be numeric.", call. = FALSE)
  }
  keep <- is.finite(data[[time]]) & is.finite(data[[response]])
  if (!any(keep)) {
    stop("No complete finite observations are available for dynamic fitting.", call. = FALSE)
  }
  obs_data <- data[keep, , drop = FALSE]
  ord <- order(obs_data[[time]])
  obs_data <- obs_data[ord, , drop = FALSE]
  rownames(obs_data) <- NULL
  list(data = obs_data, time = obs_data[[time]], response = obs_data[[response]])
}

.predmicror_dynamic_prepare_start <- function(start, fixed) {
  .predmicror_validate_start_list(start)
  if (!is.null(fixed) && length(fixed) > 0L) {
    if (!is.list(fixed) || is.null(names(fixed))) {
      stop("`fixed` must be a named list.", call. = FALSE)
    }
    start[names(fixed)] <- fixed
  }
  start
}

.predmicror_dynamic_estimate_names <- function(start, estimate, fixed) {
  scalar_numeric <- vapply(start, function(x) is.numeric(x) && length(x) == 1 && is.finite(x), logical(1))
  candidates <- names(start)[scalar_numeric]
  if (!is.null(fixed) && length(fixed) > 0L) {
    candidates <- setdiff(candidates, names(fixed))
  }
  if (is.null(estimate)) {
    estimate <- candidates
  }
  if (!is.character(estimate) || !length(estimate)) {
    stop("`estimate` must contain at least one parameter name.", call. = FALSE)
  }
  missing <- setdiff(estimate, candidates)
  if (length(missing)) {
    stop(sprintf("Cannot estimate missing or non-scalar parameters: %s.", paste(missing, collapse = ", ")), call. = FALSE)
  }
  estimate
}

.predmicror_dynamic_replace_start <- function(start, values) {
  values <- as.list(values)
  for (nm in names(values)) {
    start[[nm]] <- values[[nm]]
  }
  start
}

.predmicror_dynamic_bounds <- function(start, estimate, lower, upper) {
  list(
    lower = .predmicror_dynamic_bound_vector(start, estimate, lower, lower_bound = TRUE),
    upper = .predmicror_dynamic_bound_vector(start, estimate, upper, lower_bound = FALSE)
  )
}

.predmicror_dynamic_bound_vector <- function(start, estimate, bounds, lower_bound) {
  default <- if (lower_bound) rep(-Inf, length(estimate)) else rep(Inf, length(estimate))
  names(default) <- estimate

  if (lower_bound) {
    positive <- c("a", "b", "MUmax", "mu", "mu_max", "b_ref", "bref", "z", "n", "p", "shape")
    default[intersect(estimate, positive)] <- 0
  }

  if (is.null(bounds)) {
    return(default)
  }

  if (is.list(bounds)) {
    bounds <- unlist(bounds, use.names = TRUE)
  }
  if (!is.numeric(bounds) || is.null(names(bounds))) {
    stop("`lower` and `upper` must be named numeric vectors or lists.", call. = FALSE)
  }
  for (nm in intersect(names(bounds), estimate)) {
    if (!is.finite(bounds[[nm]])) {
      default[[nm]] <- bounds[[nm]]
    } else {
      default[[nm]] <- as.numeric(bounds[[nm]])
    }
  }
  default
}

#' @export
print.predmicror_dynamic_fit <- function(x, ...) {
  cat("predmicror dynamic fit\n")
  cat("  type: ", x$type, "\n", sep = "")
  cat("  model: ", x$model, "\n", sep = "")
  cat("  secondary: ", x$secondary, "\n", sep = "")
  cat("  response: ", x$response, " (", x$scale, ")\n", sep = "")
  cat("  SSE: ", .predmicror_format_number(x$sse), "\n", sep = "")
  cat("  convergence: ", x$convergence, "\n", sep = "")
  invisible(x)
}

#' @export
summary.predmicror_dynamic_fit <- function(object, ...) {
  data.frame(
    parameter = names(object$coefficients),
    estimate = as.numeric(object$coefficients),
    estimated = names(object$coefficients) %in% object$estimated,
    stringsAsFactors = FALSE
  )
}

#' @export
coef.predmicror_dynamic_fit <- function(object, ...) {
  object$coefficients
}

#' @export
fitted.predmicror_dynamic_fit <- function(object, ...) {
  object$fitted
}

#' @export
residuals.predmicror_dynamic_fit <- function(object, ...) {
  object$residuals
}

#' @export
predict.predmicror_dynamic_fit <- function(object, profile = NULL, times = NULL, start = NULL, ...) {
  profile <- if (is.null(profile)) object$profile else .predmicror_as_dynamic_profile(profile)
  times <- if (is.null(times)) object$data[[object$time]] else times
  start <- if (is.null(start)) as.list(object$coefficients) else start

  if (identical(object$type, "dynamic_growth")) {
    return(predict_dynamic_growth(
      profile = profile,
      model = object$model,
      secondary = object$secondary,
      start = start,
      times = times,
      scale = object$scale,
      dt = object$dt,
      method = object$solver,
      temperature = object$temperature,
      ...
    ))
  }

  predict_dynamic_inactivation(
    profile = profile,
    model = object$model,
    secondary = object$secondary,
    start = start,
    times = times,
    dt = object$dt,
    method = object$solver,
    temperature = object$temperature,
    ...
  )
}

#' @export
plot.predmicror_dynamic_fit <- function(x,
                                        xlab = x$time,
                                        ylab = paste0(x$response, " (", x$scale, ")"),
                                        ...) {
  x_values <- x$data[[x$time]]
  y_values <- x$data[[x$response]]
  ord <- order(x_values)
  graphics::plot(x_values, y_values, xlab = xlab, ylab = ylab, ...)
  graphics::lines(x_values[ord], x$fitted[ord])
  invisible(x)
}

#' @export
predmicror_augment.predmicror_dynamic_fit <- function(object, newdata = NULL, ...) {
  data <- if (is.null(newdata)) object$data else newdata
  if (!is.data.frame(data)) {
    stop("`newdata` must be a data frame.", call. = FALSE)
  }
  if (!object$time %in% names(data)) {
    stop(sprintf("`newdata` must contain column `%s`.", object$time), call. = FALSE)
  }
  out <- as.data.frame(data)
  pred <- predict(object, times = out[[object$time]], ...)
  out$.fitted <- pred$response
  if (object$response %in% names(out)) {
    out$.resid <- out[[object$response]] - out$.fitted
  }
  out$.model <- object$model
  out$.type <- object$type
  out
}

#' @export
fit_metrics.predmicror_dynamic_fit <- function(object, ...) {
  residual_values <- object$residuals
  observed_values <- object$data[[object$response]]
  n <- length(residual_values)
  p <- length(object$estimated)
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
  sigma2 <- sse / n
  log_lik <- if (is.finite(sigma2) && sigma2 > 0) {
    -0.5 * n * (log(2 * pi) + 1 + log(sigma2))
  } else {
    NA_real_
  }
  data.frame(
    model = object$model,
    type = object$type,
    response = object$response,
    response_scale = object$scale,
    n = n,
    p = p,
    SSE = sse,
    RMSE = rmse,
    MAE = mae,
    bias = bias,
    RSE = rse,
    R2 = r_squared,
    adj_R2 = adj_r_squared,
    logLik = log_lik,
    AIC = if (is.finite(log_lik)) -2 * log_lik + 2 * p else NA_real_,
    BIC = if (is.finite(log_lik)) -2 * log_lik + log(n) * p else NA_real_,
    converged = identical(object$convergence, 0L),
    stringsAsFactors = FALSE
  )
}
