#' Fit omnibus predictive microbiology models
#'
#' `fit_omnibus()` fits a nonlinear mixed-effects model in which the primary
#' model is one of the parameterised `predmicror` primary models and one or more
#' primary-model parameters are described by secondary covariate formulas.
#'
#' @param data A data frame.
#' @param type Character string. One of `"growth"` or `"inactivation"`.
#' @param primary Character string naming a primary model registered in
#'   `predmicror`, for example `"HuangNLM"` or `"WeibullM"`.
#' @param time,response,group Column names for time, response, and grouping
#'   variable.
#' @param secondary Optional named list of formulas, one per primary-model
#'   parameter. Parameters not listed are modelled as intercept-only effects.
#' @param random Random-effects formula passed to [nlme::nlme()]. If the formula
#'   does not include a grouping term, `| group` is added automatically.
#' @param correlation Optional correlation structure. Use `"AR1"` for
#'   [nlme::corAR1()] within groups, `NULL` for none, or pass an `nlme`
#'   correlation object.
#' @param start Numeric vector of starting values for fixed effects, in the order
#'   expected by [nlme::nlme()].
#' @param method Estimation method passed to [nlme::nlme()].
#' @param control Optional [nlme::nlmeControl()] object.
#' @param ... Additional arguments passed to [nlme::nlme()].
#'
#' @return A `predmicror_omnibus_fit` object.
#' @export
#'
#' @examplesIf interactive()
#' dat <- data.frame(
#'   Condition = rep(1:3, each = 5),
#'   Time = rep(c(1, 2, 4, 6, 8), 3)
#' )
#' dat$logN <- WeibullM(dat$Time, Y0 = 7, sigma = 6, alpha = 1)
#' fit <- fit_omnibus_inactivation(
#'   dat,
#'   primary = "WeibullM",
#'   time = "Time",
#'   response = "logN",
#'   group = "Condition",
#'   random = Y0 ~ 1,
#'   start = c(Y0 = 7, sigma = 6, alpha = 1)
#' )
fit_omnibus <- function(data,
                         type = c("growth", "inactivation"),
                         primary,
                         time,
                         response,
                         group,
                         secondary = NULL,
                         random,
                         correlation = NULL,
                         start,
                         method = "ML",
                         control = NULL,
                         ...) {
  type <- match.arg(type)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  primary <- .predmicror_omnibus_arg_name(primary, substitute(primary), "primary")
  time <- .predmicror_omnibus_arg_name(time, substitute(time), "time")
  response <- .predmicror_omnibus_arg_name(response, substitute(response), "response")
  group <- .predmicror_omnibus_arg_name(group, substitute(group), "group")

  required <- c(time, response, group)
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(sprintf("`data` is missing required column(s): %s.", paste(missing, collapse = ", ")), call. = FALSE)
  }

  specs <- .predmicror_model_specs()
  if (!primary %in% names(specs[[type]])) {
    stop(sprintf("Unknown %s primary model `%s`.", type, primary), call. = FALSE)
  }

  spec <- specs[[type]][[primary]]
  params <- spec$params
  fixed <- .predmicror_omnibus_fixed_list(params, secondary)
  model_formula <- .predmicror_omnibus_model_formula(response, spec$fun, time, params)
  random_formula <- .predmicror_omnibus_random_formula(substitute(random), group)
  correlation_obj <- .predmicror_omnibus_correlation(correlation, time, group)

  if (!is.numeric(start) || !length(start)) {
    stop("`start` must be a numeric vector of starting values.", call. = FALSE)
  }

  dat <- as.data.frame(data)
  dat[[group]] <- as.factor(dat[[group]])

  grouped_formula <- stats::as.formula(sprintf("%s ~ %s | %s", response, time, group))
  grouped_data <- nlme::groupedData(grouped_formula, data = dat)

  if (is.null(control)) {
    control <- nlme::nlmeControl(maxIter = 100, pnlsMaxIter = 50, msMaxIter = 100)
  }

  fit <- nlme::nlme(
    model = model_formula,
    data = grouped_data,
    fixed = fixed,
    random = random_formula,
    correlation = correlation_obj,
    start = start,
    method = method,
    control = control,
    ...
  )

  fit$call$model <- model_formula
  fit$call$data <- grouped_data
  fit$call$fixed <- fixed
  fit$call$random <- random_formula
  fit$call$correlation <- correlation_obj

  structure(
    list(
      fit = fit,
      type = type,
      primary = primary,
      model = primary,
      formula = model_formula,
      data = dat,
      grouped_data = grouped_data,
      time = time,
      response = response,
      response_scale = spec$response_scale,
      group = group,
      secondary = fixed,
      random = random_formula,
      correlation = correlation,
      start = start,
      method = method,
      control = control,
      parameters = params
    ),
    class = "predmicror_omnibus_fit"
  )
}

#' @rdname fit_omnibus
#' @export
fit_omnibus_growth <- function(data,
                               primary,
                               time,
                               response,
                               group,
                               secondary = NULL,
                               random,
                               correlation = NULL,
                               start,
                               method = "ML",
                               control = NULL,
                               ...) {
  fit_omnibus(
    data = data,
    type = "growth",
    primary = primary,
    time = time,
    response = response,
    group = group,
    secondary = secondary,
    random = random,
    correlation = correlation,
    start = start,
    method = method,
    control = control,
    ...
  )
}

#' @rdname fit_omnibus
#' @export
fit_omnibus_inactivation <- function(data,
                                     primary,
                                     time,
                                     response,
                                     group,
                                     secondary = NULL,
                                     random,
                                     correlation = NULL,
                                     start,
                                     method = "ML",
                                     control = NULL,
                                     ...) {
  fit_omnibus(
    data = data,
    type = "inactivation",
    primary = primary,
    time = time,
    response = response,
    group = group,
    secondary = secondary,
    random = random,
    correlation = correlation,
    start = start,
    method = method,
    control = control,
    ...
  )
}


.predmicror_omnibus_arg_name <- function(value, expr, arg) {
  if (is.character(value) && length(value) == 1L) {
    return(value)
  }
  .predmicror_arg_name(expr, arg)
}

.predmicror_omnibus_fixed_list <- function(params, secondary) {
  if (is.null(secondary)) {
    secondary <- list()
  }
  if (!is.list(secondary) || is.null(names(secondary)) && length(secondary) > 0L) {
    stop("`secondary` must be a named list of formulas.", call. = FALSE)
  }

  unknown <- setdiff(names(secondary), params)
  if (length(unknown)) {
    stop(sprintf("Unknown primary parameter(s) in `secondary`: %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  }

  out <- lapply(params, function(param) {
    if (param %in% names(secondary)) {
      f <- secondary[[param]]
      if (!inherits(f, "formula")) {
        stop("Each `secondary` entry must be a formula.", call. = FALSE)
      }
      .predmicror_omnibus_parameter_formula(param, f)
    } else {
      stats::as.formula(sprintf("%s ~ 1", param))
    }
  })
  names(out) <- params
  out
}

.predmicror_omnibus_parameter_formula <- function(param, formula) {
  if (length(formula) == 2L) {
    rhs <- deparse(formula[[2]])
    stats::as.formula(sprintf("%s ~ %s", param, rhs))
  } else if (length(formula) == 3L) {
    lhs <- deparse(formula[[2]])
    if (!identical(lhs, param)) {
      stop(sprintf("Formula for `%s` must have `%s` on the left-hand side.", param, param), call. = FALSE)
    }
    formula
  } else {
    stop("Invalid formula in `secondary`.", call. = FALSE)
  }
}

.predmicror_omnibus_model_formula <- function(response, fun, time, params) {
  rhs <- sprintf("%s(%s, %s)", fun, time, paste(params, collapse = ", "))
  stats::as.formula(sprintf("%s ~ %s", response, rhs))
}

.predmicror_omnibus_random_formula <- function(random_expr, group) {
  if (missing(random_expr) || identical(random_expr, quote(expr = ))) {
    stop("`random` must be supplied, for example `MUmax ~ 1` or `sigma ~ 1`.", call. = FALSE)
  }

  if (inherits(random_expr, "formula")) {
    random_formula <- random_expr
  } else if (is.character(random_expr)) {
    random_formula <- stats::as.formula(random_expr)
  } else {
    random_formula <- eval(random_expr, parent.frame())
  }

  if (!inherits(random_formula, "formula")) {
    stop("`random` must be a formula.", call. = FALSE)
  }

  random_text <- paste(deparse(random_formula), collapse = " ")
  if (!grepl("|", random_text, fixed = TRUE)) {
    random_formula <- stats::as.formula(sprintf("%s | %s", random_text, group))
  }
  random_formula
}

.predmicror_omnibus_correlation <- function(correlation, time, group) {
  if (is.null(correlation)) {
    return(NULL)
  }
  if (is.character(correlation) && length(correlation) == 1L) {
    if (toupper(correlation) == "AR1") {
      return(nlme::corAR1(form = stats::as.formula(sprintf("~ %s | %s", time, group))))
    }
    stop("Unsupported `correlation`. Use NULL, \"AR1\", or an nlme correlation object.", call. = FALSE)
  }
  correlation
}

#' Methods for omnibus fits
#'
#' @param x,object A `predmicror_omnibus_fit` object.
#' @param newdata Optional data frame for prediction.
#' @param level Prediction level passed to [stats::predict()].
#' @param k Penalty used by `AIC()`.
#' @param ... Additional arguments passed to the underlying `nlme` method.
#'
#' @return The value returned by the corresponding method.
#' @name predmicror_omnibus_methods
NULL

#' @rdname predmicror_omnibus_methods
#' @export
print.predmicror_omnibus_fit <- function(x, ...) {
  cat("predmicror omnibus fit\n")
  cat("  type: ", x$type, "\n", sep = "")
  cat("  primary: ", x$primary, "\n", sep = "")
  cat("  response: ", x$response, " (", x$response_scale, ")\n", sep = "")
  cat("  group: ", x$group, "\n", sep = "")
  cat("  formula: ", deparse(x$formula), "\n", sep = "")
  cat("\n")
  print(x$fit, ...)
  invisible(x)
}

#' @rdname predmicror_omnibus_methods
#' @export
summary.predmicror_omnibus_fit <- function(object, ...) {
  summary(object$fit, ...)
}

#' @rdname predmicror_omnibus_methods
#' @export
coef.predmicror_omnibus_fit <- function(object, ...) {
  stats::coef(object$fit, ...)
}

#' @rdname predmicror_omnibus_methods
#' @export
fitted.predmicror_omnibus_fit <- function(object, ...) {
  stats::fitted(object$fit, ...)
}

#' @rdname predmicror_omnibus_methods
#' @export
residuals.predmicror_omnibus_fit <- function(object, ...) {
  stats::residuals(object$fit, ...)
}

#' @rdname predmicror_omnibus_methods
#' @export
predict.predmicror_omnibus_fit <- function(object, newdata = NULL, level = 0, ...) {
  if (is.null(newdata)) {
    stats::predict(object$fit, level = level, ...)
  } else {
    stats::predict(object$fit, newdata = as.data.frame(newdata), level = level, ...)
  }
}

#' @rdname predmicror_omnibus_methods
#' @export
logLik.predmicror_omnibus_fit <- function(object, ...) {
  stats::logLik(object$fit, ...)
}

#' @rdname predmicror_omnibus_methods
#' @export
AIC.predmicror_omnibus_fit <- function(object, ..., k = 2) {
  stats::AIC(object$fit, ..., k = k)
}

#' @rdname predmicror_omnibus_methods
#' @export
BIC.predmicror_omnibus_fit <- function(object, ...) {
  stats::BIC(object$fit, ...)
}

#' @export
predmicror_augment.predmicror_omnibus_fit <- function(object, newdata = NULL, level = 0, ...) {
  data <- if (is.null(newdata)) object$data else as.data.frame(newdata)

  if (!object$time %in% names(data)) {
    stop(sprintf("`newdata` must contain column `%s`.", object$time), call. = FALSE)
  }

  out <- as.data.frame(data)
  out$.fitted <- as.numeric(predict(object, newdata = out, level = level, ...))

  if (object$response %in% names(out)) {
    out$.resid <- out[[object$response]] - out$.fitted
  }

  out$.model <- object$primary
  out$.type <- paste0("omnibus_", object$type)
  out
}

#' @export
as.data.frame.predmicror_omnibus_fit <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- predmicror_augment(x, ...)

  if (!is.null(row.names)) {
    row.names(out) <- row.names
  }

  out
}

#' @export
fit_metrics.predmicror_omnibus_fit <- function(object, level = 0, ...) {
  aug <- predmicror_augment(object, level = level, ...)
  residual_values <- aug$.resid
  observed_values <- aug[[object$response]]

  n <- length(residual_values)
  p <- length(nlme::fixef(object$fit))
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
    model = object$primary,
    type = paste0("omnibus_", object$type),
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
    converged = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Validate an omnibus fit by leaving out one group
#'
#' @param object A `predmicror_omnibus_fit` object.
#' @param group_value Group value to leave out.
#' @param level Prediction level. Defaults to `0` for population-level
#'   prediction of the left-out group.
#' @param ... Additional arguments passed to `fit_omnibus()` during refitting.
#'
#' @return A list with the refitted model, validation data, predictions,
#'   residuals, bias factor, and accuracy factor.
#' @export
validate_omnibus_leave_one_out <- function(object, group_value, level = 0, ...) {
  if (!inherits(object, "predmicror_omnibus_fit")) {
    stop("`object` must be a `predmicror_omnibus_fit` object.", call. = FALSE)
  }

  group_chr <- as.character(object$data[[object$group]])
  left_out <- as.character(group_value)
  train <- object$data[group_chr != left_out, , drop = FALSE]
  validation <- object$data[group_chr == left_out, , drop = FALSE]

  if (!nrow(validation)) {
    stop("`group_value` was not found in the fitted data.", call. = FALSE)
  }

  refit <- fit_omnibus(
    data = train,
    type = object$type,
    primary = object$primary,
    time = object$time,
    response = object$response,
    group = object$group,
    secondary = object$secondary,
    random = object$random,
    correlation = object$correlation,
    start = object$start,
    method = object$method,
    control = object$control,
    ...
  )

  predicted <- as.numeric(predict(refit, newdata = validation, level = level))
  validation$.predicted <- predicted
  validation$.resid <- validation[[object$response]] - predicted

  list(
    group = group_value,
    fit = refit,
    data = validation,
    observed = validation[[object$response]],
    predicted = predicted,
    residuals = validation$.resid,
    bias_factor = bias_factor(validation[[object$response]], predicted),
    accuracy_factor = accuracy_factor(validation[[object$response]], predicted)
  )
}

#' Bias and accuracy factors
#'
#' `bias_factor()` and `accuracy_factor()` calculate the multiplicative model
#' bias and accuracy factors commonly used in predictive microbiology model
#' validation.
#'
#' @param observed,predicted Numeric vectors. Values must be positive.
#'
#' @return A numeric scalar.
#' @export
bias_factor <- function(observed, predicted) {
  .predmicror_omnibus_validate_factor_inputs(observed, predicted)
  10^mean(log10(predicted / observed))
}

#' @rdname bias_factor
#' @export
accuracy_factor <- function(observed, predicted) {
  .predmicror_omnibus_validate_factor_inputs(observed, predicted)
  10^mean(abs(log10(predicted / observed)))
}

.predmicror_omnibus_validate_factor_inputs <- function(observed, predicted) {
  if (!is.numeric(observed) || !is.numeric(predicted)) {
    stop("`observed` and `predicted` must be numeric.", call. = FALSE)
  }
  if (length(observed) != length(predicted)) {
    stop("`observed` and `predicted` must have the same length.", call. = FALSE)
  }
  if (!length(observed)) {
    stop("`observed` and `predicted` must not be empty.", call. = FALSE)
  }
  if (any(!is.finite(observed)) || any(!is.finite(predicted))) {
    stop("`observed` and `predicted` must be finite.", call. = FALSE)
  }
  if (any(observed <= 0) || any(predicted <= 0)) {
    stop("`observed` and `predicted` must be positive.", call. = FALSE)
  }
  invisible(TRUE)
}
