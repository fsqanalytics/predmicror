#' Methods for `predmicror_fit` objects
#'
#' These methods delegate to the fitted nonlinear least-squares object returned
#' by [gslnls::gsl_nls()], while preserving the model metadata added by the
#' `fit_*()` wrappers.
#'
#' @param x,object A `predmicror_fit` object.
#' @param newdata Optional data frame for prediction. If omitted, predictions
#'   are computed for the original data.
#' @param xlab,ylab Axis labels used by `plot()`.
#' @param ... Additional arguments passed to the underlying method.
#' @param k Penalty used by `AIC()`.
#'
#' @return The value returned by the corresponding method for the underlying
#'   nonlinear model object. `plot()` invisibly returns `x`.
#'
#' @importFrom graphics lines plot
#' @importFrom stats AIC BIC coef fitted logLik predict residuals vcov
#'
#' @name predmicror_fit_methods
NULL

#' @rdname predmicror_fit_methods
#' @export
print.predmicror_fit <- function(x, ...) {
  cat("predmicror fit\n")
  cat("  type: ", x$type, "\n", sep = "")
  cat("  model: ", x$model, "\n", sep = "")
  cat("  response: ", x$response, " (", x$response_scale, ")\n", sep = "")
  cat("  formula: ", deparse(x$formula), "\n", sep = "")
  cat("\n")
  print(x$fit, ...)
  invisible(x)
}

#' @rdname predmicror_fit_methods
#' @export
summary.predmicror_fit <- function(object, ...) {
  summary(object$fit, ...)
}

#' @rdname predmicror_fit_methods
#' @export
predict.predmicror_fit <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    stats::predict(object$fit, ...)
  } else {
    stats::predict(object$fit, newdata = newdata, ...)
  }
}

#' @rdname predmicror_fit_methods
#' @export
plot.predmicror_fit <- function(x, xlab = x$x,
                                ylab = paste0(x$response, " (", x$response_scale, ")"),
                                ...) {
  x_values <- x$data[[x$x]]
  y_values <- x$data[[x$response]]
  fitted_values <- stats::fitted(x$fit)
  order_index <- order(x_values)

  graphics::plot(
    x_values,
    y_values,
    xlab = xlab,
    ylab = ylab,
    ...
  )
  graphics::lines(x_values[order_index], fitted_values[order_index])

  invisible(x)
}

#' @rdname predmicror_fit_methods
#' @export
coef.predmicror_fit <- function(object, ...) {
  stats::coef(object$fit, ...)
}

#' @rdname predmicror_fit_methods
#' @export
fitted.predmicror_fit <- function(object, ...) {
  stats::fitted(object$fit, ...)
}

#' @rdname predmicror_fit_methods
#' @export
residuals.predmicror_fit <- function(object, ...) {
  stats::residuals(object$fit, ...)
}

#' @rdname predmicror_fit_methods
#' @export
vcov.predmicror_fit <- function(object, ...) {
  stats::vcov(object$fit, ...)
}

#' @rdname predmicror_fit_methods
#' @export
logLik.predmicror_fit <- function(object, ...) {
  stats::logLik(object$fit, ...)
}

#' @rdname predmicror_fit_methods
#' @export
AIC.predmicror_fit <- function(object, ..., k = 2) {
  stats::AIC(object$fit, ..., k = k)
}

#' @rdname predmicror_fit_methods
#' @export
BIC.predmicror_fit <- function(object, ...) {
  stats::BIC(object$fit, ...)
}
