#' Finite-difference sensitivity for dynamic predictions
#'
#' `dynamic_sensitivity()` perturbs parameters in a dynamic prediction and
#' returns unscaled and scaled sensitivity coefficients. It is designed as a
#' lightweight diagnostic for sampling design and parameter identifiability.
#'
#' @param type Character string. One of `"growth"` or `"inactivation"`.
#' @param profile A [dynamic_profile()] object or compatible data frame.
#' @param start Named list of parameter values passed to the dynamic prediction
#'   function.
#' @param parameters Character vector of parameter names to perturb. Defaults to
#'   all numeric scalar entries in `start`.
#' @param relative_delta Relative perturbation size.
#' @param times Optional output times.
#' @param ... Additional arguments passed to [predict_dynamic_growth()] or
#'   [predict_dynamic_inactivation()].
#'
#' @return A data frame with time, parameter, prediction, sensitivity, and
#'   scaled sensitivity.
#' @export
#'
#' @examples
#' profile <- dynamic_profile(time = c(0, 5, 10), temperature = c(10, 15, 20))
#' sens <- dynamic_sensitivity(
#'   "growth",
#'   profile = profile,
#'   start = list(logN0 = 2, logNmax = 8, a = 0.08, Tmin = 7, lag = 1),
#'   times = seq(0, 10, by = 2),
#'   dt = 0.25
#' )
#' head(sens)
dynamic_sensitivity <- function(type = c("growth", "inactivation"),
                                profile,
                                start,
                                parameters = NULL,
                                relative_delta = 1e-6,
                                times = NULL,
                                ...) {
  type <- match.arg(type)
  .predmicror_validate_start_list(start)
  if (!is.numeric(relative_delta) || length(relative_delta) != 1 || !is.finite(relative_delta) || relative_delta <= 0) {
    stop("`relative_delta` must be a positive finite number.", call. = FALSE)
  }

  if (is.null(parameters)) {
    scalar_numeric <- vapply(start, function(x) is.numeric(x) && length(x) == 1 && is.finite(x), logical(1))
    parameters <- names(start)[scalar_numeric]
  }
  parameters <- intersect(parameters, names(start))
  if (!length(parameters)) {
    stop("No numeric scalar parameters are available for sensitivity analysis.", call. = FALSE)
  }

  predict_fun <- switch(
    type,
    growth = predict_dynamic_growth,
    inactivation = predict_dynamic_inactivation
  )
  base <- predict_fun(profile = profile, start = start, times = times, ...)

  rows <- lapply(parameters, function(parameter) {
    value <- start[[parameter]]
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
      return(NULL)
    }
    delta <- abs(value) * relative_delta
    if (delta == 0) {
      delta <- relative_delta
    }
    perturbed <- start
    perturbed[[parameter]] <- value + delta
    pred <- predict_fun(profile = profile, start = perturbed, times = base$time, ...)
    sensitivity <- (pred$response - base$response) / delta
    data.frame(
      time = base$time,
      parameter = parameter,
      prediction = base$response,
      sensitivity = sensitivity,
      scaled_sensitivity = sensitivity * value,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
