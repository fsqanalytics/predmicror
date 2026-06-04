#' Predict microbial inactivation under dynamic environmental conditions
#'
#' `predict_dynamic_inactivation()` solves a dynamic Weibull-Peleg type
#' inactivation model over a time-varying environmental profile. The output is
#' returned on the base-10 logarithmic scale.
#'
#' @param profile A [dynamic_profile()] object or data frame with a `time` column.
#' @param model Character string. Currently only `"weibull_peleg"` is supported.
#' @param secondary Character string defining the secondary model for the Peleg
#'   rate parameter `b`. Use `"constant"` for constant conditions or `"z_value"`
#'   for a log-linear temperature effect.
#' @param start Named list. Supply `logN0`, shape parameter `n`, and either `b`
#'   for `secondary = "constant"` or `b_ref`, `T_ref`, and `z` for `secondary =
#'   "z_value"`.
#' @param times Optional numeric vector of output times.
#' @param dt Maximum integration step used by the internal RK4 solver.
#' @param method Solver method. Currently only `"rk4"` is implemented.
#' @param temperature Name of the temperature column in `profile`.
#'
#' @return A `predmicror_dynamic_prediction` data frame with time, `logN`,
#'   `log_survival`, temperature, and metadata attributes.
#' @export
#'
#' @examples
#' profile <- dynamic_profile(time = c(0, 10, 20), temperature = c(55, 58, 60))
#' pred <- predict_dynamic_inactivation(
#'   profile = profile,
#'   start = list(logN0 = 7, b_ref = 0.15, T_ref = 55, z = 8, n = 1.2),
#'   secondary = "z_value",
#'   dt = 0.25
#' )
#' head(pred)
predict_dynamic_inactivation <- function(profile,
                                         model = "weibull_peleg",
                                         secondary = "constant",
                                         start,
                                         times = NULL,
                                         dt = 0.01,
                                         method = "rk4",
                                         temperature = "temperature") {
  profile <- .predmicror_as_dynamic_profile(profile)
  model <- match.arg(model, "weibull_peleg")
  secondary <- match.arg(secondary, c("constant", "z_value"))
  method <- match.arg(method, "rk4")
  .predmicror_validate_start_list(start)

  logN0 <- .predmicror_pick_start(start, "logN0")
  shape <- .predmicror_pick_start(start, c("n", "p", "shape"), default = 1, required = FALSE)
  if (shape <= 0) {
    stop("The Weibull shape parameter must be positive.", call. = FALSE)
  }

  output_times <- .predmicror_output_times(profile, times, dt)
  solve_times <- .predmicror_solve_grid(output_times, profile, dt)
  temp_fun <- .predmicror_profile_fun(profile, temperature)
  rate_fun <- .predmicror_inactivation_rate_fun(secondary, start)
  t0 <- min(solve_times)

  deriv <- function(t, logN) {
    elapsed <- max(t - t0, .Machine$double.eps)
    -rate_fun(temp_fun(t)) * shape * elapsed^(shape - 1)
  }

  solution <- .predmicror_rk4_scalar(solve_times, logN0, deriv)
  logN <- .predmicror_prediction_interpolate(solution, output_times)
  out <- data.frame(
    time = output_times,
    response = logN,
    logN = logN,
    log_survival = logN - logN0,
    temperature = as.numeric(temp_fun(output_times)),
    stringsAsFactors = FALSE
  )
  attr(out, "model") <- model
  attr(out, "type") <- "dynamic_inactivation"
  attr(out, "secondary") <- secondary
  attr(out, "scale") <- "log10"
  class(out) <- c("predmicror_dynamic_prediction", "data.frame")
  out
}

.predmicror_inactivation_rate_fun <- function(secondary, start) {
  if (identical(secondary, "constant")) {
    b <- .predmicror_pick_start(start, "b")
    if (b < 0) {
      stop("`start$b` must be non-negative.", call. = FALSE)
    }
    return(function(T) rep(b, length(T)))
  }

  if (identical(secondary, "z_value")) {
    b_ref <- .predmicror_pick_start(start, c("b_ref", "bref"))
    T_ref <- .predmicror_pick_start(start, c("T_ref", "Tref"))
    z <- .predmicror_pick_start(start, "z")
    if (b_ref < 0 || z <= 0) {
      stop("`b_ref` must be non-negative and `z` must be positive.", call. = FALSE)
    }
    return(function(T) b_ref * 10^((T - T_ref) / z))
  }

  stop("Unsupported inactivation secondary model.", call. = FALSE)
}
