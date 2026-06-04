#' Predict microbial growth under dynamic environmental conditions
#'
#' `predict_dynamic_growth()` solves a differential Huang-type growth model over
#' a time-varying environmental profile. It is intended for forward prediction
#' under dynamic temperature conditions and uses an internal fourth-order
#' Runge-Kutta solver.
#'
#' @param profile A [dynamic_profile()] object or data frame with a `time` column
#'   and an environmental variable, usually `temperature`.
#' @param model Character string. Currently only `"huang"` is supported.
#' @param secondary Character string defining the secondary model for the growth
#'   rate. One of `"huang_sqrt"`, `"huang_full_sqrt"`, or `"constant"`.
#' @param start Named list of parameters. Use `logN0` and `logNmax` for base-10
#'   input, or `Y0` and `Ymax` for natural-log input. For `secondary =
#'   "huang_sqrt"`, supply `a` and `Tmin`. For `secondary =
#'   "huang_full_sqrt"`, also supply `b` and `Tmax`. For `secondary =
#'   "constant"`, supply `MUmax`.
#' @param times Optional numeric vector of output times. If `NULL`, a regular
#'   sequence covering the profile is created from `dt`.
#' @param scale Scale of the returned `response` column. One of `"log10"` or
#'   `"ln"`.
#' @param dt Maximum integration step used by the internal RK4 solver.
#' @param method Solver method. Currently only `"rk4"` is implemented.
#' @param temperature Name of the temperature column in `profile`.
#'
#' @return A `predmicror_dynamic_prediction` data frame with time, prediction,
#'   `lnN`, `logN`, temperature, and metadata attributes.
#' @export
#'
#' @examples
#' profile <- dynamic_profile(
#'   time = c(0, 5, 10, 15, 20),
#'   temperature = c(10, 4, 15, 15, 10)
#' )
#' pred <- predict_dynamic_growth(
#'   profile = profile,
#'   start = list(logN0 = 2, logNmax = 8.8, a = 0.0886, Tmin = 8.91, lag = 2),
#'   dt = 0.25
#' )
#' head(pred)
predict_dynamic_growth <- function(profile,
                                   model = "huang",
                                   secondary = "huang_sqrt",
                                   start,
                                   times = NULL,
                                   scale = c("log10", "ln"),
                                   dt = 0.01,
                                   method = "rk4",
                                   temperature = "temperature") {
  profile <- .predmicror_as_dynamic_profile(profile)
  model <- match.arg(model, "huang")
  secondary <- match.arg(secondary, c("huang_sqrt", "huang_full_sqrt", "constant"))
  scale <- match.arg(scale)
  method <- match.arg(method, "rk4")
  .predmicror_validate_start_list(start)

  Y0 <- if (!is.null(start$Y0)) {
    .predmicror_pick_start(start, "Y0")
  } else {
    .predmicror_pick_start(start, "logN0") * log(10)
  }
  Ymax <- if (!is.null(start$Ymax)) {
    .predmicror_pick_start(start, "Ymax")
  } else {
    .predmicror_pick_start(start, "logNmax") * log(10)
  }
  lag <- .predmicror_pick_start(start, "lag", default = 0, required = FALSE)

  output_times <- .predmicror_output_times(profile, times, dt)
  solve_times <- .predmicror_solve_grid(output_times, profile, dt)
  temp_fun <- .predmicror_profile_fun(profile, temperature)
  rate_fun <- .predmicror_growth_rate_fun(secondary, start)

  deriv <- function(t, Y) {
    mu <- rate_fun(temp_fun(t))
    lag_factor <- 1 / (1 + exp(-4 * (t - lag)))
    mu * lag_factor * (1 - exp(Y - Ymax))
  }

  solution <- .predmicror_rk4_scalar(solve_times, Y0, deriv)
  lnN <- .predmicror_prediction_interpolate(solution, output_times)
  logN <- lnN / log(10)
  response <- if (identical(scale, "ln")) lnN else logN
  out <- data.frame(
    time = output_times,
    response = response,
    lnN = lnN,
    logN = logN,
    temperature = as.numeric(temp_fun(output_times)),
    stringsAsFactors = FALSE
  )
  attr(out, "model") <- model
  attr(out, "type") <- "dynamic_growth"
  attr(out, "secondary") <- secondary
  attr(out, "scale") <- scale
  class(out) <- c("predmicror_dynamic_prediction", "data.frame")
  out
}

.predmicror_growth_rate_fun <- function(secondary, start) {
  if (identical(secondary, "constant")) {
    MUmax <- .predmicror_pick_start(start, c("MUmax", "mu", "mu_max"))
    return(function(T) rep(MUmax, length(T)))
  }

  if (identical(secondary, "huang_sqrt")) {
    a <- .predmicror_pick_start(start, "a")
    Tmin <- .predmicror_pick_start(start, "Tmin")
    return(function(T) {
      sqrt_mu <- a * pmax(T - Tmin, 0)^0.75
      pmax(sqrt_mu, 0)^2
    })
  }

  if (identical(secondary, "huang_full_sqrt")) {
    a <- .predmicror_pick_start(start, "a")
    b <- .predmicror_pick_start(start, "b")
    Tmin <- .predmicror_pick_start(start, "Tmin")
    Tmax <- .predmicror_pick_start(start, "Tmax")
    return(function(T) {
      sqrt_mu <- a * pmax(T - Tmin, 0)^0.75 * (1 - exp(b * (T - Tmax)))
      sqrt_mu[T >= Tmax] <- 0
      pmax(sqrt_mu, 0)^2
    })
  }

  stop("Unsupported growth secondary model.", call. = FALSE)
}

#' @export
print.predmicror_dynamic_prediction <- function(x, ...) {
  cat(sprintf("%s prediction\n", attr(x, "type")))
  cat(sprintf("  model: %s\n", attr(x, "model")))
  cat(sprintf("  secondary: %s\n", attr(x, "secondary")))
  cat(sprintf("  rows: %d\n", nrow(x)))
  NextMethod()
  invisible(x)
}
