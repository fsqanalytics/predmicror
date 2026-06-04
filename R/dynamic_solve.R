.predmicror_rk4_scalar <- function(times, y0, deriv) {
  if (!is.numeric(times) || length(times) < 1 || anyNA(times)) {
    stop("`times` must be a numeric vector without missing values.", call. = FALSE)
  }
  times <- sort(unique(times))
  y <- numeric(length(times))
  y[1] <- y0
  if (length(times) == 1) {
    return(data.frame(time = times, value = y))
  }
  for (i in seq_len(length(times) - 1)) {
    h <- times[i + 1] - times[i]
    if (!is.finite(h) || h < 0) {
      stop("`times` must be increasing.", call. = FALSE)
    }
    ti <- times[i]
    yi <- y[i]
    k1 <- deriv(ti, yi)
    k2 <- deriv(ti + 0.5 * h, yi + 0.5 * h * k1)
    k3 <- deriv(ti + 0.5 * h, yi + 0.5 * h * k2)
    k4 <- deriv(ti + h, yi + h * k3)
    y[i + 1] <- yi + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
  }
  data.frame(time = times, value = y)
}

.predmicror_pick_start <- function(start, names, default = NULL, required = TRUE) {
  for (nm in names) {
    if (!is.null(start[[nm]])) {
      value <- start[[nm]]
      if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
        stop(sprintf("`start$%s` must be a finite numeric scalar.", nm), call. = FALSE)
      }
      return(value)
    }
  }
  if (!is.null(default)) {
    return(default)
  }
  if (required) {
    stop(sprintf("`start` must include one of: %s.", paste(names, collapse = ", ")), call. = FALSE)
  }
  NULL
}

.predmicror_validate_start_list <- function(start) {
  if (missing(start) || !is.list(start) || is.null(names(start))) {
    stop("`start` must be a named list of parameter values.", call. = FALSE)
  }
  invisible(start)
}

.predmicror_prediction_interpolate <- function(solution, output_times) {
  f <- stats::approxfun(solution$time, solution$value, rule = 2)
  as.numeric(f(output_times))
}
