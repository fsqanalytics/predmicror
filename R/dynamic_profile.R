#' Create a dynamic environmental profile
#'
#' `dynamic_profile()` stores time-varying environmental conditions such as
#' temperature, pH, or water activity for dynamic predictive microbiology
#' simulations. The profile is interpolated internally when dynamic models are
#' solved between observed profile points.
#'
#' @param time Numeric vector with profile times.
#' @param temperature Optional numeric vector with temperatures at `time`.
#' @param ph Optional numeric vector with pH values at `time`.
#' @param aw Optional numeric vector with water activity values at `time`.
#' @param ... Additional named numeric vectors with the same length as `time`.
#'
#' @return A `predmicror_dynamic_profile` data frame sorted by time.
#' @export
#'
#' @examples
#' profile <- dynamic_profile(
#'   time = c(0, 5, 10, 15),
#'   temperature = c(10, 4, 12, 20)
#' )
#' profile
#' @seealso [predict_dynamic_growth()], [predict_dynamic_inactivation()]
dynamic_profile <- function(time, temperature = NULL, ph = NULL, aw = NULL, ...) {
  if (missing(time)) {
    stop("`time` must be supplied.", call. = FALSE)
  }
  if (!is.numeric(time) || anyNA(time) || !length(time)) {
    stop("`time` must be a non-empty numeric vector without missing values.", call. = FALSE)
  }
  if (anyDuplicated(time)) {
    stop("`time` values in a dynamic profile must be unique.", call. = FALSE)
  }

  values <- list(temperature = temperature, ph = ph, aw = aw, ...)
  values <- values[!vapply(values, is.null, logical(1))]

  if (!length(values)) {
    stop("At least one environmental variable must be supplied.", call. = FALSE)
  }

  n <- length(time)
  for (nm in names(values)) {
    if (!is.numeric(values[[nm]]) || length(values[[nm]]) != n) {
      stop(sprintf("`%s` must be a numeric vector with the same length as `time`.", nm), call. = FALSE)
    }
  }

  out <- data.frame(time = time, values, check.names = FALSE)
  out <- out[order(out$time), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("predmicror_dynamic_profile", "data.frame")
  out
}

#' @export
print.predmicror_dynamic_profile <- function(x, ...) {
  cat("Dynamic environmental profile\n")
  cat(sprintf("  points: %d\n", nrow(x)))
  cat(sprintf("  time range: [%s, %s]\n", .predmicror_format_number(min(x$time)), .predmicror_format_number(max(x$time))))
  vars <- setdiff(names(x), "time")
  cat(sprintf("  variables: %s\n", paste(vars, collapse = ", ")))
  NextMethod()
  invisible(x)
}

.predmicror_as_dynamic_profile <- function(profile) {
  if (inherits(profile, "predmicror_dynamic_profile")) {
    return(profile)
  }
  if (!is.data.frame(profile)) {
    stop("`profile` must be a dynamic profile or a data frame with a `time` column.", call. = FALSE)
  }
  if (!"time" %in% names(profile)) {
    stop("`profile` must contain a `time` column.", call. = FALSE)
  }
  values <- profile[setdiff(names(profile), "time")]
  do.call(dynamic_profile, c(list(time = profile$time), as.list(values)))
}

.predmicror_profile_fun <- function(profile, variable) {
  profile <- .predmicror_as_dynamic_profile(profile)
  if (!variable %in% names(profile)) {
    stop(sprintf("The dynamic profile does not contain `%s`.", variable), call. = FALSE)
  }
  stats::approxfun(profile$time, profile[[variable]], rule = 2)
}

.predmicror_output_times <- function(profile, times, dt) {
  profile <- .predmicror_as_dynamic_profile(profile)
  if (!is.numeric(dt) || length(dt) != 1 || !is.finite(dt) || dt <= 0) {
    stop("`dt` must be a positive finite number.", call. = FALSE)
  }
  if (is.null(times)) {
    from <- min(profile$time)
    to <- max(profile$time)
    times <- seq(from, to, by = dt)
    if (tail(times, 1) < to) {
      times <- c(times, to)
    }
  }
  if (!is.numeric(times) || anyNA(times) || !length(times)) {
    stop("`times` must be a non-empty numeric vector without missing values.", call. = FALSE)
  }
  sort(unique(times))
}

.predmicror_solve_grid <- function(output_times, profile, dt) {
  profile <- .predmicror_as_dynamic_profile(profile)
  from <- min(output_times)
  to <- max(output_times)
  grid <- seq(from, to, by = dt)
  if (!length(grid) || grid[1] != from) {
    grid <- c(from, grid)
  }
  if (tail(grid, 1) < to) {
    grid <- c(grid, to)
  }
  sort(unique(c(grid, output_times, profile$time)))
}

.predmicror_format_number <- function(x, digits = 5) {
  format(signif(x, digits = digits), trim = TRUE, scientific = FALSE)
}
