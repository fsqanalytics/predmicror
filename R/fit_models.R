#' List models available through the fitting wrappers
#'
#' @param type Character string. One of `"all"`, `"growth"`, `"inactivation"`,
#'   or `"cardinal"`.
#'
#' @return A data frame with the model type, model name, expected response scale,
#'   and parameters that must be supplied in `start`.
#' @export
#'
#' @examples
#' predmicror_models()
#' predmicror_models("growth")
predmicror_models <- function(type = "all") {
  specs <- .predmicror_model_specs()
  type <- match.arg(type, c("all", names(specs)))

  selected <- if (identical(type, "all")) names(specs) else type

  rows <- lapply(selected, function(model_type) {
    models <- specs[[model_type]]
    do.call(rbind, lapply(names(models), function(model_name) {
      spec <- models[[model_name]]
      data.frame(
        type = model_type,
        model = model_name,
        response_scale = spec$response_scale,
        parameters = paste(spec$params, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }))
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Fit a primary growth model
#'
#' `fit_growth()` validates the input data, builds the nonlinear model formula,
#' fits it with [gslnls::gsl_nls()], and returns a `predmicror_fit` object.
#'
#' Growth models expect the response to be the natural logarithm of the microbial
#' concentration, usually `lnN`.
#'
#' @param data A data frame containing the time and response variables.
#' @param model A model name, either quoted or unquoted. See
#'   [predmicror_models()].
#' @param time Column containing time values, either quoted or unquoted.
#' @param response Column containing the response values, either quoted or
#'   unquoted. Defaults to `"lnN"`.
#' @param start Named list of initial parameter values for the selected model.
#' @param ... Additional arguments passed to [gslnls::gsl_nls()].
#'
#' @return A `predmicror_fit` object with the fitted model and metadata.
#' @export
#'
#' @examples
#' data(growthfull)
#' fit <- fit_growth(
#'   growthfull,
#'   model = "HuangFM",
#'   time = "Time",
#'   response = "lnN",
#'   start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
#' )
#' coef(fit)
fit_growth <- function(data, model, time, response = "lnN", start, ...) {
  if (missing(start)) {
    stop("`start` must be supplied as a named list of initial values.", call. = FALSE)
  }

  .predmicror_fit_model(
    data = data,
    model = .predmicror_arg_name(substitute(model), "model"),
    x = .predmicror_arg_name(substitute(time), "time"),
    response = .predmicror_arg_name(substitute(response), "response"),
    start = start,
    type = "growth",
    x_role = "time",
    ...
  )
}

#' Fit a microbial inactivation model
#'
#' `fit_inactivation()` validates the input data, builds the nonlinear model
#' formula, fits it with [gslnls::gsl_nls()], and returns a `predmicror_fit`
#' object.
#'
#' Inactivation models expect the response to be the base 10 logarithm of the
#' microbial concentration, usually `logN`.
#'
#' @param data A data frame containing the time and response variables.
#' @param model A model name, either quoted or unquoted. See
#'   [predmicror_models()].
#' @param time Column containing time values, either quoted or unquoted.
#' @param response Column containing the response values, either quoted or
#'   unquoted. Defaults to `"logN"`.
#' @param start Named list of initial parameter values for the selected model.
#' @param ... Additional arguments passed to [gslnls::gsl_nls()].
#'
#' @return A `predmicror_fit` object with the fitted model and metadata.
#' @export
#'
#' @examples
#' data(mafart2005Li11)
#' fit <- fit_inactivation(
#'   mafart2005Li11,
#'   model = "WeibullM",
#'   time = "Time",
#'   response = "logN",
#'   start = list(Y0 = 10, sigma = 3, alpha = 1)
#' )
#' coef(fit)
fit_inactivation <- function(data, model, time, response = "logN", start, ...) {
  if (missing(start)) {
    stop("`start` must be supplied as a named list of initial values.", call. = FALSE)
  }

  .predmicror_fit_model(
    data = data,
    model = .predmicror_arg_name(substitute(model), "model"),
    x = .predmicror_arg_name(substitute(time), "time"),
    response = .predmicror_arg_name(substitute(response), "response"),
    start = start,
    type = "inactivation",
    x_role = "time",
    ...
  )
}

#' Fit a cardinal parameter model
#'
#' `fit_cardinal()` validates the input data, builds the nonlinear model formula,
#' fits it with [gslnls::gsl_nls()], and returns a `predmicror_fit` object.
#'
#' Cardinal models expect the response to be the square root of the growth rate,
#' usually `sqrtGR`.
#'
#' @param data A data frame containing the environmental factor and response
#'   variables.
#' @param model A model name, either quoted or unquoted. See
#'   [predmicror_models()].
#' @param x Column containing the environmental factor values, either quoted or
#'   unquoted.
#' @param response Column containing the response values, either quoted or
#'   unquoted. Defaults to `"sqrtGR"`.
#' @param start Named list of initial parameter values for the selected model.
#' @param ... Additional arguments passed to [gslnls::gsl_nls()].
#'
#' @return A `predmicror_fit` object with the fitted model and metadata.
#' @export
#'
#' @examples
#' data(salmonella)
#' fit <- fit_cardinal(
#'   salmonella,
#'   model = "CMTI",
#'   x = "Temp",
#'   response = "sqrtGR",
#'   start = list(Tmax = 42, Tmin = 1, MUopt = 1, Topt = 37)
#' )
#' coef(fit)
fit_cardinal <- function(data, model, x, response = "sqrtGR", start, ...) {
  if (missing(start)) {
    stop("`start` must be supplied as a named list of initial values.", call. = FALSE)
  }

  .predmicror_fit_model(
    data = data,
    model = .predmicror_arg_name(substitute(model), "model"),
    x = .predmicror_arg_name(substitute(x), "x"),
    response = .predmicror_arg_name(substitute(response), "response"),
    start = start,
    type = "cardinal",
    x_role = "x",
    ...
  )
}

.predmicror_fit_model <- function(data, model, x, response, start, type, x_role, ...) {
  specs <- .predmicror_model_specs()[[type]]
  model <- .predmicror_match_model(model, specs, type)
  spec <- specs[[model]]

  .predmicror_validate_data(data, x, response)
  start <- .predmicror_validate_start(start, spec$params, spec$defaults, model)

  formula <- .predmicror_formula(
    response = response,
    fun = spec$fun,
    x = x,
    params = spec$params
  )

  fit <- gslnls::gsl_nls(formula, data = data, start = start, ...)

  structure(
    list(
      fit = fit,
      model = model,
      type = type,
      formula = formula,
      data = data,
      x = x,
      x_role = x_role,
      response = response,
      response_scale = spec$response_scale,
      start = start,
      parameters = spec$params
    ),
    class = "predmicror_fit"
  )
}

.predmicror_model_specs <- function() {
  list(
    growth = list(
      HuangFM = list(
        fun = "HuangFM",
        params = c("Y0", "Ymax", "MUmax", "lag"),
        response_scale = "lnN",
        defaults = list()
      ),
      BaranyiFM = list(
        fun = "BaranyiFM",
        params = c("Y0", "Ymax", "MUmax", "lag"),
        response_scale = "lnN",
        defaults = list()
      ),
      ZwieteringFM = list(
        fun = "ZwieteringFM",
        params = c("Y0", "Ymax", "MUmax", "lag"),
        response_scale = "lnN",
        defaults = list()
      ),
      RossoFM = list(
        fun = "RossoFM",
        params = c("Y0", "Ymax", "MUmax", "lag"),
        response_scale = "lnN",
        defaults = list()
      ),
      HuangRM = list(
        fun = "HuangRM",
        params = c("Y0", "MUmax", "lag"),
        response_scale = "lnN",
        defaults = list()
      ),
      BaranyiRM = list(
        fun = "BaranyiRM",
        params = c("Y0", "MUmax", "lag"),
        response_scale = "lnN",
        defaults = list()
      ),
      BuchananRM = list(
        fun = "BuchananRM",
        params = c("Y0", "MUmax", "lag"),
        response_scale = "lnN",
        defaults = list()
      ),
      HuangNLM = list(
        fun = "HuangNLM",
        params = c("Y0", "Ymax", "MUmax"),
        response_scale = "lnN",
        defaults = list()
      ),
      FangNLM = list(
        fun = "FangNLM",
        params = c("Y0", "Ymax", "MUmax"),
        response_scale = "lnN",
        defaults = list()
      ),
      RichardsNLM = list(
        fun = "RichardsNLM",
        params = c("Y0", "Ymax", "MUmax", "m"),
        response_scale = "lnN",
        defaults = list(m = 1)
      )
    ),
    inactivation = list(
      WeibullM = list(
        fun = "WeibullM",
        params = c("Y0", "sigma", "alpha"),
        response_scale = "log10N",
        defaults = list()
      ),
      WeibullMM = list(
        fun = "WeibullMM",
        params = c("Y0", "Yres", "sigma", "alpha"),
        response_scale = "log10N",
        defaults = list()
      ),
      WeibullPH = list(
        fun = "WeibullPH",
        params = c("Y0", "k", "alpha"),
        response_scale = "log10N",
        defaults = list()
      ),
      GeeraerdST = list(
        fun = "GeeraerdST",
        params = c("Y0", "Yres", "kmax", "Sl"),
        response_scale = "log10N",
        defaults = list()
      ),
      HuangRGS = list(
        fun = "HuangRGS",
        params = c("Y0", "k", "M"),
        response_scale = "log10N",
        defaults = list()
      )
    ),
    cardinal = list(
      CMTI = list(
        fun = "CMTI",
        params = c("Tmax", "Tmin", "MUopt", "Topt"),
        response_scale = "sqrtGR",
        defaults = list()
      ),
      CMAW = list(
        fun = "CMAW",
        params = c("AWmin", "MUopt", "AWopt"),
        response_scale = "sqrtGR",
        defaults = list()
      ),
      CMPH = list(
        fun = "CMPH",
        params = c("pHmax", "pHmin", "MUopt", "pHopt"),
        response_scale = "sqrtGR",
        defaults = list()
      ),
      CMInh = list(
        fun = "CMInh",
        params = c("MIC", "MUopt", "alpha"),
        response_scale = "sqrtGR",
        defaults = list()
      )
    )
  )
}

.predmicror_arg_name <- function(expr, arg) {
  if (is.character(expr) && length(expr) == 1L) {
    return(expr)
  }

  if (is.symbol(expr)) {
    return(as.character(expr))
  }

  stop(sprintf("`%s` must be supplied as a single column/model name.", arg), call. = FALSE)
}

.predmicror_match_model <- function(model, specs, type) {
  available <- names(specs)

  if (model %in% available) {
    return(model)
  }

  matched <- available[tolower(available) == tolower(model)]
  if (length(matched) == 1L) {
    return(matched)
  }

  stop(
    sprintf(
      "Unknown %s model `%s`. Available models: %s.",
      type,
      model,
      paste(available, collapse = ", ")
    ),
    call. = FALSE
  )
}

.predmicror_validate_data <- function(data, x, response) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  missing_cols <- setdiff(c(x, response), names(data))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("`data` is missing column(s): %s.", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  for (col in c(x, response)) {
    if (!is.numeric(data[[col]])) {
      stop(sprintf("Column `%s` must be numeric.", col), call. = FALSE)
    }
    if (any(!is.finite(data[[col]]))) {
      stop(sprintf("Column `%s` must contain only finite values.", col), call. = FALSE)
    }
  }

  invisible(data)
}

.predmicror_validate_start <- function(start, params, defaults, model) {
  if (!is.list(start) || is.null(names(start))) {
    stop("`start` must be a named list.", call. = FALSE)
  }

  for (nm in names(defaults)) {
    if (!nm %in% names(start)) {
      start[[nm]] <- defaults[[nm]]
    }
  }

  missing_params <- setdiff(params, names(start))
  if (length(missing_params) > 0L) {
    stop(
      sprintf(
        "Missing start value(s) for %s: %s.",
        model,
        paste(missing_params, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  extra_params <- setdiff(names(start), params)
  if (length(extra_params) > 0L) {
    stop(
      sprintf(
        "Unknown start value(s) for %s: %s.",
        model,
        paste(extra_params, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (param in params) {
    value <- start[[param]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop(
        sprintf("Start value `%s` must be a single finite numeric value.", param),
        call. = FALSE
      )
    }
  }

  start[params]
}

.predmicror_formula <- function(response, fun, x, params) {
  rhs <- sprintf(
    "%s(%s, %s)",
    fun,
    .predmicror_bt(x),
    paste(params, collapse = ", ")
  )
  stats::as.formula(
    paste(.predmicror_bt(response), "~", rhs),
    env = parent.frame()
  )
}

.predmicror_bt <- function(x) {
  if (grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) && !x %in% .predmicror_reserved_words()) {
    return(x)
  }

  paste0("`", gsub("`", "\\`", x, fixed = TRUE), "`")
}

.predmicror_reserved_words <- function() {
  c(
    "if", "else", "repeat", "while", "function", "for", "in", "next",
    "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_",
    "NA_real_", "NA_complex_", "NA_character_"
  )
}
