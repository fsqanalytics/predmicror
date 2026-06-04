# Internal deterministic registry and verification helpers for the assistant.

predmicror_assist_registry <- function() {
  registry <- list(
    HuangFM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthfull",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "full Huang primary growth model",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "Ymax = 22", "MUmax = 1.7", "lag = 5"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    BaranyiFM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthfull",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "full Baranyi primary growth model",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "Ymax = 22", "MUmax = 1.7", "lag = 5"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    ZwieteringFM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthfull",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "full Zwietering primary growth model",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "Ymax = 22", "MUmax = 1.7", "lag = 5"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    RossoFM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthfull",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "full Rosso primary growth model",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "Ymax = 22", "MUmax = 1.7", "lag = 5"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    HuangRM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthred",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "reduced Huang primary growth model",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "MUmax = 1.7", "lag = 5"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    BaranyiRM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthred",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "reduced Baranyi primary growth model",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "MUmax = 1.7", "lag = 5"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    BuchananRM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthred",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "reduced Buchanan primary growth model",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "MUmax = 1.7", "lag = 5"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    HuangNLM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthnolag",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "Huang primary growth model without lag",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "Ymax = 22", "MUmax = 1.7"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    FangNLM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthnolag",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "Fang primary growth model without lag",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "Ymax = 22", "MUmax = 1.7"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    RichardsNLM = list(
      type = "growth", wrapper = "fit_growth", dataset = "growthnolag",
      x_arg = "time", x_col = "Time", response = "lnN", response_scale = "lnN",
      title = "Richards primary growth model without lag",
      x_label = "Time", y_label = "ln N",
      start = c("Y0 = 0", "Ymax = 22", "MUmax = 1.7", "m = 1"),
      constraints = "Growth responses should be on the natural logarithm scale (lnN)."
    ),
    WeibullM = list(
      type = "inactivation", wrapper = "fit_inactivation", dataset = "mafart2005Li11",
      x_arg = "time", x_col = "Time", response = "logN", response_scale = "log10N",
      title = "Weibull-Mafart microbial inactivation model",
      x_label = "Time", y_label = "log10 N",
      start = c("Y0 = max(mafart2005Li11$logN)", "sigma = 3", "alpha = 1"),
      constraints = "Wrapper-based inactivation workflows expect log10 counts, usually in a logN column. Some low-level legacy examples use lnN; avoid mixing scales."
    ),
    WeibullMM = list(
      type = "inactivation", wrapper = "fit_inactivation", dataset = "mafart2005Li11",
      x_arg = "time", x_col = "Time", response = "logN", response_scale = "log10N",
      title = "modified Weibull-Mafart inactivation model with residual tail",
      x_label = "Time", y_label = "log10 N",
      start = c("Y0 = max(mafart2005Li11$logN)", "Yres = min(mafart2005Li11$logN)", "sigma = 3", "alpha = 1"),
      constraints = "Wrapper-based inactivation workflows expect log10 counts, usually in a logN column. Some low-level legacy examples use lnN; avoid mixing scales."
    ),
    WeibullPH = list(
      type = "inactivation", wrapper = "fit_inactivation", dataset = "mafart2005Li11",
      x_arg = "time", x_col = "Time", response = "logN", response_scale = "log10N",
      title = "Weibull-Peleg-Huang microbial inactivation model",
      x_label = "Time", y_label = "log10 N",
      start = c("Y0 = max(mafart2005Li11$logN)", "k = 0.3", "alpha = 1"),
      constraints = "Wrapper-based inactivation workflows expect log10 counts, usually in a logN column. Some low-level legacy examples use lnN; avoid mixing scales."
    ),
    GeeraerdST = list(
      type = "inactivation", wrapper = "fit_inactivation", dataset = "mafart2005Li11",
      x_arg = "time", x_col = "Time", response = "logN", response_scale = "log10N",
      title = "Geeraerd shoulder-tail microbial inactivation model",
      x_label = "Time", y_label = "log10 N",
      start = c("Y0 = max(mafart2005Li11$logN)", "Yres = min(mafart2005Li11$logN)", "kmax = 0.7", "Sl = 4"),
      constraints = "Wrapper-based inactivation workflows expect log10 counts, usually in a logN column. Some low-level legacy examples use lnN; avoid mixing scales."
    ),
    HuangRGS = list(
      type = "inactivation", wrapper = "fit_inactivation", dataset = "mafart2005Li11",
      x_arg = "time", x_col = "Time", response = "logN", response_scale = "log10N",
      title = "Huang reparameterized Gompertz survival model",
      x_label = "Time", y_label = "log10 N",
      start = c("Y0 = max(mafart2005Li11$logN)", "k = 0.3", "M = 1"),
      constraints = "Wrapper-based inactivation workflows expect log10 counts, usually in a logN column. Some low-level legacy examples use lnN; avoid mixing scales."
    ),
    CMTI = list(
      type = "cardinal", wrapper = "fit_cardinal", dataset = "salmonella",
      x_arg = "x", x_col = "Temp", response = "sqrtGR", response_scale = "sqrtGR",
      title = "cardinal temperature model",
      x_label = "Temperature", y_label = "sqrt GR",
      start = c("Tmax = 42", "Tmin = 1", "MUopt = 1", "Topt = 37"),
      constraints = "Temperature cardinal parameters should respect Tmin < Topt < Tmax."
    ),
    CMAW = list(
      type = "cardinal", wrapper = "fit_cardinal", dataset = "aw",
      x_arg = "x", x_col = "aw", response = "sqrtGR", response_scale = "sqrtGR",
      title = "cardinal water activity model",
      x_label = "Water activity", y_label = "sqrt GR",
      start = c("AWmin = 0.89", "MUopt = 1", "AWopt = 0.98"),
      constraints = "Water activity is bounded by 1; choose AWmin < AWopt <= 1."
    ),
    CMPH = list(
      type = "cardinal", wrapper = "fit_cardinal", dataset = "ph",
      x_arg = "x", x_col = "pH", response = "sqrtGR", response_scale = "sqrtGR",
      title = "cardinal pH model",
      x_label = "pH", y_label = "sqrt GR",
      start = c("pHmax = 9", "pHmin = 3", "MUopt = 1", "pHopt = 7"),
      constraints = "pH cardinal parameters should respect pHmin < pHopt < pHmax."
    ),
    CMInh = list(
      type = "cardinal", wrapper = "fit_cardinal", dataset = "inh",
      x_arg = "x", x_col = "Conce", response = "sqrtGR", response_scale = "sqrtGR",
      title = "cardinal inhibitor model",
      x_label = "Inhibitor concentration", y_label = "sqrt GR",
      start = c("MIC = 0.89", "MUopt = 1", "alpha = 1"),
      constraints = "The MIC should be greater than the tested concentrations that still permit growth."
    )
  )

  specs <- tryCatch(.predmicror_model_specs(), error = function(e) NULL)
  if (!is.null(specs)) {
    for (type in names(specs)) {
      for (model in names(specs[[type]])) {
        if (!model %in% names(registry)) {
          next
        }
        registry[[model]]$type <- type
        registry[[model]]$response_scale <- specs[[type]][[model]]$response_scale
        registry[[model]]$params <- specs[[type]][[model]]$params
        registry[[model]]$fun <- specs[[type]][[model]]$fun
      }
    }
  }

  for (model in names(registry)) {
    if (is.null(registry[[model]]$params)) {
      registry[[model]]$params <- predmicror_assist_start_names(registry[[model]]$start)
    }
    if (is.null(registry[[model]]$fun)) {
      registry[[model]]$fun <- model
    }
    registry[[model]]$model <- model
  }

  registry
}

predmicror_assist_start_names <- function(start) {
  trimws(sub("=.*$", "", start))
}

predmicror_assist_model_index_from_registry <- function(registry) {
  entries <- vapply(names(registry), function(model) {
    spec <- registry[[model]]
    args <- paste(c(spec$x_col, spec$params), collapse = ", ")
    sprintf("%s(%s) - %s", model, args, spec$title)
  }, character(1))
  list(entries = sort(entries), names = sort(names(registry)))
}

predmicror_assist_default_candidates <- function(query, flags, registry) {
  q <- tolower(query)
  pick <- character(0)

  if (grepl("\\bph\\b", q)) {
    pick <- "CMPH"
  } else if (grepl("\\baw\\b|water activity|atividade da agua|atividade da \\u00e1gua", q)) {
    pick <- "CMAW"
  } else if (grepl("temperat", q)) {
    pick <- "CMTI"
  } else if (grepl("inibidor|inhibitor|\\bmic\\b|concentr", q)) {
    pick <- "CMInh"
  } else if ("cardinal" %in% flags) {
    pick <- "CMTI"
  } else if ("inactivation" %in% flags) {
    pick <- "WeibullM"
  } else if ("growth" %in% flags || "fit" %in% flags) {
    pick <- "HuangFM"
  }

  intersect(pick, names(registry))
}

predmicror_assist_language <- function(query) {
  q <- tolower(iconv(query, to = "ASCII//TRANSLIT", sub = ""))
  if (grepl("\\b(como|ajust|crescimento|inativ|melhor|modelo|dados|codigo|func|escala)\\b", q, perl = TRUE)) {
    return("pt")
  }
  "en"
}

predmicror_assist_wants_direct_gsl <- function(query) {
  q <- tolower(query)
  grepl("gsl_nls|direct|low[- ]level|sem wrapper|sem os wrappers|without wrapper", q)
}

predmicror_assist_fit_requested <- function(flags, candidates) {
  any(c("fit", "growth", "inactivation", "cardinal") %in% flags) || length(candidates) > 0
}

predmicror_assist_wrapper_code <- function(spec) {
  new_data <- switch(spec$type,
    cardinal = paste0("new_", tolower(spec$x_col)),
    inactivation = "new_times",
    growth = "new_times",
    "new_data"
  )

  c(
    paste0("data(", spec$dataset, ")"),
    paste0("fit <- ", spec$wrapper, "("),
    paste0("  data = ", spec$dataset, ","),
    paste0("  model = \"", spec$model, "\","),
    paste0("  ", spec$x_arg, " = \"", spec$x_col, "\","),
    paste0("  response = \"", spec$response, "\","),
    paste0("  start = list(", paste(spec$start, collapse = ", "), ")"),
    ")",
    "summary(fit)",
    "fit_metrics(fit)",
    "aug <- predmicror_augment(fit)",
    paste0(new_data, " <- data.frame(", spec$x_col, " = seq(min(", spec$dataset, "$", spec$x_col, "), max(", spec$dataset, "$", spec$x_col, "), length.out = 200))"),
    paste0("pred <- predict(fit, newdata = ", new_data, ")"),
    paste0("plot(", spec$response, " ~ ", spec$x_col, ", data = ", spec$dataset, ", xlab = \"", spec$x_label, "\", ylab = \"", spec$y_label, "\")"),
    paste0("lines(", new_data, "$", spec$x_col, ", pred, col = \"blue\", lwd = 2)"),
    paste0("points(", spec$dataset, "$", spec$x_col, ", aug$.fitted, pch = 16, col = \"grey40\")")
  )
}

predmicror_assist_direct_code <- function(spec) {
  new_data <- switch(spec$type,
    cardinal = paste0("new_", tolower(spec$x_col)),
    inactivation = "new_times",
    growth = "new_times",
    "new_data"
  )
  params <- paste(spec$params, collapse = ", ")

  c(
    paste0("data(", spec$dataset, ")"),
    paste0("fit <- gslnls::gsl_nls(", spec$response, " ~ ", spec$fun, "(", spec$x_col, ", ", params, "),"),
    paste0("  data = ", spec$dataset, ","),
    paste0("  start = list(", paste(spec$start, collapse = ", "), ")"),
    ")",
    "summary(fit)",
    "fitted_vals <- fitted(fit)",
    paste0(new_data, " <- data.frame(", spec$x_col, " = seq(min(", spec$dataset, "$", spec$x_col, "), max(", spec$dataset, "$", spec$x_col, "), length.out = 200))"),
    paste0("pred <- predict(fit, newdata = ", new_data, ")"),
    paste0("plot(", spec$response, " ~ ", spec$x_col, ", data = ", spec$dataset, ", xlab = \"", spec$x_label, "\", ylab = \"", spec$y_label, "\")"),
    paste0("lines(", new_data, "$", spec$x_col, ", pred, col = \"blue\", lwd = 2)"),
    paste0("points(", spec$dataset, "$", spec$x_col, ", fitted_vals, pch = 16, col = \"grey40\")")
  )
}

predmicror_assist_generic_code <- function() {
  c(
    "predmicror_models()",
    "predmicror_models(\"growth\")",
    "predmicror_models(\"inactivation\")",
    "predmicror_models(\"cardinal\")"
  )
}

predmicror_assist_code_from_registry <- function(candidate, registry, prefer_wrappers = TRUE, query = "") {
  if (!length(candidate) || !candidate[1] %in% names(registry)) {
    return(paste(predmicror_assist_generic_code(), collapse = "\n"))
  }
  spec <- registry[[candidate[1]]]
  use_wrapper <- isTRUE(prefer_wrappers) && !predmicror_assist_wants_direct_gsl(query)
  lines <- if (use_wrapper) predmicror_assist_wrapper_code(spec) else predmicror_assist_direct_code(spec)
  paste(lines, collapse = "\n")
}

predmicror_assist_validate_code <- function(code, registry = predmicror_assist_registry(), candidate = character(0)) {
  problems <- character(0)

  parsed <- tryCatch(parse(text = code), error = function(e) e)
  if (inherits(parsed, "error")) {
    problems <- c(problems, paste("Parse error:", conditionMessage(parsed)))
  }

  calls <- regmatches(code, gregexpr("\\b[A-Za-z][A-Za-z0-9_.]*\\s*(?=\\()", code, perl = TRUE))[[1]]
  calls <- trimws(calls)
  allowed <- c(
    names(registry),
    "fit_growth", "fit_inactivation", "fit_cardinal",
    "predmicror_augment", "fit_metrics", "compare_models", "predmicror_models",
    "data", "summary", "fitted", "predict", "plot", "lines", "points",
    "seq", "min", "max", "list", "data.frame", "library", "require",
    "c", "log", "log10", "exp", "gsl_nls"
  )
  unknown <- setdiff(unique(calls), allowed)
  unknown <- unknown[!grepl("^[A-Z]$", unknown)]
  if (length(unknown) > 0) {
    problems <- c(problems, paste("Unknown function call(s):", paste(unknown, collapse = ", ")))
  }

  if (length(candidate) > 0 && candidate[1] %in% names(registry)) {
    spec <- registry[[candidate[1]]]
    if (identical(spec$type, "growth") && !grepl("\\blnN\\s*~|response\\s*=\\s*\"lnN\"", code)) {
      problems <- c(problems, "Growth examples should use response scale lnN.")
    }
    if (identical(spec$type, "inactivation") && grepl("fit_inactivation", code) &&
        !grepl("response\\s*=\\s*\"logN\"", code)) {
      problems <- c(problems, "fit_inactivation examples should declare response = \"logN\".")
    }
    if (identical(spec$type, "cardinal") && !grepl("\\bsqrtGR\\s*~|response\\s*=\\s*\"sqrtGR\"", code)) {
      problems <- c(problems, "Cardinal examples should use response scale sqrtGR.")
    }
  }

  list(ok = length(problems) == 0, problems = problems)
}

predmicror_assist_validation_text <- function(validation, language = "en") {
  if (isTRUE(validation$ok)) {
    if (identical(language, "pt")) {
      return("Verificacao: o codigo foi validado estaticamente com parse() e regras simples de escala/assinatura.")
    }
    return("Verification: the code passed static parse() and simple scale/signature checks.")
  }

  prefix <- if (identical(language, "pt")) {
    "Avisos de verificacao estatica:"
  } else {
    "Static verification warnings:"
  }
  paste(prefix, paste(validation$problems, collapse = "; "))
}

predmicror_assist_deterministic_answer <- function(query,
                                                   candidates,
                                                   flags,
                                                   registry = predmicror_assist_registry(),
                                                   prefer_wrappers = TRUE,
                                                   verify_code = TRUE) {
  language <- predmicror_assist_language(query)
  if (length(candidates) == 0) {
    candidates <- predmicror_assist_default_candidates(query, flags, registry)
  }

  fit_requested <- predmicror_assist_fit_requested(flags, candidates)
  if (!fit_requested && length(candidates) == 0) {
    code <- paste(predmicror_assist_generic_code(), collapse = "\n")
    validation <- if (isTRUE(verify_code)) predmicror_assist_validate_code(code, registry) else list(ok = NA, problems = character(0))
    intro <- if (identical(language, "pt")) {
      paste(
        "Posso ajudar a escolher modelos de crescimento, inativacao e modelos cardinais do predmicror.",
        "Comece por listar o catalogo e confirme a escala da resposta antes do ajuste.",
        sep = "\n"
      )
    } else {
      paste(
        "I can help choose predmicror growth, inactivation, and cardinal models.",
        "Start by listing the model catalog and checking the response scale before fitting.",
        sep = "\n"
      )
    }
    answer <- paste(intro, "", "```r", code, "```", predmicror_assist_validation_text(validation, language), sep = "\n")
    return(list(answer = answer, code = code, validation = validation, candidates = candidates, language = language, mode = "generic"))
  }

  if (length(candidates) == 0) {
    candidates <- "HuangFM"
  }
  candidate <- candidates[1]
  spec <- registry[[candidate]]
  code <- predmicror_assist_code_from_registry(candidate, registry, prefer_wrappers, query)
  validation <- if (isTRUE(verify_code)) {
    predmicror_assist_validate_code(code, registry, candidate)
  } else {
    list(ok = NA, problems = character(0))
  }

  using_wrapper <- isTRUE(prefer_wrappers) && !predmicror_assist_wants_direct_gsl(query)
  intro <- if (identical(language, "pt")) {
    paste(
      sprintf("Modelo sugerido: `%s` - %s.", candidate, spec$title),
      sprintf("Escala esperada da resposta: `%s`; coluna de exemplo: `%s`.", spec$response_scale, spec$response),
      if (using_wrapper) {
        sprintf("Usei o wrapper `%s()` como caminho principal; a chamada direta a `gsl_nls()` fica para uso avancado.", spec$wrapper)
      } else {
        "Usei uma chamada direta a `gslnls::gsl_nls()` porque a pergunta pediu o nivel baixo ou nao privilegiou wrappers."
      },
      spec$constraints,
      sep = "\n"
    )
  } else {
    paste(
      sprintf("Suggested model: `%s` - %s.", candidate, spec$title),
      sprintf("Expected response scale: `%s`; example response column: `%s`.", spec$response_scale, spec$response),
      if (using_wrapper) {
        sprintf("I used the `%s()` wrapper as the main path; direct `gsl_nls()` calls are better left for advanced use.", spec$wrapper)
      } else {
        "I used a direct `gslnls::gsl_nls()` call because the query requested low-level fitting or did not prefer wrappers."
      },
      spec$constraints,
      sep = "\n"
    )
  }

  answer <- paste(intro, "", "```r", code, "```", predmicror_assist_validation_text(validation, language), sep = "\n")
  list(answer = answer, code = code, validation = validation, candidates = candidates, language = language, mode = if (using_wrapper) "wrapper" else "direct")
}

predmicror_assist_merge_answer_with_code <- function(answer, deterministic) {
  if (!nzchar(answer)) {
    return(deterministic$answer)
  }
  intro <- predmicror_assist_strip_code_blocks(answer)
  intro <- predmicror_assist_strip_inline_code(intro)
  intro <- trimws(intro)
  if (!nzchar(intro)) {
    intro <- trimws(strsplit(deterministic$answer, "```", fixed = TRUE)[[1]][1])
  }
  paste(
    intro,
    "",
    "```r",
    deterministic$code,
    "```",
    predmicror_assist_validation_text(deterministic$validation, deterministic$language),
    sep = "\n"
  )
}
