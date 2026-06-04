# Internal data-reading and profiling helpers for the predmicror assistant.

predmicror_assist_read_data <- function(file,
                                        sheet = NULL,
                                        sep = NULL,
                                        dec = ".",
                                        na.strings = c("", "NA")) {
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
    stop("`file` must be a non-empty character string.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("`file` does not exist: ", file, call. = FALSE)
  }

  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package \"readxl\" must be installed to read Excel files.", call. = FALSE)
    }
    out <- readxl::read_excel(file, sheet = if (is.null(sheet)) 1 else sheet)
    return(as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (!ext %in% c("csv", "txt", "tsv", "tab", "dat")) {
    stop("Unsupported file type. Use .csv, .txt, .tsv, .xls, or .xlsx.", call. = FALSE)
  }

  if (is.null(sep)) {
    sep <- predmicror_assist_detect_separator(file)
  }

  utils::read.table(
    file = file,
    header = TRUE,
    sep = sep,
    dec = dec,
    na.strings = na.strings,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    comment.char = "",
    quote = "\"'"
  )
}

predmicror_assist_detect_separator <- function(file) {
  lines <- readLines(file, n = 5L, warn = FALSE)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    return(",")
  }
  first <- lines[1]
  counts <- c(
    comma = lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))),
    semicolon = lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))),
    tab = lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE)))
  )
  seps <- c(comma = ",", semicolon = ";", tab = "\t")
  seps[[names(counts)[which.max(counts)]]]
}

predmicror_assist_prepare_data <- function(data = NULL,
                                           file = NULL,
                                           sheet = NULL,
                                           sep = NULL,
                                           dec = ".",
                                           na.strings = c("", "NA")) {
  has_data <- !is.null(data)
  has_file <- !is.null(file)
  if (has_data && has_file) {
    stop("Use either `data` or `file`, not both.", call. = FALSE)
  }
  if (!has_data && !has_file) {
    return(NULL)
  }

  if (has_file) {
    data <- predmicror_assist_read_data(file, sheet = sheet, sep = sep, dec = dec, na.strings = na.strings)
    source <- list(kind = "file", file = file, sheet = sheet, sep = sep, dec = dec)
  } else {
    if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
    }
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
    source <- list(kind = "data", file = NULL, sheet = NULL, sep = sep, dec = dec)
  }

  profile <- predmicror_assist_profile_data(data)
  profile$source <- source
  profile
}

predmicror_assist_profile_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  names_in <- names(data)
  normalized <- predmicror_assist_normalize_names(names_in)
  classes <- vapply(data, function(x) paste(class(x), collapse = "/"), character(1))
  numeric <- vapply(data, is.numeric, logical(1))
  missing <- vapply(data, function(x) sum(is.na(x)), integer(1))
  ranges <- lapply(names_in, function(nm) {
    x <- data[[nm]]
    if (is.numeric(x) && any(!is.na(x))) {
      rng <- range(x, na.rm = TRUE)
      c(min = unname(rng[1]), max = unname(rng[2]))
    } else {
      c(min = NA_real_, max = NA_real_)
    }
  })
  names(ranges) <- names_in

  columns <- list(
    time = predmicror_assist_guess_column(names_in, normalized, c(
      "time", "tempo", "hour", "hours", "hora", "horas", "h", "t", "min", "minute", "minutes"
    ), numeric),
    response = predmicror_assist_guess_column(names_in, normalized, c(
      "logn", "lnn", "ln_n", "log10n", "log10_n", "n", "count", "counts", "cfu", "ufc", "y", "response", "resposta", "od"
    ), numeric),
    sqrtgr = predmicror_assist_guess_column(names_in, normalized, c(
      "sqrtgr", "sqrt_gr", "sqrtgrowthrate", "sqrt_growth_rate", "growthrate", "growth_rate", "gr", "mu", "mumax", "u"
    ), numeric),
    temperature = predmicror_assist_guess_column(names_in, normalized, c(
      "temp", "temperature", "temperatura", "tdegc", "tc", "celsius"
    ), numeric),
    ph = predmicror_assist_guess_column(names_in, normalized, c("ph", "p_h"), numeric),
    aw = predmicror_assist_guess_column(names_in, normalized, c("aw", "a_w", "wateractivity", "water_activity"), numeric),
    inhibitor = predmicror_assist_guess_column(names_in, normalized, c(
      "inhibitor", "inibidor", "conce", "concentration", "concentracao", "concentracao", "conc", "mic"
    ), numeric)
  )

  if (is.na(columns$response) && !is.na(columns$sqrtgr)) {
    columns$response <- columns$sqrtgr
  }

  task <- predmicror_assist_guess_data_task(data, columns)
  response_scale <- predmicror_assist_guess_response_scale(data, columns)
  candidate <- predmicror_assist_candidate_from_profile(task, columns)

  list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    names = names_in,
    classes = classes,
    numeric = numeric,
    missing = missing,
    ranges = ranges,
    columns = columns,
    task = task,
    response_scale = response_scale,
    candidate = candidate
  )
}

predmicror_assist_normalize_names <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  gsub("[^a-z0-9]+", "", x)
}

predmicror_assist_guess_column <- function(names_in, normalized, patterns, numeric) {
  patterns <- predmicror_assist_normalize_names(patterns)
  exact <- which(normalized %in% patterns & numeric)
  if (length(exact)) {
    return(names_in[exact[1]])
  }
  contains_patterns <- patterns[nchar(patterns) > 1L]
  contains <- integer(0)
  if (length(contains_patterns)) {
    contains <- which(vapply(normalized, function(nm) {
      any(vapply(contains_patterns, function(pat) grepl(pat, nm, fixed = TRUE), logical(1)))
    }, logical(1)) & numeric)
  }
  if (length(contains)) {
    return(names_in[contains[1]])
  }
  NA_character_
}

predmicror_assist_guess_data_task <- function(data, columns) {
  if (!is.na(columns$sqrtgr) && (!is.na(columns$temperature) || !is.na(columns$ph) || !is.na(columns$aw) || !is.na(columns$inhibitor))) {
    return("cardinal")
  }
  if (!is.na(columns$time) && !is.na(columns$response)) {
    ord <- order(data[[columns$time]])
    y <- data[[columns$response]][ord]
    y <- y[!is.na(y)]
    if (length(y) >= 2L) {
      delta <- y[length(y)] - y[1]
      if (is.finite(delta) && delta < 0) {
        return("inactivation")
      }
      if (is.finite(delta) && delta >= 0) {
        return("growth")
      }
    }
    return("growth")
  }
  if (!is.na(columns$temperature) || !is.na(columns$ph) || !is.na(columns$aw) || !is.na(columns$inhibitor)) {
    return("cardinal")
  }
  NA_character_
}

predmicror_assist_guess_response_scale <- function(data, columns) {
  if (is.na(columns$response)) {
    return(NA_character_)
  }
  nm <- predmicror_assist_normalize_names(columns$response)
  if (grepl("sqrt", nm) || identical(columns$response, columns$sqrtgr)) {
    return("sqrtGR")
  }
  if (grepl("ln", nm)) {
    return("lnN")
  }
  if (grepl("log", nm)) {
    return("log10N")
  }
  x <- data[[columns$response]]
  if (is.numeric(x) && any(!is.na(x))) {
    rng <- range(x, na.rm = TRUE)
    if (rng[1] >= -1 && rng[2] <= 12) {
      return("log-like")
    }
  }
  "unknown"
}

predmicror_assist_candidate_from_profile <- function(task, columns) {
  if (identical(task, "cardinal")) {
    if (!is.na(columns$ph)) return("CMPH")
    if (!is.na(columns$aw)) return("CMAW")
    if (!is.na(columns$inhibitor)) return("CMInh")
    return("CMTI")
  }
  if (identical(task, "inactivation")) {
    return("WeibullM")
  }
  if (identical(task, "growth")) {
    return("HuangFM")
  }
  character(0)
}

predmicror_assist_profile_text <- function(profile, language = "en") {
  if (is.null(profile)) {
    return("")
  }
  cols <- profile$columns
  miss <- sum(profile$missing, na.rm = TRUE)
  col_map <- c(
    time = cols$time,
    response = cols$response,
    temperature = cols$temperature,
    pH = cols$ph,
    aw = cols$aw,
    inhibitor = cols$inhibitor
  )
  col_map <- col_map[!is.na(col_map)]
  col_txt <- if (length(col_map)) {
    paste(sprintf("%s=`%s`", names(col_map), unname(col_map)), collapse = ", ")
  } else {
    "none"
  }
  ranges_txt <- predmicror_assist_profile_ranges_text(profile)
  if (identical(language, "pt")) {
    paste(
      sprintf("Perfil dos dados: %d linhas, %d colunas, %d valores em falta.", profile$n_rows, profile$n_cols, miss),
      sprintf("Tarefa provavel: %s; escala provavel da resposta: %s.", predmicror_assist_na_label(profile$task), predmicror_assist_na_label(profile$response_scale)),
      sprintf("Colunas detectadas: %s.", col_txt),
      ranges_txt,
      sep = "\n"
    )
  } else {
    paste(
      sprintf("Data profile: %d rows, %d columns, %d missing values.", profile$n_rows, profile$n_cols, miss),
      sprintf("Likely task: %s; likely response scale: %s.", predmicror_assist_na_label(profile$task), predmicror_assist_na_label(profile$response_scale)),
      sprintf("Detected columns: %s.", col_txt),
      ranges_txt,
      sep = "\n"
    )
  }
}

predmicror_assist_profile_ranges_text <- function(profile) {
  numeric_names <- names(profile$numeric)[profile$numeric]
  numeric_names <- numeric_names[seq_len(min(length(numeric_names), 6L))]
  if (!length(numeric_names)) {
    return("Numeric ranges: none detected.")
  }
  parts <- vapply(numeric_names, function(nm) {
    rng <- profile$ranges[[nm]]
    sprintf("%s=[%s, %s]", nm, signif(rng["min"], 4), signif(rng["max"], 4))
  }, character(1))
  paste("Numeric ranges:", paste(parts, collapse = "; "))
}

predmicror_assist_na_label <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) "unknown" else x
}

predmicror_assist_data_code <- function(spec, profile) {
  cols <- profile$columns
  task <- profile$task
  data_lines <- predmicror_assist_data_loading_lines(profile$source)
  x_col <- predmicror_assist_x_col_for_spec(spec, cols)
  response <- if (!is.na(cols$response)) cols$response else spec$response
  start <- predmicror_assist_start_for_data(spec, x_col, response)
  new_data <- if (identical(task, "cardinal")) "new_x" else "new_times"

  c(
    data_lines,
    "# Check the detected column mapping before fitting.",
    paste0("fit <- ", spec$wrapper, "("),
    "  data = dat,",
    paste0("  model = \"", spec$model, "\","),
    paste0("  ", spec$x_arg, " = \"", x_col, "\","),
    paste0("  response = \"", response, "\","),
    paste0("  start = list(", paste(start, collapse = ", "), ")"),
    ")",
    "summary(fit)",
    "fit_metrics(fit)",
    "aug <- predmicror_augment(fit)",
    paste0(new_data, " <- data.frame(\"", x_col, "\" = seq(min(dat[[\"", x_col, "\"]], na.rm = TRUE), max(dat[[\"", x_col, "\"]], na.rm = TRUE), length.out = 200), check.names = FALSE)"),
    paste0("pred <- predict(fit, newdata = ", new_data, ")"),
    paste0("plot(dat[[\"", x_col, "\"]], dat[[\"", response, "\"]], xlab = \"", x_col, "\", ylab = \"", response, "\")"),
    paste0("lines(", new_data, "[[\"", x_col, "\"]], pred, col = \"blue\", lwd = 2)"),
    paste0("points(dat[[\"", x_col, "\"]], aug$.fitted, pch = 16, col = \"grey40\")")
  )
}

predmicror_assist_data_loading_lines <- function(source) {
  if (is.null(source) || identical(source$kind, "data")) {
    return("dat <- your_data_frame")
  }
  file <- gsub("\\\\", "/", source$file)
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xls", "xlsx")) {
    sheet <- if (is.null(source$sheet)) "1" else deparse(source$sheet)
    return(c("# install.packages(\"readxl\") if needed", paste0("dat <- as.data.frame(readxl::read_excel(\"", file, "\", sheet = ", sheet, "), check.names = FALSE)")))
  }
  sep <- source$sep
  if (is.null(sep)) {
    sep <- predmicror_assist_detect_separator(file)
  }
  paste0("dat <- utils::read.table(\"", file, "\", header = TRUE, sep = ", deparse(sep), ", dec = ", deparse(source$dec), ", check.names = FALSE)")
}

predmicror_assist_x_col_for_spec <- function(spec, cols) {
  if (identical(spec$type, "cardinal")) {
    if (identical(spec$model, "CMPH") && !is.na(cols$ph)) return(cols$ph)
    if (identical(spec$model, "CMAW") && !is.na(cols$aw)) return(cols$aw)
    if (identical(spec$model, "CMInh") && !is.na(cols$inhibitor)) return(cols$inhibitor)
    if (!is.na(cols$temperature)) return(cols$temperature)
  }
  if (!is.na(cols$time)) {
    return(cols$time)
  }
  spec$x_col
}

predmicror_assist_start_for_data <- function(spec, x_col, response) {
  if (identical(spec$type, "growth")) {
    starts <- c(
      Y0 = paste0('Y0 = min(dat[["', response, '"]], na.rm = TRUE)'),
      Ymax = paste0('Ymax = max(dat[["', response, '"]], na.rm = TRUE)'),
      MUmax = "MUmax = 0.5",
      lag = paste0('lag = min(dat[["', x_col, '"]], na.rm = TRUE)')
    )
    return(unname(starts[predmicror_assist_start_names(spec$start)]))
  }
  if (identical(spec$type, "inactivation")) {
    starts <- c(
      Y0 = paste0('Y0 = max(dat[["', response, '"]], na.rm = TRUE)'),
      Yres = paste0('Yres = min(dat[["', response, '"]], na.rm = TRUE)'),
      sigma = "sigma = 1",
      alpha = "alpha = 1",
      k = "k = 0.3",
      kmax = "kmax = 0.7",
      Sl = "Sl = 1",
      M = "M = 1"
    )
    return(unname(starts[predmicror_assist_start_names(spec$start)]))
  }
  if (identical(spec$model, "CMTI")) {
    return(c(
      paste0('Tmax = max(dat[["', x_col, '"]], na.rm = TRUE) + 5'),
      paste0('Tmin = min(dat[["', x_col, '"]], na.rm = TRUE) - 5'),
      paste0('MUopt = max(dat[["', response, '"]], na.rm = TRUE)'),
      paste0('Topt = dat[["', x_col, '"]][which.max(dat[["', response, '"]])]')
    ))
  }
  if (identical(spec$model, "CMPH")) {
    return(c(
      paste0('pHmax = max(dat[["', x_col, '"]], na.rm = TRUE) + 0.5'),
      paste0('pHmin = min(dat[["', x_col, '"]], na.rm = TRUE) - 0.5'),
      paste0('MUopt = max(dat[["', response, '"]], na.rm = TRUE)'),
      paste0('pHopt = dat[["', x_col, '"]][which.max(dat[["', response, '"]])]')
    ))
  }
  if (identical(spec$model, "CMAW")) {
    return(c(
      paste0('AWmin = min(dat[["', x_col, '"]], na.rm = TRUE) - 0.02'),
      paste0('MUopt = max(dat[["', response, '"]], na.rm = TRUE)'),
      paste0('AWopt = dat[["', x_col, '"]][which.max(dat[["', response, '"]])]')
    ))
  }
  if (identical(spec$model, "CMInh")) {
    return(c(
      paste0('MIC = max(dat[["', x_col, '"]], na.rm = TRUE) * 1.2'),
      paste0('MUopt = max(dat[["', response, '"]], na.rm = TRUE)'),
      "alpha = 1"
    ))
  }
  spec$start
}

predmicror_assist_override_profile <- function(profile,
                                               task = NULL,
                                               columns = list()) {
  if (is.null(profile)) {
    return(NULL)
  }

  task <- predmicror_assist_clean_override(task)
  if (!is.null(task)) {
    if (!task %in% c("growth", "inactivation", "cardinal")) {
      stop("`task` must be one of 'growth', 'inactivation', or 'cardinal'.", call. = FALSE)
    }
    profile$task <- task
  }

  if (length(columns)) {
    for (nm in intersect(names(columns), names(profile$columns))) {
      value <- predmicror_assist_clean_override(columns[[nm]])
      if (!is.null(value)) {
        if (!value %in% profile$names) {
          stop("Column override not found in data: ", value, call. = FALSE)
        }
        profile$columns[[nm]] <- value
      }
    }
  }

  profile$candidate <- predmicror_assist_candidate_from_profile(profile$task, profile$columns)
  profile$response_scale <- predmicror_assist_guess_response_scale_from_profile(profile)
  profile
}

predmicror_assist_clean_override <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NULL)
  }
  x <- as.character(x[[1]])
  x <- trimws(x)
  if (!nzchar(x) || identical(tolower(x), "auto")) {
    return(NULL)
  }
  x
}

predmicror_assist_guess_response_scale_from_profile <- function(profile) {
  if (is.null(profile) || is.na(profile$columns$response)) {
    return(NA_character_)
  }
  nm <- predmicror_assist_normalize_names(profile$columns$response)
  if (grepl("sqrt", nm) || identical(profile$columns$response, profile$columns$sqrtgr)) {
    return("sqrtGR")
  }
  if (grepl("ln", nm)) {
    return("lnN")
  }
  if (grepl("log", nm)) {
    return("log10N")
  }
  rng <- profile$ranges[[profile$columns$response]]
  if (length(rng) == 2L && all(is.finite(rng)) && rng["min"] >= -1 && rng["max"] <= 12) {
    return("log-like")
  }
  "unknown"
}
