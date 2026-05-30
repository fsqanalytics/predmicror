#' Assistant for predmicror (local Ollama)
#'
#' Use a local Ollama model to answer questions about predmicror models, check
#' log scales, and draft `gsl_nls` calls. Requires the `ollama` CLI in PATH.
#'
#' @param query Character question to ask.
#' @param model Ollama model name.
#' @param root Path to the package root for context collection. Defaults to the
#'   installed package path when available, otherwise the current working
#'   directory.
#' @param return_context Logical; if TRUE, returns a list with answer and
#'   context.
#' @param conversation Optional list or character vector with prior questions
#'   and answers to include as conversational context.
#'
#' @return Character response by default; list with answer and context when
#'   `return_context = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' predmicror_assistant("How do I fit a Huang model?")
#' }
#'
predmicror_assistant <- function(query,
                                 model = "llama3-groq-tool-use:8b",
                                 root = NULL,
                                 return_context = FALSE,
                                 conversation = NULL) {
  if (!nzchar(Sys.which("ollama"))) {
    stop("ollama not found in PATH.", call. = FALSE)
  }
  if (is.null(root)) {
    root <- system.file("", package = "predmicror")
    if (!nzchar(root)) {
      root <- getwd()
    }
  }
  context <- predmicror_assist_collect_context(query, root = root)
  model_index <- predmicror_assist_model_index(root)
  candidates <- predmicror_assist_select_models(query, model_index)
  flags <- predmicror_assist_query_flags(query)
  history <- predmicror_assist_format_history(conversation)
  templates <- predmicror_assist_example_templates(candidates, flags)
  prompt <- predmicror_assist_build_prompt(
    query,
    context,
    model_index,
    candidates,
    flags,
    history,
    templates
  )
  env <- c("TERM=dumb", "NO_COLOR=1", "OLLAMA_NO_SPINNER=1")
  output <- system2("ollama", c("run", model),
    input = prompt, stdout = TRUE, stderr = TRUE, env = env
  )
  answer <- predmicror_assist_clean_output(paste(output, collapse = "\n"))
  answer <- predmicror_assist_normalize_response(answer)
  answer <- predmicror_assist_normalize_text(answer)
  if (!grepl("```", answer)) {
    inline_code <- predmicror_assist_extract_inline_code(answer)
    template_code <- predmicror_assist_code_template(candidates)
    code_block <- ""
    if (nzchar(template_code) &&
        (!nzchar(inline_code) || predmicror_assist_code_uses_examples(inline_code))) {
      code_block <- template_code
    } else if (nzchar(inline_code)) {
      code_block <- inline_code
    } else if (nzchar(template_code)) {
      code_block <- template_code
    }
    if (nzchar(code_block)) {
      explanation <- predmicror_assist_strip_inline_code(answer)
      explanation <- trimws(explanation)
      if (!nzchar(explanation)) {
        explanation <- "Here is a complete example."
      }
      answer <- paste(
        explanation,
        "",
        "```r",
        code_block,
        "```",
        sep = "\n"
      )
    }
  }
  if (return_context) {
    return(list(answer = answer, context = context))
  }
  answer
}

predmicror_assist_model_index <- function(root) {
  r_dir <- file.path(root, "R")
  if (!dir.exists(r_dir)) {
    return(list(entries = character(0), names = character(0)))
  }
  r_files <- list.files(r_dir, full.names = TRUE, pattern = "\\.R$")
  r_files <- r_files[!basename(r_files) %in% c(
    "assistant.R",
    "assistant_app.R",
    "zzz.R",
    "predmicror-package.R"
  )]
  entries <- character(0)
  names <- character(0)
  for (path in r_files) {
    lines <- tryCatch(readLines(path, warn = FALSE),
      error = function(e) character(0)
    )
    if (length(lines) == 0) {
      next
    }
    for (i in seq_along(lines)) {
      line <- lines[i]
      match <- regexec("^\\s*([A-Za-z0-9_.]+)\\s*<-\\s*function\\s*\\(([^)]*)\\)", line)
      parts <- regmatches(line, match)[[1]]
      if (length(parts) > 0) {
        name <- parts[2]
        args <- gsub("\\s+", " ", parts[3])
        title <- predmicror_assist_extract_title(lines, i)
        if (!nzchar(title)) {
          next
        }
        entries <- c(entries, sprintf("%s(%s) - %s", name, args, title))
        names <- c(names, name)
      }
    }
  }
  list(entries = sort(unique(entries)), names = sort(unique(names)))
}

predmicror_assist_format_history <- function(history, max_turns = 3) {
  if (is.null(history)) {
    return("")
  }
  if (is.character(history)) {
    items <- history[nzchar(history)]
    if (length(items) == 0) {
      return("")
    }
    if (length(items) > max_turns * 2) {
      items <- tail(items, max_turns * 2)
    }
    return(paste(items, collapse = "\n"))
  }
  if (!is.list(history) || length(history) == 0) {
    return("")
  }
  items <- history
  if (length(items) > max_turns) {
    items <- tail(items, max_turns)
  }
  lines <- character(0)
  for (item in items) {
    q <- if (!is.null(item$question)) item$question else ""
    a <- if (!is.null(item$answer)) item$answer else ""
    if (nzchar(q)) {
      lines <- c(lines, paste0("Q: ", q))
    }
    if (nzchar(a)) {
      lines <- c(lines, paste0("A: ", a))
    }
  }
  if (length(lines) == 0) {
    return("")
  }
  paste(lines, collapse = "\n")
}

predmicror_assist_template_map <- function() {
  list(
    HuangFM = c(
      "HuangFM (full growth):",
      "data(growthfull)",
      "fit <- gsl_nls(lnN ~ HuangFM(Time, Y0, Ymax, MUmax, lag),",
      "  data = growthfull,",
      "  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_times <- data.frame(Time = seq(min(growthfull$Time), max(growthfull$Time), length.out = 200))",
      "pred <- predict(fit, newdata = new_times)",
      "plot(lnN ~ Time, data = growthfull, xlab = \"Time\", ylab = \"ln N\")",
      "lines(new_times$Time, pred, col = \"blue\", lwd = 2)",
      "points(growthfull$Time, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    HuangNLM = c(
      "HuangNLM (no lag):",
      "data(growthnolag)",
      "fit <- gsl_nls(lnN ~ HuangNLM(Time, Y0, Ymax, MUmax),",
      "  data = growthnolag,",
      "  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_times <- data.frame(Time = seq(min(growthnolag$Time), max(growthnolag$Time), length.out = 200))",
      "pred <- predict(fit, newdata = new_times)",
      "plot(lnN ~ Time, data = growthnolag, xlab = \"Time\", ylab = \"ln N\")",
      "lines(new_times$Time, pred, col = \"blue\", lwd = 2)",
      "points(growthnolag$Time, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    HuangRM = c(
      "HuangRM (reduced):",
      "data(growthred)",
      "fit <- gsl_nls(lnN ~ HuangRM(Time, Y0, MUmax, lag),",
      "  data = growthred,",
      "  start = list(Y0 = 0, MUmax = 1.7, lag = 5))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_times <- data.frame(Time = seq(min(growthred$Time), max(growthred$Time), length.out = 200))",
      "pred <- predict(fit, newdata = new_times)",
      "plot(lnN ~ Time, data = growthred, xlab = \"Time\", ylab = \"ln N\")",
      "lines(new_times$Time, pred, col = \"blue\", lwd = 2)",
      "points(growthred$Time, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    BaranyiFM = c(
      "BaranyiFM (full growth):",
      "data(growthfull)",
      "fit <- gsl_nls(lnN ~ BaranyiFM(Time, Y0, Ymax, MUmax, lag),",
      "  data = growthfull,",
      "  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_times <- data.frame(Time = seq(min(growthfull$Time), max(growthfull$Time), length.out = 200))",
      "pred <- predict(fit, newdata = new_times)",
      "plot(lnN ~ Time, data = growthfull, xlab = \"Time\", ylab = \"ln N\")",
      "lines(new_times$Time, pred, col = \"blue\", lwd = 2)",
      "points(growthfull$Time, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    BaranyiRM = c(
      "BaranyiRM (reduced):",
      "data(growthred)",
      "fit <- gsl_nls(lnN ~ BaranyiRM(Time, Y0, MUmax, lag),",
      "  data = growthred,",
      "  start = list(Y0 = 0, MUmax = 1.7, lag = 5))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_times <- data.frame(Time = seq(min(growthred$Time), max(growthred$Time), length.out = 200))",
      "pred <- predict(fit, newdata = new_times)",
      "plot(lnN ~ Time, data = growthred, xlab = \"Time\", ylab = \"ln N\")",
      "lines(new_times$Time, pred, col = \"blue\", lwd = 2)",
      "points(growthred$Time, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    RichardsNLM = c(
      "RichardsNLM (no lag):",
      "data(growthnolag)",
      "fit <- gsl_nls(lnN ~ RichardsNLM(Time, Y0, Ymax, MUmax, m),",
      "  data = growthnolag,",
      "  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, m = 1))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_times <- data.frame(Time = seq(min(growthnolag$Time), max(growthnolag$Time), length.out = 200))",
      "pred <- predict(fit, newdata = new_times)",
      "plot(lnN ~ Time, data = growthnolag, xlab = \"Time\", ylab = \"ln N\")",
      "lines(new_times$Time, pred, col = \"blue\", lwd = 2)",
      "points(growthnolag$Time, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    FangNLM = c(
      "FangNLM (no lag):",
      "data(growthnolag)",
      "fit <- gsl_nls(lnN ~ FangNLM(Time, Y0, Ymax, MUmax),",
      "  data = growthnolag,",
      "  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_times <- data.frame(Time = seq(min(growthnolag$Time), max(growthnolag$Time), length.out = 200))",
      "pred <- predict(fit, newdata = new_times)",
      "plot(lnN ~ Time, data = growthnolag, xlab = \"Time\", ylab = \"ln N\")",
      "lines(new_times$Time, pred, col = \"blue\", lwd = 2)",
      "points(growthnolag$Time, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    CMTI = c(
      "CMTI (cardinal temperature):",
      "data(salmonella)",
      "fit <- gsl_nls(sqrtGR ~ CMTI(Temp, Tmax, Tmin, MUopt, Topt),",
      "  data = salmonella,",
      "  start = list(Tmax = 42, Tmin = 1, MUopt = 1.0, Topt = 37))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_temp <- data.frame(Temp = seq(min(salmonella$Temp), max(salmonella$Temp), length.out = 200))",
      "pred <- predict(fit, newdata = new_temp)",
      "plot(sqrtGR ~ Temp, data = salmonella, xlab = \"Temperature\", ylab = \"sqrt GR\")",
      "lines(new_temp$Temp, pred, col = \"blue\", lwd = 2)",
      "points(salmonella$Temp, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    CMAW = c(
      "CMAW (cardinal water activity):",
      "data(aw)",
      "fit <- gsl_nls(sqrtGR ~ CMAW(aw, AWmin, MUopt, AWopt),",
      "  data = aw,",
      "  start = list(AWmin = 0.89, MUopt = 1.0, AWopt = 0.98))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_aw <- data.frame(aw = seq(min(aw$aw), max(aw$aw), length.out = 200))",
      "pred <- predict(fit, newdata = new_aw)",
      "plot(sqrtGR ~ aw, data = aw, xlab = \"Water activity\", ylab = \"sqrt GR\")",
      "lines(new_aw$aw, pred, col = \"blue\", lwd = 2)",
      "points(aw$aw, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    CMPH = c(
      "CMPH (cardinal pH):",
      "data(ph)",
      "fit <- gsl_nls(sqrtGR ~ CMPH(pH, pHmax, pHmin, MUopt, pHopt),",
      "  data = ph,",
      "  start = list(pHmax = 9, pHmin = 3, MUopt = 1.0, pHopt = 7))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_ph <- data.frame(pH = seq(min(ph$pH), max(ph$pH), length.out = 200))",
      "pred <- predict(fit, newdata = new_ph)",
      "plot(sqrtGR ~ pH, data = ph, xlab = \"pH\", ylab = \"sqrt GR\")",
      "lines(new_ph$pH, pred, col = \"blue\", lwd = 2)",
      "points(ph$pH, fitted_vals, pch = 16, col = \"grey40\")"
    ),
    CMInh = c(
      "CMInh (cardinal inhibitor):",
      "data(inh)",
      "fit <- gsl_nls(sqrtGR ~ CMInh(Conce, MIC, MUopt, alpha),",
      "  data = inh,",
      "  start = list(MIC = 0.89, MUopt = 1.0, alpha = 1))",
      "summary(fit)",
      "fitted_vals <- fitted(fit)",
      "new_conc <- data.frame(Conce = seq(min(inh$Conce), max(inh$Conce), length.out = 200))",
      "pred <- predict(fit, newdata = new_conc)",
      "plot(sqrtGR ~ Conce, data = inh, xlab = \"Inhibitor concentration\", ylab = \"sqrt GR\")",
      "lines(new_conc$Conce, pred, col = \"blue\", lwd = 2)",
      "points(inh$Conce, fitted_vals, pch = 16, col = \"grey40\")"
    )
  )
}

predmicror_assist_code_template <- function(candidates) {
  if (length(candidates) == 0) {
    return("")
  }
  templates <- predmicror_assist_template_map()
  for (name in candidates) {
    if (name %in% names(templates)) {
      lines <- templates[[name]]
      if (length(lines) > 0 && grepl(":$", lines[1])) {
        lines <- lines[-1]
      }
      return(paste(lines, collapse = "\n"))
    }
  }
  ""
}

predmicror_assist_example_templates <- function(candidates, flags) {
  lines <- character(0)

  lines <- c(
    lines,
    "Start values are initial guesses; parameters are estimated by gsl_nls.",
    "For model fitting questions, include summary(), fitted values, and a plot."
  )

  templates <- predmicror_assist_template_map()

  if (length(candidates) == 0) {
    return(paste(lines, collapse = "\n"))
  }

  for (name in candidates) {
    if (name %in% names(templates)) {
      lines <- c(lines, "", templates[[name]])
    }
  }

  paste(lines, collapse = "\n")
}

predmicror_assist_extract_title <- function(lines, idx) {
  if (idx <= 1) {
    return("")
  }
  block <- character(0)
  j <- idx - 1
  while (j >= 1 && grepl("^\\s*#'", lines[j])) {
    block <- c(lines[j], block)
    j <- j - 1
  }
  if (length(block) == 0) {
    return("")
  }
  block <- sub("^\\s*#'\\s?", "", block)
  block <- trimws(block)
  block <- block[nzchar(block) & !grepl("^@", block)]
  if (length(block) == 0) {
    return("")
  }
  block[1]
}

predmicror_assist_select_models <- function(query, model_index) {
  q <- tolower(query)
  candidates <- character(0)

  if (length(model_index$names) > 0) {
    for (name in model_index$names) {
      if (grepl(paste0("\\b", tolower(name), "\\b"), q)) {
        candidates <- c(candidates, name)
      }
    }
  }

  add_if <- function(pattern, names) {
    if (grepl(pattern, q)) {
      candidates <<- c(candidates, names)
    }
  }

  add_if("\\bhuang\\b", c("HuangFM", "HuangRM", "HuangNLM"))
  add_if("\\bgompertz\\b", "HuangRGS")
  add_if("\\bbaranyi\\b", c("BaranyiFM", "BaranyiRM"))
  add_if("\\bzwietering\\b", "ZwieteringFM")
  add_if("\\bbuchanan\\b", "BuchananRM")
  add_if("\\bfang\\b", "FangNLM")
  add_if("\\brichards\\b", "RichardsNLM")
  add_if("\\brosso\\b", "RossoFM")
  add_if("\\bweibull\\b", c("WeibullM", "WeibullMM", "WeibullPH"))
  add_if("\\bgeeraerd\\b", "GeeraerdST")
  add_if("\\binactivation\\b|\\bsurvival\\b|\\bkill\\b|\\blethal\\b",
    c("WeibullM", "WeibullMM", "WeibullPH", "GeeraerdST", "HuangRGS")
  )
  add_if("\\bcardinal\\b", c("CMTI", "CMAW", "CMPH", "CMInh"))
  add_if("\\bph\\b", "CMPH")
  add_if("\\baw\\b|water activity", "CMAW")
  add_if("\\btemperature\\b|\\btemp\\b", "CMTI")
  add_if("\\binhibitor\\b|\\bmic\\b|\\binh\\b", "CMInh")

  candidates <- unique(candidates)
  if (length(model_index$names) > 0) {
    candidates <- intersect(candidates, model_index$names)
  }

  prefer_suffix <- function(pattern, suffix) {
    if (grepl(pattern, q)) {
      preferred <- grep(paste0(suffix, "$"), candidates, value = TRUE)
      others <- setdiff(candidates, preferred)
      candidates <<- c(preferred, others)
    }
  }

  prefer_suffix("no\\s*lag|nolag", "NLM")
  prefer_suffix("\\breduced\\b", "RM")
  prefer_suffix("\\bfull\\b", "FM")

  candidates
}

predmicror_assist_query_flags <- function(query) {
  q <- tolower(query)
  flags <- character(0)
  if (grepl("\\bfit\\b|fitting|gsl_nls|estimate|calibrat", q)) {
    flags <- c(flags, "fit")
  }
  if (grepl("\\bln\\b|natural log", q)) {
    flags <- c(flags, "ln")
  }
  if (grepl("no\\s*lag|nolag", q)) {
    flags <- c(flags, "no lag")
  }
  if (grepl("\\breduced\\b", q)) {
    flags <- c(flags, "reduced")
  }
  if (grepl("\\bfull\\b", q)) {
    flags <- c(flags, "full")
  }
  if (grepl("\\binactivation\\b|\\bsurvival\\b|\\bkill\\b", q)) {
    flags <- c(flags, "inactivation")
  }
  if (grepl("\\bgrowth\\b", q)) {
    flags <- c(flags, "growth")
  }
  if (grepl("\\bcardinal\\b", q)) {
    flags <- c(flags, "cardinal")
  }
  flags
}

predmicror_assist_collect_context <- function(query, root = ".", max_chars = 8000) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  files <- c(
    file.path(root, "README.md"),
    file.path(root, "README.Rmd")
  )
  vignettes_dir <- file.path(root, "vignettes")
  if (dir.exists(vignettes_dir)) {
    files <- c(files, list.files(vignettes_dir,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "\\.(Rmd|md)$"
    ))
  }
  man_dir <- file.path(root, "man")
  if (dir.exists(man_dir)) {
    files <- c(files, list.files(man_dir,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "\\.Rd$"
    ))
  }
  r_dir <- file.path(root, "R")
  if (dir.exists(r_dir)) {
    files <- c(files, list.files(r_dir,
      recursive = TRUE,
      full.names = TRUE,
      pattern = "\\.R$"
    ))
  }
  files <- unique(files[file.exists(files)])

  tokens <- unlist(strsplit(tolower(query), "[^a-z0-9]+"))
  tokens <- tokens[nchar(tokens) > 2]
  tokens <- unique(tokens)
  if (length(tokens) > 8) {
    tokens <- tokens[1:8]
  }
  pattern <- if (length(tokens) > 0) paste(tokens, collapse = "|") else ""

  context_lines <- character(0)
  if (nzchar(pattern) && length(files) > 0 && nzchar(Sys.which("rg"))) {
    args <- c("-n", "-C", "2", "-m", "2", "-e", pattern, files)
    context_lines <- tryCatch(system2("rg", args, stdout = TRUE, stderr = TRUE),
      error = function(e) character(0)
    )
  } else if (nzchar(pattern) && length(files) > 0 && nzchar(Sys.which("grep"))) {
    args <- c("-n", "-C", "2", "-m", "2", "-E", pattern, files)
    context_lines <- tryCatch(system2("grep", args, stdout = TRUE, stderr = TRUE),
      error = function(e) character(0)
    )
  }
  if (length(context_lines) == 0 && nzchar(pattern) && length(files) > 0) {
    context_lines <- predmicror_assist_collect_context_lines(
      files,
      pattern,
      context = 2,
      max_matches = 2
    )
  }

  if (length(context_lines) == 0) {
    readme <- file.path(root, "README.md")
    if (file.exists(readme)) {
      context_lines <- tryCatch(readLines(readme, warn = FALSE),
        error = function(e) character(0)
      )
    }
  }

  fun_lines <- character(0)
  if (dir.exists(r_dir)) {
    r_files <- list.files(r_dir, full.names = TRUE, pattern = "\\.R$")
    r_files <- r_files[!basename(r_files) %in% c(
      "assistant.R",
      "assistant_app.R",
      "zzz.R",
      "predmicror-package.R"
    )]
    for (path in r_files) {
      lines <- tryCatch(readLines(path, warn = FALSE),
        error = function(e) character(0)
      )
      if (length(lines) > 0) {
        matches <- grep("<- function", lines, value = TRUE)
        fun_lines <- c(fun_lines, matches)
      }
    }
  }
  fun_names <- unique(sub(" <- function.*", "", fun_lines))
  fun_names <- fun_names[nzchar(fun_names)]
  if (length(fun_names) > 0) {
    fun_block <- paste(sort(fun_names), collapse = ", ")
    context_lines <- c(context_lines, "", "Available functions:", fun_block)
  }

  context <- paste(context_lines, collapse = "\n")
  if (nchar(context) > max_chars) {
    context <- substr(context, 1, max_chars)
  }
  context
}

predmicror_assist_build_prompt <- function(query, context, model_index, candidates, flags, history, templates) {
  fit_instructions <- ""
  fit_format <- ""
  fit_requested <- "fit" %in% flags || "cardinal" %in% flags
  if (fit_requested) {
    fit_instructions <- paste(
      "If the user is asking to fit a model, provide a complete R example:",
      "load data, fit with gsl_nls, run summary(fit), extract fitted values,",
      "generate predictions on a smooth time grid, and plot observed vs fitted.",
      "Keep it in a single fenced R code block for learnr.",
      sep = " "
    )
    fit_format <- "Include summary(), fitted values, and a plot when fitting.\n"
  }
  dataset_note <- ""
  if (fit_requested) {
    dataset_note <- paste(
      "If you use a built-in dataset, mention its name and that the response",
      "column is lnN for growth datasets."
    )
  }
  system_prompt <- paste(
    "You are the predmicror assistant.",
    "Use ONLY functions listed in the Model Catalog.",
    "Pick the best matching function(s) based on the question and flags.",
    "If a model family (e.g., Huang) is requested without specifying 'full', 'reduced', or 'no-lag', default to the 'full' variant.",
    "When a cardinal model is requested (e.g., 'cardinal temperature'), prioritize the specific cardinal model based on the input factor (e.g., 'temperature' implies 'CMTI', 'pH' implies 'CMPH').",
    "If the query mentions full/reduced/no-lag, choose FM/RM/NLM variants.",
    "If the question is about growth curves, do not ask for unrelated cardinal inputs.",
    "If an Example template is available for the selected model, reuse it as the main code block (do not omit steps).",
    "Provide a gsl_nls call with named arguments and reasonable start values.",
    "When providing a gsl_nls example, use concrete data loading (e.g., 'data(growthfull)' or 'data(bixina)') from the package when applicable, instead of generic placeholders like 'data <- ...'.",
    "If the user does not supply a dataset, default to a relevant package example dataset for the chosen model.",
    "If you use predmicror growth example datasets (growthfull, growthnolag, growthred), the response column is lnN (natural log). Use lnN on the left-hand side of the formula.",
    "For cardinal models, use sqrtGR as the response and the matching example dataset and column name: CMTI -> salmonella (Temp), CMAW -> aw (aw), CMPH -> ph (pH), CMInh -> inh (Conce).",
    "If you show package loading, use 'library(predmicror)' and do not call library() on model functions.",
    "If you include code, use a single fenced R code block and avoid other code blocks.",
    "Do not ask the user to provide model parameters like Y0, Ymax, MUmax, lag; those are estimated.",
    dataset_note,
    "Only ask for missing dataset details (time column, response column, units) if they cannot be inferred from the context or flags.",
    "Warn about parameter constraints (e.g., Tmin < Topt < Tmax, aw <= 1).",
    "Do not invent parameters not present in the chosen function signature.",
    "Never answer with only 'need more information'. Always provide a concrete example.",
    fit_instructions,
    sep = "\n"
  )

  model_catalog <- if (length(model_index$entries) > 0) {
    paste(model_index$entries, collapse = "\n")
  } else {
    "No model catalog available."
  }
  candidate_block <- if (length(candidates) > 0) {
    paste(candidates, collapse = ", ")
  } else {
    "None detected."
  }
  flags_line <- if (length(flags) > 0) {
    paste(flags, collapse = ", ")
  } else {
    "none"
  }

  history_block <- ""
  if (nzchar(history)) {
    history_block <- paste("\n\nConversation so far:\n", history, sep = "")
  }
  template_block <- ""
  if (nzchar(templates)) {
    template_block <- paste("\n\nExample templates:\n", templates, sep = "")
  }

  paste(
    system_prompt,
    "\n\nQuery flags:\n",
    flags_line,
    "\n\nCandidate functions:\n",
    candidate_block,
    "\n\nModel Catalog:\n",
    model_catalog,
    history_block,
    template_block,
    "\n\nContext:\n",
    context,
    "\n\nUser question:\n",
    query,
    "\n\nAnswer format: Model choice; data prep; gsl_nls example in a fenced R code block; optional clarifying question about columns/time units.\n",
    fit_format,
    "Answer:",
    sep = ""
  )
}

predmicror_assist_clean_output <- function(text) {
  # Strip ANSI control sequences, control pictures, and braille spinners.
  cleaned <- gsub("\u001b\\[[0-9;?]*[ -/]*[@-~]", "", text)
  cleaned <- gsub("[\u2400-\u243F]", "", cleaned)
  cleaned <- gsub("[\u2800-\u28FF]", "", cleaned)
  cleaned <- gsub("\r", "\n", cleaned, fixed = TRUE)
  lines <- unlist(strsplit(cleaned, "\n", fixed = TRUE))
  lines <- gsub("[ \t]+$", "", lines)
  lines <- lines[grepl("\\S", lines)]
  if (length(lines) == 0) {
    return("")
  }
  lines <- lines[!grepl("^sh:\\s*.*not found$", lines)]
  paste(lines, collapse = "\n")
}

predmicror_assist_code_uses_examples <- function(code_text) {
  if (!nzchar(code_text)) {
    return(FALSE)
  }
  if (grepl("\\bdata\\((growthfull|growthnolag|growthred|salmonella|aw|ph|inh|bixina|mafart2005Li11)\\)", code_text)) {
    return(TRUE)
  }
  if (grepl("\\b(growthfull|growthnolag|growthred|salmonella|bixina|mafart2005Li11)\\b", code_text)) {
    return(TRUE)
  }
  if (grepl("\\bCMAW\\b", code_text) && grepl("\\baw\\b", code_text)) {
    return(TRUE)
  }
  if (grepl("\\bCMPH\\b", code_text) && grepl("\\bpH\\b|\\bph\\b", code_text)) {
    return(TRUE)
  }
  if (grepl("\\bCMInh\\b", code_text) && grepl("\\bConce\\b|\\binh\\b", code_text)) {
    return(TRUE)
  }
  FALSE
}

predmicror_assist_normalize_response <- function(text) {
  if (!nzchar(text)) {
    return(text)
  }
  if (grepl("\\bdata\\((growthfull|growthnolag|growthred)\\)", text)) {
    text <- gsub("\\bN\\s*~", "lnN ~", text)
  }
  if (grepl("\\bCMTI\\b|\\bCMAW\\b|\\bCMPH\\b|\\bCMInh\\b", text) ||
      grepl("\\bdata\\((salmonella|aw|ph|inh)\\)", text)) {
    text <- gsub("\\blnN\\s*~", "sqrtGR ~", text)
    text <- gsub("\\bN\\s*~", "sqrtGR ~", text)
  }
  text
}
