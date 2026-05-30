library(shiny)

resolve_assistant <- function() {
  if (requireNamespace("predmicror", quietly = TRUE)) {
    return(list(fn = predmicror::predmicror_assistant, root = NULL))
  }
  app_root <- normalizePath(file.path(getwd(), "..", "..", ".."), winslash = "/", mustWork = FALSE)
  local_file <- file.path(app_root, "R", "assistant.R")
  if (!file.exists(local_file)) {
    stop("predmicror_assistant not found. Install predmicror or run from the repo root.")
  }
  source(local_file)
  list(fn = predmicror_assistant, root = app_root)
}

assistant <- resolve_assistant()

load_assist_utils <- function(root = NULL) {
  if (requireNamespace("predmicror", quietly = TRUE)) {
    return(list(
      extract_code_blocks = predmicror:::predmicror_assist_extract_code_blocks,
      normalize_text = predmicror:::predmicror_assist_normalize_text,
      extract_inline_code = predmicror:::predmicror_assist_extract_inline_code,
      strip_inline_code = predmicror:::predmicror_assist_strip_inline_code,
      strip_code_blocks = predmicror:::predmicror_assist_strip_code_blocks
    ))
  }
  if (!is.null(root)) {
    utils_file <- file.path(root, "R", "assistant_utils.R")
    if (file.exists(utils_file)) {
      source(utils_file)
      return(list(
        extract_code_blocks = predmicror_assist_extract_code_blocks,
        normalize_text = predmicror_assist_normalize_text,
        extract_inline_code = predmicror_assist_extract_inline_code,
        strip_inline_code = predmicror_assist_strip_inline_code,
        strip_code_blocks = predmicror_assist_strip_code_blocks
      ))
    }
  }
  stop("predmicror assistant utilities not found. Install predmicror or run from the repo root.")
}

assist_utils <- load_assist_utils(assistant$root)
extract_code_blocks <- assist_utils$extract_code_blocks
normalize_code_text <- assist_utils$normalize_text
extract_inline_code <- assist_utils$extract_inline_code
strip_inline_code <- assist_utils$strip_inline_code
strip_code_blocks <- assist_utils$strip_code_blocks

list_ollama_models <- function() {
  if (!nzchar(Sys.which("ollama"))) {
    return(character(0))
  }
  out <- tryCatch(system2("ollama", "list", stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  if (length(out) < 2) {
    return(character(0))
  }
  lines <- out[-1]
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) {
    return(character(0))
  }
  parts <- strsplit(lines, "\\s+")
  names <- vapply(parts, `[`, 1, FUN.VALUE = character(1), USE.NAMES = FALSE)
  names[nzchar(names)]
}

find_predmicror_logo <- function(root = NULL) {
  if (requireNamespace("predmicror", quietly = TRUE)) {
    pkg_logo <- system.file(
      "shiny", "predmicror-assistant", "www", "logo.png",
      package = "predmicror"
    )
    if (nzchar(pkg_logo) && file.exists(pkg_logo)) {
      return(pkg_logo)
    }
  }
  if (!is.null(root)) {
    repo_logo <- file.path(root, "inst", "shiny", "predmicror-assistant", "www", "logo.png")
    if (file.exists(repo_logo)) {
      return(repo_logo)
    }
  }
  cwd_www <- file.path(getwd(), "www", "logo.png")
  if (file.exists(cwd_www)) {
    return(cwd_www)
  }
  cwd_repo <- file.path(getwd(), "inst", "shiny", "predmicror-assistant", "www", "logo.png")
  if (file.exists(cwd_repo)) {
    return(cwd_repo)
  }
  cwd_logo <- file.path(getwd(), "logo.png")
  if (file.exists(cwd_logo)) {
    return(cwd_logo)
  }
  ""
}

predmicror_function_names <- function(root = NULL) {
  if (requireNamespace("predmicror", quietly = TRUE)) {
    return(getNamespaceExports("predmicror"))
  }
  if (is.null(root)) {
    return(character(0))
  }
  r_dir <- file.path(root, "R")
  if (!dir.exists(r_dir)) {
    return(character(0))
  }
  r_files <- list.files(r_dir, full.names = TRUE, pattern = "\\.R$")
  names <- character(0)
  for (path in r_files) {
    lines <- tryCatch(readLines(path, warn = FALSE),
      error = function(e) character(0)
    )
    if (length(lines) == 0) {
      next
    }
    matches <- regexec("^\\s*([A-Za-z0-9_.]+)\\s*<-\\s*function\\s*\\(", lines)
    parts <- regmatches(lines, matches)
    hits <- vapply(parts, function(item) if (length(item) > 1) item[2] else "", "")
    names <- c(names, hits[nzchar(hits)])
  }
  unique(names)
}

sanitize_learnr_code <- function(code_text, function_names) {
  if (!nzchar(code_text) || length(function_names) == 0) {
    return(normalize_code_text(code_text))
  }
  code_text <- normalize_code_text(code_text)
  lines <- strsplit(code_text, "\n", fixed = TRUE)[[1]]
  cleaned <- vapply(lines, function(line) {
    match <- regexec("^\\s*(library|require)\\s*\\(([^)]+)\\)\\s*$", line)
    parts <- regmatches(line, match)[[1]]
    if (length(parts) > 0) {
      pkg <- gsub("^['\"]|['\"]$", "", trimws(parts[3]))
      if (pkg == "predmicror" || pkg %in% function_names) {
        return("predmicror_attach()")
      }
    }
    line
  }, character(1))
  paste(cleaned, collapse = "\n")
}

normalize_example_dataset <- function(code_text) {
  if (!nzchar(code_text)) {
    return(code_text)
  }
  if (!grepl("\\bgrowthdata\\b|\\bgrowth_data\\b", code_text, ignore.case = TRUE)) {
    return(code_text)
  }
  dataset <- "growthfull"
  if (grepl("\\bHuangNLM\\b|\\bFangNLM\\b|\\bRichardsNLM\\b", code_text)) {
    dataset <- "growthnolag"
  } else if (grepl("\\bHuangRM\\b|\\bBaranyiRM\\b|\\bBuchananRM\\b", code_text)) {
    dataset <- "growthred"
  }
  code_text <- gsub("(?i)\\bgrowthdata\\b", dataset, code_text, perl = TRUE)
  code_text <- gsub("(?i)\\bgrowth_data\\b", dataset, code_text, perl = TRUE)
  if (!grepl("\\bdata\\s*\\(", code_text)) {
    code_text <- paste0("data(", dataset, ")\n", code_text)
  }
  code_text
}

split_learnr_sections <- function(code_text) {
  if (!nzchar(code_text)) {
    return(list())
  }
  lines <- strsplit(code_text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) {
    return(list())
  }
  trimmed <- trimws(lines)
  first_idx <- function(pattern) {
    idx <- which(grepl(pattern, trimmed))
    if (length(idx) == 0) {
      return(NA_integer_)
    }
    idx[1]
  }

  idx_summary <- first_idx("^summary\\s*\\(")
  idx_fitted <- first_idx("^fitted\\s*\\(|^fitted_vals\\s*<-")
  idx_pred <- first_idx("^(new_times|newTimes)\\s*<-|^pred\\s*<-|predict\\s*\\(")
  idx_plot <- first_idx("^(plot|lines|points)\\s*\\(")

  pred_start <- suppressWarnings(min(c(idx_fitted, idx_pred), na.rm = TRUE))
  if (is.infinite(pred_start)) {
    pred_start <- NA_integer_
  }

  starts <- c(1, idx_summary, pred_start, idx_plot)
  starts <- sort(unique(starts[!is.na(starts)]))
  if (length(starts) <= 1) {
    return(list(list(type = "fit", body = code_text)))
  }

  infer_type <- function(body) {
    if (grepl("\\bsummary\\s*\\(", body)) {
      return("summary")
    }
    if (grepl("\\bplot\\s*\\(|\\blines\\s*\\(|\\bpoints\\s*\\(", body)) {
      return("plot")
    }
    if (grepl("fitted\\s*\\(|fitted_vals\\s*<-|new_times|newTimes|predict\\s*\\(", body)) {
      return("predict")
    }
    "fit"
  }

  sections <- list()
  for (i in seq_along(starts)) {
    start <- starts[i]
    end <- if (i < length(starts)) starts[i + 1] - 1 else length(lines)
    body_lines <- lines[start:end]
    while (length(body_lines) > 0 && trimws(body_lines[1]) == "") {
      body_lines <- body_lines[-1]
    }
    while (length(body_lines) > 0 && trimws(body_lines[length(body_lines)]) == "") {
      body_lines <- body_lines[-length(body_lines)]
    }
    if (length(body_lines) == 0) {
      next
    }
    body <- paste(body_lines, collapse = "\n")
    sections[[length(sections) + 1]] <- list(
      type = infer_type(body),
      body = body
    )
  }
  sections
}

build_learnr_tutorial <- function(answer_text,
                                  code_text,
                                  function_names = character(0),
                                  app_root = NULL) {
  tutorial_dir <- tempfile("predmicror-learnr-")
  dir.create(tutorial_dir, recursive = TRUE)
  tutorial_file <- file.path(tutorial_dir, "tutorial.Rmd")
  css_file <- file.path(tutorial_dir, "predmicror-learnr.css")

  css_rules <- c(
    "body { background: #f6f4f1; }",
    ".tutorial-navbar, .tutorial-footer, .tutorial-nav, .tutorial-sidebar,",
    "#tutorial-navbar, #tutorial-footer, #tutorial-nav, #tutorial-sidebar {",
    "  display: none !important;",
    "}",
    "main, .tutorial-content, .tutorial-main, .tutorial-body {",
    "  max-width: 920px; width: 100%; margin: 0 auto;",
    "}",
    ".tutorial-content, main { padding: 1.5rem 1.75rem; }",
    ".tutorial-content > * { max-width: 100%; }",
    "h1, h2, h3 { letter-spacing: 0.01em; }",
    "pre {",
    "  background: #1f2933; color: #f8f8f2; border-radius: 10px; padding: 0.9rem;",
    "  max-width: 100%; overflow-x: auto; white-space: pre-wrap; word-break: break-word;",
    "}",
    "pre code { white-space: inherit; }",
    "code { font-size: 0.95em; }",
    ".predmicror-hero {",
    "  display: flex; gap: 1rem; align-items: center;",
    "  padding: 1rem 1.25rem; border-radius: 16px;",
    "  background: linear-gradient(135deg, #fff4e6 0%, #f1f7f1 100%);",
    "  border: 1px solid #eadfce; margin-bottom: 1.5rem;",
    "}",
    ".predmicror-hero img {",
    "  width: 56px; height: auto; background: #ffffff; border-radius: 12px;",
    "  padding: 0.2rem; box-shadow: 0 6px 12px rgba(16, 24, 40, 0.12);",
    "}",
    ".predmicror-hero-title { font-size: 1.5rem; font-weight: 700; margin: 0; }",
    ".predmicror-hero-sub { color: #5b5f62; margin: 0.2rem 0 0 0; }",
    ".predmicror-step {",
    "  background: #ffffff; border-radius: 14px;",
    "  padding: 1rem 1.25rem; margin: 1rem 0;",
    "  border: 1px solid #e6e1d8;",
    "  box-shadow: 0 8px 20px rgba(16, 24, 40, 0.06);",
    "}",
    ".predmicror-step h3 { margin-top: 0; }",
    ".tutorial-exercise { margin-top: 0.75rem; }",
    "@media (max-width: 700px) {",
    "  .predmicror-hero { flex-direction: column; align-items: flex-start; }",
    "  .predmicror-hero img { width: 48px; }",
    "}"
  )
  writeLines(css_rules, css_file)

  logo_path <- find_predmicror_logo(app_root)
  logo_name <- ""
  if (nzchar(logo_path) && file.exists(logo_path)) {
    logo_name <- "logo.png"
    file.copy(logo_path, file.path(tutorial_dir, logo_name), overwrite = TRUE)
  }

  explanation <- strip_code_blocks(answer_text)
  if (!nzchar(trimws(explanation))) {
    explanation <- "No explanation text available."
  }
  used_inline <- FALSE
  if (!nzchar(trimws(code_text))) {
    code_text <- extract_inline_code(answer_text)
    used_inline <- nzchar(trimws(code_text))
  }
  if (!nzchar(trimws(code_text))) {
    code_text <- paste(
      "# No R code block detected in the assistant answer.",
      "# Paste or edit code here before running.",
      sep = "\n"
    )
  }
  code_text <- sanitize_learnr_code(code_text, function_names)
  code_text <- normalize_example_dataset(code_text)
  dataset_name <- ""
  if (nzchar(code_text)) {
    match <- regexec("\\bdata\\s*\\(\\s*([A-Za-z0-9_.]+)\\s*\\)", code_text)
    parts <- regmatches(code_text, match)[[1]]
    if (length(parts) > 1) {
      dataset_name <- parts[2]
    }
  }
  if (used_inline) {
    explanation <- strip_inline_code(explanation)
    if (!nzchar(trimws(explanation))) {
      explanation <- "Example code recovered from the assistant response."
    }
  }
  sections <- split_learnr_sections(code_text)
  fit_section <- NULL
  predict_section <- NULL
  if (length(sections) > 1) {
    for (section in sections) {
      if (identical(section$type, "fit")) {
        fit_section <- section
      }
      if (is.null(predict_section) && identical(section$type, "predict")) {
        predict_section <- section
      }
    }
  }
  fit_helper <- character(0)
  if (!is.null(fit_section)) {
    fit_lines <- strsplit(fit_section$body, "\n", fixed = TRUE)[[1]]
    fit_targets <- character(0)
    for (line in fit_lines) {
      match <- regexec("^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*<-", line)
      parts <- regmatches(line, match)[[1]]
      if (length(parts) > 1) {
        fit_targets <- c(fit_targets, parts[2])
      }
    }
    fit_targets <- unique(fit_targets)
    fit_assign_block <- character(0)
    if (length(fit_targets) > 0) {
      target_list <- paste(sprintf("\"%s\"", fit_targets), collapse = ", ")
      fit_assign_block <- c(
        paste0("    for (name in c(", target_list, ")) {"),
        "      if (exists(name, inherits = FALSE)) {",
        "        assign(name, get(name, inherits = FALSE), envir = .GlobalEnv)",
        "      }",
        "    }"
      )
    }
    fit_helper <- c(
      "  predmicror_step_fit <- function() {",
      paste0("    ", fit_lines),
      fit_assign_block,
      "    if (exists(\"fit\", inherits = FALSE)) {",
      "      assign(\"fit\", fit, envir = .GlobalEnv)",
      "    }",
      "    fit",
      "  }"
    )
  }
  predict_helper <- character(0)
  if (!is.null(predict_section) && !is.null(fit_section)) {
    predict_lines <- strsplit(predict_section$body, "\n", fixed = TRUE)[[1]]
    assign_targets <- character(0)
    for (line in predict_lines) {
      match <- regexec("^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*<-", line)
      parts <- regmatches(line, match)[[1]]
      if (length(parts) > 1) {
        assign_targets <- c(assign_targets, parts[2])
      }
    }
    assign_targets <- unique(setdiff(assign_targets, "fit"))
    assign_block <- character(0)
    if (length(assign_targets) > 0) {
      target_list <- paste(sprintf("\"%s\"", assign_targets), collapse = ", ")
      assign_block <- c(
        paste0("    for (name in c(", target_list, ")) {"),
        "      if (exists(name, inherits = FALSE)) {",
        "        assign(name, get(name, inherits = FALSE), envir = .GlobalEnv)",
        "      }",
        "    }"
      )
    }
    predict_helper <- c(
      "  predmicror_step_predict <- function() {",
      "    if (!exists(\"fit\", inherits = FALSE)) {",
      "      predmicror_step_fit()",
      "    }",
      paste0("    ", predict_lines),
      assign_block,
      "  }"
    )
  }

  section_meta <- list(
    fit = list(
      title = "Load data and fit the model",
      blurb = paste(
        "Load the example dataset and fit the model with gsl_nls().",
        "Start values are initial guesses that help the optimizer converge."
      )
    ),
    summary = list(
      title = "Inspect the fitted model",
      blurb = paste(
        "Review parameter estimates, standard errors, and residual diagnostics",
        "using summary(fit)."
      )
    ),
    predict = list(
      title = "Compute fitted values and predictions",
      blurb = paste(
        "Extract fitted values and generate a smooth prediction curve on a",
        "time grid to see the model trend."
      )
    ),
    plot = list(
      title = "Plot observed vs fitted",
      blurb = paste(
        "Plot the observed data and overlay the fitted curve and fitted points",
        "to check how well the model matches the observations."
      )
    )
  )

  hero_block <- c(
    "<div class=\"predmicror-hero\">",
    if (nzchar(logo_name)) {
      paste0("<img src=\"", logo_name, "\" alt=\"predmicror logo\" />")
    } else {
      ""
    },
    "<div>",
    "<p class=\"predmicror-hero-title\">predmicror assistant</p>",
    "<p class=\"predmicror-hero-sub\">Run, review, and refine model fits step by step.</p>",
    "</div>",
    "</div>"
  )
  hero_block <- hero_block[nzchar(hero_block)]
  hero_literal <- encodeString(paste(hero_block, collapse = "\n"), quote = "\"")
  dataset_block <- c(
    "## Dataset preview",
    "",
    "```{r dataset-preview}",
    "if (nzchar(predmicror_dataset)) {",
    "  if (predmicror_load_dataset(predmicror_dataset)) {",
    "    ds <- get(predmicror_dataset, inherits = FALSE)",
    "    print(utils::head(ds))",
    "    print(summary(ds))",
    "  } else {",
    "    message(\"Dataset not found in environment: \", predmicror_dataset)",
    "  }",
    "} else {",
    "  message(\"No dataset detected in the assistant response.\")",
    "}",
    "```",
    ""
  )
  reset_block <- c(
    "## Reset your session",
    "",
    "Run this if you want to clear fitted objects and start over.",
    "",
    "```{r reset-session, exercise=TRUE}",
    "predmicror_reset_env()",
    "```",
    ""
  )

  content <- c(
    "---",
    "title: \"predmicror assistant\"",
    "output: learnr::tutorial",
    "runtime: shiny_prerendered",
    "css: \"predmicror-learnr.css\"",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "suppressPackageStartupMessages({",
    "  predmicror_attach <- function() {",
    "    if (requireNamespace(\"predmicror\", quietly = TRUE)) {",
    "      library(predmicror)",
    "      return(invisible(TRUE))",
    "    }",
    "    predmicror_root <- Sys.getenv(\"PREDMICROR_APP_ROOT\", \"\")",
    "    if (nzchar(predmicror_root)) {",
    "      r_dir <- file.path(predmicror_root, \"R\")",
    "      if (dir.exists(r_dir)) {",
    "        r_files <- list.files(r_dir, pattern = \"\\\\.R$\", full.names = TRUE)",
    "        for (path in r_files) {",
    "          source(path)",
    "        }",
    "      }",
    "      data_dir <- file.path(predmicror_root, \"data\")",
    "      if (dir.exists(data_dir)) {",
    "        data_files <- list.files(",
    "          data_dir,",
    "          pattern = \"\\\\.(rda|RData)$\",",
    "          full.names = TRUE,",
    "          ignore.case = TRUE",
    "        )",
    "        for (path in data_files) {",
    "          load(path, envir = .GlobalEnv)",
    "        }",
    "      }",
    "      return(invisible(FALSE))",
    "    }",
    "    message(\"predmicror not available; install it or run from the repo root.\")",
    "    invisible(FALSE)",
    "  }",
    "  predmicror_attach()",
    "  predmicror_app_root <- Sys.getenv(\"PREDMICROR_APP_ROOT\", \"\")",
    paste0("  predmicror_dataset <- \"", dataset_name, "\""),
    "  predmicror_load_dataset <- function(name) {",
    "    if (!nzchar(name)) {",
    "      return(FALSE)",
    "    }",
    "    if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {",
    "      return(TRUE)",
    "    }",
    "    if (requireNamespace(\"predmicror\", quietly = TRUE)) {",
    "      try(utils::data(list = name, package = \"predmicror\", envir = .GlobalEnv), silent = TRUE)",
    "    }",
    "    if (!exists(name, envir = .GlobalEnv, inherits = FALSE) && nzchar(predmicror_app_root)) {",
    "      data_dir <- file.path(predmicror_app_root, \"data\")",
    "      if (dir.exists(data_dir)) {",
    "        pattern <- paste0(\"^\", name, \"\\\\.(rda|RData)$\")",
    "        data_files <- list.files(",
    "          data_dir,",
    "          pattern = pattern,",
    "          full.names = TRUE,",
    "          ignore.case = TRUE",
    "        )",
    "        for (path in data_files) {",
    "          load(path, envir = .GlobalEnv)",
    "        }",
    "      }",
    "    }",
    "    exists(name, envir = .GlobalEnv, inherits = FALSE)",
    "  }",
    "  predmicror_reset_env <- function() {",
    "    keep <- c(",
    "      \"predmicror_dataset\",",
    "      \"predmicror_attach\",",
    "      \"predmicror_load_dataset\",",
    "      \"predmicror_app_root\",",
    "      \"predmicror_reset_env\",",
    "      \"predmicror_step_fit\",",
    "      \"predmicror_step_predict\"",
    "    )",
    "    rm(list = setdiff(ls(envir = .GlobalEnv), keep), envir = .GlobalEnv)",
    "    predmicror_attach()",
    "  }",
    "  if (requireNamespace(\"gslnls\", quietly = TRUE)) {",
    "    library(gslnls)",
    "  }",
    fit_helper,
    predict_helper,
    "})",
    "```",
    "",
    "```{r hero, echo=FALSE, results='asis'}",
    paste0("cat(", hero_literal, ")"),
    "```",
    "",
    "## Assistant response",
    "",
    explanation,
    ""
  )
  content <- c(content, dataset_block)

  if (length(sections) <= 1) {
    content <- c(
      content,
      "## Try the code",
      "",
      "```{r exercise-1, exercise=TRUE}",
      code_text,
      "```"
    )
  } else {
    content <- c(content, "## Step-by-step example", "")
    for (i in seq_along(sections)) {
      section <- sections[[i]]
      meta <- section_meta[[section$type]]
      if (is.null(meta)) {
        meta <- list(title = paste("Step", i), blurb = "")
      }
      guard <- character(0)
      if (!is.null(fit_section) && !identical(section$type, "fit")) {
        guard <- c(
          "if (!exists(\"fit\", inherits = FALSE)) {",
          "  predmicror_step_fit()",
          "}"
        )
      }
      if (!is.null(predict_section) && identical(section$type, "plot")) {
        guard <- c(
          guard,
          "if (exists(\"predmicror_step_predict\", inherits = FALSE) &&",
          "    (!exists(\"new_times\", inherits = FALSE) || !exists(\"pred\", inherits = FALSE))) {",
          "  predmicror_step_predict()",
          "}"
        )
      }
      body_lines <- c(guard, section$body)
      content <- c(
        content,
        "::: {.predmicror-step}",
        paste0("### Step ", i, " - ", meta$title),
        "",
        meta$blurb,
        "",
        paste0("```{r step-", i, ", exercise=TRUE}"),
        body_lines,
        "```",
        ":::",
        ""
      )
    }
  }
  content <- c(content, reset_block)
  writeLines(content, tutorial_file)
  list(dir = tutorial_dir, file = tutorial_file)
}

default_model <- Sys.getenv("OLLAMA_MODEL", "llama3:instruct")
has_bslib <- requireNamespace("bslib", quietly = TRUE)
app_theme <- NULL
if (has_bslib) {
  app_theme <- bslib::bs_theme(bootswatch = "united")
}

disclaimer_text <- paste(
  "This is a tool for academic and research purposes.",
  "The models and their outputs are not guaranteed to be accurate.",
  "Always validate the results with experimental data."
)
disclaimer_control <- NULL
disclaimer_note <- NULL
if (has_bslib) {
  disclaimer_control <- actionButton("disclaimer_btn", "?", class = "btn btn-outline-primary btn-sm") |>
    bslib::popover(
      title = "Disclaimer",
      disclaimer_text
    )
} else {
  disclaimer_note <- div(class = "text-muted small mt-2", disclaimer_text)
}

logo_path <- find_predmicror_logo(assistant$root)
logo_src <- "logo.png"
if (nzchar(logo_path) && file.exists(logo_path)) {
  addResourcePath("predmicror-assets", dirname(logo_path))
  logo_src <- file.path("predmicror-assets", basename(logo_path))
}

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$style(
      HTML(
        paste(
          "body { background: linear-gradient(180deg, #f8f9fa 0%, #fff8f0 100%); }",
          ".brand-logo { height: 38px; width: auto; margin-right: 0.6rem;",
          "  background: rgba(255, 255, 255, 0.9); border-radius: 10px;",
          "  padding: 0.1rem 0.2rem; filter: drop-shadow(0 2px 4px rgba(0, 0, 0, 0.2)); }",
          ".app-hero { background: linear-gradient(135deg, #ffffff 0%, #f3f6ff 100%);",
          "  border-left: 4px solid var(--bs-primary);",
          "  border-radius: 14px; padding: 1.25rem 1.5rem; margin-bottom: 1.5rem;",
          "  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.06); }",
          ".app-title { margin: 0; font-weight: 700; }",
          ".app-subtitle { color: #6c757d; margin-top: 0.35rem; }",
          ".onboarding-card { background: #ffffff; border: 1px solid #e5e7eb; border-radius: 14px;",
          "  padding: 1rem 1.25rem; margin-bottom: 1rem;",
          "  box-shadow: 0 8px 20px rgba(16, 24, 40, 0.05); }",
          ".onboarding-steps { display: grid; gap: 0.75rem; }",
          ".onboarding-step { display: flex; gap: 0.75rem; align-items: flex-start; }",
          ".step-badge { width: 28px; height: 28px; border-radius: 999px; background: var(--bs-primary);",
          "  color: #ffffff; display: inline-flex; align-items: center; justify-content: center;",
          "  font-size: 0.85rem; font-weight: 600; }",
          ".step-title { font-weight: 600; margin-bottom: 0.1rem; }",
          ".example-row { display: flex; flex-wrap: wrap; gap: 0.5rem; }",
          ".example-label { color: #6c757d; font-size: 0.8rem; text-transform: uppercase;",
          "  letter-spacing: 0.04em; margin-top: 0.75rem; }",
          ".prompt-chip { border-radius: 999px; }",
          ".ask-panel { border: 1px solid #e5e7eb; border-radius: 12px; padding: 1.1rem; margin-top: 1.25rem; background: #ffffff; }",
          ".ask-panel textarea { min-height: 140px; }",
          ".ask-panel .form-control:focus { border-color: #93c5fd; box-shadow: 0 0 0 0.15rem rgba(59, 130, 246, 0.2); }",
          ".ask-actions { display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 0.5rem; }",
          ".ask-status { display: inline-flex; align-items: center; gap: 0.4rem; color: #2563eb;",
          "  font-weight: 600; margin-top: 0.5rem; }",
          ".spinner { width: 12px; height: 12px; border-radius: 999px; border: 2px solid #cbd5e1;",
          "  border-top-color: #2563eb; animation: spin 0.9s linear infinite; }",
          "@keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }",
          ".ask-hint { color: #6c757d; font-size: 0.9rem; margin-top: 0.5rem; }",
          ".learnr-actions { display: flex; flex-wrap: wrap; gap: 0.5rem; margin-top: 0.5rem; }",
          ".qa-card { border: 1px solid #e5e7eb; border-radius: 12px; margin-bottom: 0.85rem;",
          "  background: #ffffff; box-shadow: 0 6px 16px rgba(0, 0, 0, 0.05); }",
          ".qa-card .card-body { padding: 1rem 1.1rem; }",
          ".qa-label { font-size: 0.8rem; color: #6c757d; text-transform: uppercase; letter-spacing: 0.04em; }",
          ".qa-question { font-weight: 600; margin-top: 0.25rem; }",
          ".qa-answer { margin-top: 0.5rem; white-space: pre-wrap; width: 100%; }",
          ".qa-answer pre { margin: 0; max-height: 420px; overflow: auto; width: 100%; }",
          ".qa-answer pre code { display: block; width: 100%; }",
          ".status-row { display: flex; flex-wrap: wrap; gap: 0.5rem; margin: 0.5rem 0 1rem; }",
          ".status-item { display: inline-flex; align-items: center; gap: 0.4rem; padding: 0.3rem 0.6rem;",
          "  border-radius: 999px; border: 1px solid #e5e7eb; background: #ffffff; font-size: 0.85rem; }",
          ".status-label { text-transform: uppercase; letter-spacing: 0.04em; font-size: 0.7rem; color: #6c757d; }",
          ".status-pill { display: inline-block; padding: 0.2rem 0.5rem; border-radius: 12px; font-size: 0.8rem; }",
          ".status-ok { background: var(--bs-success-bg-subtle); color: var(--bs-success-text-emphasis); }",
          ".status-warn { background: var(--bs-warning-bg-subtle); color: var(--bs-warning-text-emphasis); }",
          ".empty-state { color: #6c757d; font-style: italic; }",
          ".sidebar-card { background: #ffffff; border: 1px solid #e5e7eb; border-radius: 12px;",
          "  padding: 1rem; box-shadow: 0 6px 16px rgba(0, 0, 0, 0.05); }",
          "@media (max-width: 991px) {",
          "  .navbar .container { flex-wrap: wrap; gap: 0.5rem; }",
          "  .app-hero { padding: 1rem; }",
          "  .sidebar-card { margin-bottom: 1rem; }",
          "}",
          sep = "\n"
        )
      )
    ),
    tags$link(rel = "stylesheet", href = "default.min.css"),
    tags$script(src = "highlight.min.js"),
    tags$script(HTML(
      paste(
        "(function(){",
        "  function predmicrorHighlight(){",
        "    if (window.hljs) { hljs.highlightAll(); }",
        "  }",
        "  document.addEventListener('DOMContentLoaded', predmicrorHighlight);",
        "  document.addEventListener('shiny:connected', function(){",
        "    if (window.Shiny && Shiny.addCustomMessageHandler) {",
        "      Shiny.addCustomMessageHandler('hljs', function(){",
        "        setTimeout(predmicrorHighlight, 0);",
        "      });",
        "    }",
        "  });",
        "})();",
        sep = "\n"
      )
    ))
  ),
  tags$nav(
    class = "navbar navbar-expand-lg navbar-dark bg-primary mb-4",
    div(
      class = "container",
      tags$a(
        class = "navbar-brand d-flex align-items-center",
        href = "https://fsqanalytics.github.io/predmicror/",
        target = "_blank",
        tags$img(src = logo_src, alt = "predmicror logo", class = "brand-logo"),
        span("predmicror")
      ),
      div(
        class = "navbar-nav ms-auto",
        tags$a(
          class = "nav-link text-white",
          href = "https://fsqanalytics.github.io/predmicror/reference/",
          target = "_blank",
          "Reference"
        ),
        tags$a(
          class = "nav-link text-white",
          href = "https://fsqanalytics.github.io/predmicror/articles/",
          target = "_blank",
          "Articles"
        ),
        tags$a(
          class = "nav-link text-white",
          href = "https://github.com/fsqanalytics/predmicror",
          target = "_blank",
          "GitHub"
        )
      )
    )
  ),
  div(
    class = "container",
    div(
      class = "app-hero",
      h2(class = "app-title", "predmicror assistant"),
      p(
        class = "app-subtitle",
        "Ask about growth, inactivation, and cardinal models. Responses are grounded in the package functions."
      )
    ),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(
          class = "sidebar-card",
          div(
            class = "d-flex align-items-center justify-content-between mb-2",
            h4(class = "mb-0", "Settings"),
            disclaimer_control
          ),
          disclaimer_note,
          selectizeInput(
            "model",
            "Ollama model",
            choices = NULL,
            selected = default_model,
            options = list(create = TRUE, placeholder = "Type or select a model")
          ),
          actionButton("refresh_models", "Refresh models", class = "btn btn-outline-primary btn-sm"),
          uiOutput("ollama_status"),
          hr(),
          checkboxInput("use_history", "Include previous Q/A", value = TRUE),
          checkboxInput("show_context", "Show context", value = FALSE),
          actionButton("clear_history", "Clear conversation", class = "btn btn-outline-secondary btn-sm"),
          hr(),
          h4("Learnr"),
          p(
            class = "text-muted small",
            "Launch a learnr tutorial from the latest answer to execute and explore the code."
          ),
          div(
            class = "learnr-actions",
            actionButton("launch_learnr", "Launch learnr", class = "btn btn-outline-primary btn-sm"),
            actionButton("stop_learnr", "Stop learnr", class = "btn btn-outline-secondary btn-sm")
          ),
          uiOutput("learnr_status")
        )
      ),
      mainPanel(
        width = 8,
        uiOutput("status_row"),
        div(
          class = "onboarding-card",
          h3("Quick start"),
          div(
            class = "onboarding-steps",
            div(
              class = "onboarding-step",
              span(class = "step-badge", "1"),
              div(
                div(class = "step-title", "Pick a model"),
                div(class = "text-muted small", "Choose or type an Ollama model in Settings.")
              )
            ),
            div(
              class = "onboarding-step",
              span(class = "step-badge", "2"),
              div(
                div(class = "step-title", "Ask your question"),
                div(class = "text-muted small", "Include dataset columns and time units if you have them.")
              )
            ),
            div(
              class = "onboarding-step",
              span(class = "step-badge", "3"),
              div(
                div(class = "step-title", "Review and run"),
                div(class = "text-muted small", "Launch learnr to run the code and refine the fit.")
              )
            )
          ),
          div(class = "example-label", "Example prompts"),
          div(
            class = "example-row",
            actionButton("example_huang", "Fit Huang model", class = "btn btn-outline-primary btn-sm prompt-chip"),
            actionButton("example_cardinal", "Cardinal temperature", class = "btn btn-outline-primary btn-sm prompt-chip"),
            actionButton("example_inactivation", "Weibull inactivation", class = "btn btn-outline-primary btn-sm prompt-chip"),
            actionButton("example_ph", "Cardinal pH", class = "btn btn-outline-primary btn-sm prompt-chip")
          ),
          p(
            class = "text-muted small mt-2",
            "Tip: Provide a short dataset summary to get a concrete gsl_nls example."
          )
        ),
        h3("Conversation"),
        uiOutput("history"),
        div(
          class = "ask-panel",
          h3("Ask a question"),
          textAreaInput(
            "query",
            NULL,
            rows = 4,
            width = "100%",
            placeholder = "Example: How do I fit a Huang model to growth data?"
          ),
          div(
            class = "ask-actions",
            actionButton("ask", "Send", class = "btn btn-primary"),
            actionButton("clear_input", "Clear", class = "btn btn-outline-secondary")
          ),
          uiOutput("ask_status"),
          div(
            class = "ask-hint",
            "Tip: Ask a follow-up question, or paste a specific dataset summary to get a concrete gsl_nls example."
          )
        ),
        conditionalPanel(
          "input.show_context == true",
          h3("Context"),
          verbatimTextOutput("context")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  history <- reactiveVal(list())
  context <- reactiveVal("")
  ollama_models <- reactiveVal(character(0))
  last_response <- reactiveVal(NULL)
  ask_state <- reactiveVal("idle")
  learnr_state <- reactiveValues(proc = NULL, url = NULL, dir = NULL)

  stop_learnr_session <- function() {
    shiny::isolate({
      proc <- learnr_state$proc
      dir <- learnr_state$dir
      if (!is.null(proc)) {
        try({
          if (proc$is_alive()) {
            proc$kill()
          }
        }, silent = TRUE)
      }
      if (!is.null(dir) && dir.exists(dir)) {
        unlink(dir, recursive = TRUE)
      }
      learnr_state$proc <- NULL
      learnr_state$url <- NULL
      learnr_state$dir <- NULL
    })
  }

  observeEvent(input$refresh_models, {
    models <- list_ollama_models()
    ollama_models(models)
    updateSelectizeInput(session, "model",
      choices = unique(c(models, default_model)),
      selected = if (length(models) > 0) models[1] else input$model
    )
  }, ignoreInit = TRUE)

  observe({
    models <- list_ollama_models()
    ollama_models(models)
    updateSelectizeInput(session, "model",
      choices = unique(c(models, default_model)),
      selected = if (default_model %in% models) default_model else default_model
    )
  })

  observeEvent(input$example_huang, {
    updateTextAreaInput(
      session,
      "query",
      value = "How do I fit a Huang model to growth data?"
    )
  })

  observeEvent(input$example_cardinal, {
    updateTextAreaInput(
      session,
      "query",
      value = "What are the parameter constraints for the cardinal temperature model?"
    )
  })

  observeEvent(input$example_inactivation, {
    updateTextAreaInput(
      session,
      "query",
      value = "Show me a Weibull inactivation model fit example."
    )
  })

  observeEvent(input$example_ph, {
    updateTextAreaInput(
      session,
      "query",
      value = "How do I fit the cardinal pH model and interpret its parameters?"
    )
  })

  observeEvent(input$clear_history, {
    history(list())
    context("")
  })

  observeEvent(input$clear_input, {
    updateTextAreaInput(session, "query", value = "")
  })

  observeEvent(input$launch_learnr, {
    if (!requireNamespace("learnr", quietly = TRUE)) {
      showNotification("Install the learnr package to launch tutorials.", type = "error")
      return()
    }
    if (!requireNamespace("callr", quietly = TRUE)) {
      showNotification("Install the callr package to launch tutorials.", type = "error")
      return()
    }
    items <- history()
    if (length(items) == 0) {
      showNotification("No assistant answers yet.", type = "warning")
      return()
    }

    answer_text <- items[[length(items)]]$answer
    code_text <- extract_code_blocks(answer_text)
    if (!nzchar(trimws(code_text))) {
      showNotification("No fenced R code found; opening a blank exercise.", type = "warning")
    }

    stop_learnr_session()
    function_names <- predmicror_function_names(assistant$root)
    tutorial <- build_learnr_tutorial(
      answer_text,
      code_text,
      function_names,
      app_root = assistant$root
    )
    port <- if (requireNamespace("httpuv", quietly = TRUE)) {
      httpuv::randomPort()
    } else {
      sample(8000:8999, 1)
    }
    app_root <- if (is.null(assistant$root)) "" else assistant$root
    proc <- callr::r_bg(
      function(tutorial_file, port, app_root) {
        Sys.setenv(PREDMICROR_APP_ROOT = app_root)
        learnr::run_tutorial(
          name = tutorial_file,
          package = NULL,
          shiny_args = list(host = "127.0.0.1", port = port, launch.browser = FALSE)
        )
      },
      args = list(tutorial_file = tutorial$file, port = port, app_root = app_root),
      supervise = TRUE,
      env = c(PREDMICROR_APP_ROOT = app_root)
    )
    learnr_state$proc <- proc
    learnr_state$url <- paste0("http://127.0.0.1:", port)
    learnr_state$dir <- tutorial$dir
    showNotification("Learnr session started.", type = "message")
  })

  observeEvent(input$stop_learnr, {
    stop_learnr_session()
  })

  session$onSessionEnded(stop_learnr_session)

  observeEvent(input$ask, {
    query_text <- trimws(input$query)
    req(nzchar(query_text))
    ask_state("thinking")
    updateActionButton(session, "ask", label = "Thinking...")
    on.exit(ask_state("idle"), add = TRUE)
    on.exit(updateActionButton(session, "ask", label = "Send"), add = TRUE)
    history_for_prompt <- if (isTRUE(input$use_history)) history() else NULL
    start_time <- Sys.time()
    withProgress(message = "Asking your model...", value = 0.2, {
      tryCatch({
        result <- assistant$fn(
          query = query_text,
          model = input$model,
          root = assistant$root,
          return_context = TRUE,
          conversation = history_for_prompt
        )
        current <- history()
        current[[length(current) + 1]] <- list(
          question = query_text,
          answer = result$answer
        )
        history(current)
        context(result$context)
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        last_response(list(ok = TRUE, seconds = elapsed))
        session$sendCustomMessage("hljs", TRUE)
        updateTextAreaInput(session, "query", value = "")
      }, error = function(e) {
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        last_response(list(ok = FALSE, seconds = elapsed))
        showNotification(conditionMessage(e), type = "error")
        context("")
      })
    })
  })

  output$history <- renderUI({
    items <- history()
    if (length(items) == 0) {
      return(div(class = "empty-state", "No questions yet. Try an example above."))
    }
    tagList(lapply(items, function(item) {
      div(
        class = "card qa-card",
        div(
          class = "card-body",
          div(class = "qa-label", "Question"),
          div(class = "qa-question", item$question),
          div(class = "qa-label", "Answer"),
          div(class = "qa-answer", tags$pre(tags$code(item$answer)))
        )
      )
    }))
  })

  output$context <- renderText({
    context()
  })

  output$status_row <- renderUI({
    model_name <- if (is.null(input$model)) "" else trimws(input$model)
    model_name <- if (nzchar(model_name)) model_name else "None"
    ollama_ok <- nzchar(Sys.which("ollama"))
    response <- last_response()
    response_text <- "No responses yet"
    response_class <- "status-item"
    if (identical(ask_state(), "thinking")) {
      response_text <- "Waiting for response..."
    } else if (!is.null(response) && isTRUE(response$ok) && is.finite(response$seconds)) {
      response_text <- sprintf("Last response %.1fs", response$seconds)
      response_class <- "status-item status-ok"
    } else if (!is.null(response) && isFALSE(response$ok)) {
      response_text <- "Last response failed"
      response_class <- "status-item status-warn"
    }
    div(
      class = "status-row",
      div(
        class = if (ollama_ok) "status-item status-ok" else "status-item status-warn",
        span(class = "status-label", "Ollama"),
        span(if (ollama_ok) "Detected" else "Missing")
      ),
      div(
        class = "status-item",
        span(class = "status-label", "Model"),
        span(model_name)
      ),
      div(
        class = response_class,
        span(class = "status-label", "Response"),
        span(response_text)
      )
    )
  })

  output$ask_status <- renderUI({
    if (identical(ask_state(), "thinking")) {
      return(div(class = "ask-status", span(class = "spinner"), span("Thinking...")))
    }
    NULL
  })

  output$ollama_status <- renderUI({
    if (nzchar(Sys.which("ollama"))) {
      span(class = "status-pill status-ok", "Ollama detected")
    } else {
      span(class = "status-pill status-warn", "Ollama not found in PATH")
    }
  })

  output$learnr_status <- renderUI({
    proc <- learnr_state$proc
    url <- learnr_state$url
    alive <- FALSE
    if (!is.null(proc)) {
      alive <- tryCatch(proc$is_alive(), error = function(e) FALSE)
    }
    if (!alive || is.null(url)) {
      return(span(class = "text-muted small", "No learnr session running."))
    }
    tagList(
      span(class = "status-pill status-ok", "Learnr running"),
      div(tags$a(
        href = url,
        target = "_blank",
        class = "btn btn-outline-primary btn-sm mt-2",
        "Open learnr"
      ))
    )
  })
}

shinyApp(ui, server)
