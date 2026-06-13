#' Launch the predmicror assistant Shiny app
#'
#' Starts the local assistant app bundled with the package. The app can read
#' delimited text and Excel files, preview uploaded data, profile columns, apply
#' optional manual column overrides, and pass the data profile to
#' [predmicror_assistant()]. If the bundled app is not available, a fallback app
#' is created from the installed R functions.
#'
#' @param model Default Ollama model name used by [predmicror_assistant()]. The
#'   app also exposes a model selector populated from `ollama list` when Ollama
#'   is available.
#' @param root Optional package root used for assistant context collection.
#' @param host Host passed to [shiny::runApp()]. Defaults to `"127.0.0.1"`.
#' @param port Optional port passed to [shiny::runApp()].
#' @param launch.browser Logical; whether to launch a browser.
#'
#' @return Runs the Shiny app.
#' @export
#'
#' @examplesIf interactive()
#' predmicror_assistant_app()
#'
predmicror_assistant_app <- function(model = "llama3-groq-tool-use:8b",
                                     root = NULL,
                                     host = "127.0.0.1",
                                     port = NULL,
                                     launch.browser = interactive()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"shiny\" must be installed to run the app.", call. = FALSE)
  }

  app_path <- system.file("shiny", "predmicror-assistant", package = "predmicror")
  app <- if (nzchar(app_path) && file.exists(file.path(app_path, "app.R"))) {
    app_path
  } else {
    predmicror_assistant_fallback_app(model = model, root = root)
  }

  old_options <- options(
    predmicror.assistant.model = model,
    predmicror.assistant.root = root
  )
  on.exit(options(old_options), add = TRUE)

  args <- list(appDir = app, host = host, launch.browser = launch.browser)
  if (!is.null(port)) {
    args$port <- port
  }

  tryCatch(
    withCallingHandlers(
      do.call(shiny::runApp, args),
      warning = function(w) {
        if (grepl("Superclass process has cloneable=FALSE", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      },
      message = function(m) {
        if (grepl("Superclass process has cloneable=FALSE", conditionMessage(m))) {
          invokeRestart("muffleMessage")
        }
      }
    ),
    error = function(e) {
      if (grepl("_later_execCallbacks|execCallbacks\\(timeoutSecs|later::execCallbacks", conditionMessage(e))) {
        return(invisible(NULL))
      }
      stop(e)
    }
  )
}

predmicror_assist_available_ollama_models <- function(default = "llama3-groq-tool-use:8b") {
  fallback <- unique(c(default, "llama3.2", "mistral", "qwen2.5", "gemma2"))
  if (!nzchar(Sys.which("ollama"))) {
    return(fallback)
  }
  out <- tryCatch(
    system2("ollama", "list", stdout = TRUE, stderr = FALSE),
    error = function(e) character(0),
    warning = function(w) character(0)
  )
  if (length(out) <= 1L) {
    return(fallback)
  }
  models <- trimws(sub("\\s+.*$", "", out[-1L]))
  models <- models[nzchar(models)]
  unique(c(default, models, fallback))
}

predmicror_assist_restart_ollama_model <- function(model,
                                                   ollama = Sys.which("ollama"),
                                                   runner = system2) {
  model <- trimws(model %||% "")
  if (!nzchar(model)) {
    return(list(
      ok = FALSE,
      model = model,
      message = "Select an Ollama model before restarting the LLM."
    ))
  }
  if (!nzchar(ollama)) {
    return(list(
      ok = FALSE,
      model = model,
      message = "Ollama is not available on PATH; no LLM process was restarted."
    ))
  }

  warning_message <- NULL
  output <- tryCatch(
    withCallingHandlers(
      runner(ollama, c("stop", model), stdout = TRUE, stderr = TRUE),
      warning = function(w) {
        warning_message <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(output, "error")) {
    return(list(
      ok = FALSE,
      model = model,
      message = sprintf("Could not restart `%s`: %s", model, conditionMessage(output))
    ))
  }

  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  details <- paste(output[nzchar(output)], collapse = "\n")

  if (!identical(as.integer(status), 0L)) {
    detail <- if (nzchar(details)) details else warning_message
    message <- sprintf("Could not restart `%s`.", model)
    if (!is.null(detail) && nzchar(detail)) {
      message <- paste(message, detail)
    }
    return(list(ok = FALSE, model = model, output = output, message = message))
  }

  list(
    ok = TRUE,
    model = model,
    output = output,
    message = sprintf("Restarted `%s`; the next Ollama request will load a fresh LLM session.", model)
  )
}

predmicror_assistant_app_query_presets <- function() {
  c(
    "Analyse these data and suggest a predmicror model.",
    "Fit a primary growth model and explain the response scale.",
    "Fit an inactivation model and compare two alternatives.",
    "Analyse these data as a dynamic growth profile.",
    "Analyse these data as a dynamic inactivation profile.",
    "Suggest a cardinal model and starting values."

  )
}
predmicror_assistant_render_answer_html <- function(text) {
  text <- predmicror_assist_strip_code_blocks(text)
  text <- trimws(text)
  if (!nzchar(text)) {
    return(shiny::div(class = "pm-answer-empty", "No recommendation text was returned."))
  }

  html <- if (requireNamespace("commonmark", quietly = TRUE)) {
    commonmark::markdown_html(text, extensions = TRUE, smart = TRUE)
  } else {
    escaped <- htmltools::htmlEscape(text)
    paragraphs <- strsplit(escaped, "\n+", perl = TRUE)[[1]]
    paste0("<p>", paragraphs, "</p>", collapse = "")
  }

  htmltools::HTML(html)
}

predmicror_assistant_render_code_widget <- function(code, output_id = "code") {
  code <- trimws(code %||% "")
  if (!nzchar(code)) {
    code <- "# Run an analysis to populate the code tab."
  }

  if (requireNamespace("shinyAce", quietly = TRUE)) {
    return(shinyAce::aceEditor(
      outputId = output_id,
      value = code,
      mode = "r",
      theme = "chrome",
      readOnly = TRUE,
      height = "360px",
      fontSize = 13,
      wordWrap = TRUE,
      showLineNumbers = TRUE,
      highlightActiveLine = FALSE,
      showPrintMargin = FALSE
    ))
  }

  shiny::pre(class = "pm-code-fallback", code)
}


predmicror_assistant_app_sidebar_card <- function(id, title, body, open = TRUE) {
  collapse_id <- paste0(id, "_body")
  shiny::div(
    class = "pm-card pm-sidebar-card",
    shiny::div(
      class = "pm-card-head",
      shiny::div(class = "pm-section-title pm-sidebar-title", title),
      shiny::tags$button(
        type = "button",
        class = "btn btn-link pm-card-toggle",
        `data-toggle` = "collapse",
        `data-target` = paste0("#", collapse_id),
        `aria-expanded` = if (open) "true" else "false",
        shiny::icon(if (open) "chevron-up" else "chevron-down"),
        shiny::span(if (open) "Collapse" else "Expand")
      )
    ),
    shiny::div(
      id = collapse_id,
      class = paste(c("pm-card-body", "collapse"), if (open) "in" else NULL),
      body
    )
  )
}

predmicror_assistant_app_ui <- function(default_model) {
  shiny::fluidPage(
    predmicror_assistant_app_css(),
    shiny::div(
      class = "pm-shell",
      shiny::div(
        class = "pm-header",
        shiny::div(
          class = "pm-title-row",
          shiny::div(
            shiny::div(class = "pm-title", "predmicror assistant"),
            shiny::div(class = "pm-subtitle", "Data-aware model guidance for predictive microbiology")
          ),
          shiny::div(
            class = "pm-session-chip",
            shiny::span(class = "pm-session-dot"),
            shiny::textOutput("session_status", inline = TRUE)
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          4,
          predmicror_assistant_app_sidebar_card(
            "data",
            "Data",
            shiny::tagList(
              shiny::selectInput(
                "data_kind",
                "File type",
                choices = c("CSV / text" = "text", "Excel workbook" = "excel"),
                selected = "text"
              ),
              shiny::fileInput(
                "data_file",
                "Choose file",
                accept = c(".csv", ".txt", ".tsv", ".tab", ".dat", ".xls", ".xlsx")
              ),
              shiny::conditionalPanel(
                condition = "input.data_kind === 'text'",
                shiny::fluidRow(
                  shiny::column(
                    6,
                    shiny::selectInput(
                      "sep",
                      "Separator",
                      choices = c("Auto" = "auto", "Comma" = "comma", "Semicolon" = "semicolon", "Tab" = "tab"),
                      selected = "auto"
                    )
                  ),
                  shiny::column(6, shiny::textInput("dec", "Decimal", value = "."))
                )
              ),
              shiny::conditionalPanel(
                condition = "input.data_kind === 'excel'",
                shiny::textInput("sheet", "Excel sheet", value = "1")
              ),
              shiny::div(
                class = "pm-load-row",
                shiny::actionButton(
                  "load_data",
                  "Load",
                  icon = shiny::icon("folder-open"),
                  class = "btn-primary pm-load-action"
                )
              ),
              shiny::div(class = "pm-status", shiny::textOutput("data_status")),
              shiny::div(class = "pm-inline-metrics", shiny::textOutput("data_shape", inline = TRUE)),
              shiny::div(class = "pm-inline-metrics", shiny::textOutput("data_hint", inline = TRUE))
            ),
            open = TRUE
          ),
          predmicror_assistant_app_sidebar_card(
            "assistant",
            "Assistant",
            shiny::tagList(
              shiny::selectInput(
                "backend",
                "Backend",
                choices = c("auto", "deterministic", "ollama"),
                selected = "auto"
              ),
              shiny::selectInput(
                "model",
                "LLM model",
                choices = predmicror_assist_available_ollama_models(default_model),
                selected = default_model
              ),
              shiny::textInput("custom_model", "Custom Ollama model", value = ""),
              shiny::selectInput(
                "query_preset",
                "Prompt template",
                choices = c("Custom" = "", predmicror_assistant_app_query_presets()),
                selected = ""
              ),
              shiny::textAreaInput(
                "query",
                "Question",
                value = "Analyse these data and suggest a predmicror model.",
                rows = 6
              ),
              shiny::div(
                class = "pm-action-stack",
                shiny::actionButton(
                  "ask",
                  "Generate analysis",
                  icon = shiny::icon("play"),
                  class = "btn-primary pm-main-action"
                ),
                shiny::div(
                  class = "pm-action-row",
                  shiny::actionButton(
                    "restart_llm",
                    "Restart LLM",
                    icon = shiny::icon("arrows-rotate"),
                    class = "btn-default pm-secondary-action"
                  ),
                  shiny::actionButton(
                    "reset_session",
                    "Reset",
                    icon = shiny::icon("eraser"),
                    class = "btn-default pm-ghost-action"
                  )
                )
              ),
              shiny::div(class = "pm-status", shiny::textOutput("llm_status")),
              shiny::checkboxInput("prefer_wrappers", "Prefer fit_* wrappers", TRUE),
              shiny::checkboxInput("show_trace", "Show trace", FALSE)
            ),
            open = TRUE
          ),
          predmicror_assistant_app_sidebar_card(
            "mapping",
            "Column mapping",
            shiny::tagList(
              shiny::selectInput(
                "task_override",
                "Task",
                choices = c("Auto" = "", "Growth" = "growth", "Inactivation" = "inactivation", "Cardinal" = "cardinal")
              ),
              shiny::selectInput("time_col", "Time column", choices = c("Auto" = "")),
              shiny::selectInput("response_col", "Response column", choices = c("Auto" = "")),
              shiny::selectInput("temperature_col", "Temperature column", choices = c("Auto" = "")),
              shiny::selectInput("ph_col", "pH column", choices = c("Auto" = "")),
              shiny::selectInput("aw_col", "aw column", choices = c("Auto" = "")),
              shiny::selectInput("inhibitor_col", "Inhibitor column", choices = c("Auto" = "")),
              shiny::div(class = "pm-status", shiny::textOutput("mapping_status"))
            ),
            open = FALSE
          )
        ),
        shiny::column(
          8,
          shiny::div(
            class = "pm-metric-grid",
            shiny::div(class = "pm-metric", shiny::span("Rows"), shiny::strong(shiny::textOutput("metric_rows", inline = TRUE))),
            shiny::div(class = "pm-metric", shiny::span("Task"), shiny::strong(shiny::textOutput("metric_task", inline = TRUE))),
            shiny::div(class = "pm-metric", shiny::span("Response"), shiny::strong(shiny::textOutput("metric_response", inline = TRUE))),
            shiny::div(class = "pm-metric", shiny::span("Model"), shiny::strong(shiny::textOutput("metric_model", inline = TRUE)))
          ),
          shiny::div(
            class = "pm-card pm-result-card",
            shiny::div(
              class = "pm-result-header",
              shiny::div(
                shiny::div(class = "pm-section-title", "Recommendation"),
                shiny::div(class = "pm-result-subtitle", shiny::textOutput("result_caption", inline = TRUE))
              ),
              shiny::div(class = "pm-result-badge", shiny::textOutput("result_backend", inline = TRUE))
            ),
            shiny::div(class = "pm-answer-body", shiny::uiOutput("answer"))
          ),
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Code",
              shiny::div(
                class = "pm-card",
                shiny::div(class = "pm-section-title", "Reproducible R code"),
                shiny::div(class = "pm-code-body", shiny::uiOutput("code"))
              )
            ),
            shiny::tabPanel(
              "Data",
              shiny::div(class = "pm-card", shiny::div(class = "pm-section-title", "Preview"), shiny::tableOutput("preview")),
              shiny::div(class = "pm-card", shiny::div(class = "pm-section-title", "Detected profile"), shiny::verbatimTextOutput("profile"))
            ),
            shiny::tabPanel(
              "Trace",
              shiny::div(class = "pm-card", shiny::verbatimTextOutput("trace"))
            )
          )
        )
      )
    )
  )
}

predmicror_assistant_app_server <- function(root = NULL) {
  function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    llm_status <- shiny::reactiveVal("")
    busy <- shiny::reactiveVal(FALSE)
    loaded_data <- shiny::reactiveVal(NULL)
    loaded_name <- shiny::reactiveVal("")
    load_error <- shiny::reactiveVal("")

    sep_value <- function(value) {
      switch(value,
        comma = ",",
        semicolon = ";",
        tab = "\t",
        NULL
      )
    }
    sheet_value <- function(value) {
      if (is.null(value)) {
        value <- ""
      }
      value <- trimws(value)
      if (!nzchar(value)) {
        return(NULL)
      }
      number <- suppressWarnings(as.integer(value))
      if (!is.na(number) && identical(as.character(number), value)) {
        return(number)
      }
      value
    }
    upload_path <- function(upload) {
      if (is.null(upload)) {
        return(NULL)
      }
      ext <- tools::file_ext(upload$name)
      if (!nzchar(ext)) {
        return(upload$datapath)
      }
      path <- paste0(upload$datapath, ".", ext)
      file.copy(upload$datapath, path, overwrite = TRUE)
      path
    }
    clean_override <- function(value) {
      value <- predmicror_assist_clean_override(value)
      if (is.null(value)) NULL else value
    }
    selected_model <- function() {
      custom <- trimws(input$custom_model %||% "")
      if (nzchar(custom)) custom else input$model
    }
    current_profile <- function() {
      tryCatch(data_profile(), error = function(e) NULL)
    }
    current_data <- function() {
      loaded_data()
    }
    current_kind_label <- function() {
      switch(input$data_kind %||% "text",
        excel = "Excel workbook",
        "CSV / text"
      )
    }

    shiny::observeEvent(input$query_preset, {
      if (nzchar(input$query_preset %||% "")) {
        shiny::updateTextAreaInput(session, "query", value = input$query_preset)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$reset_session, {
      result(NULL)
      llm_status("")
      shiny::updateTextAreaInput(
        session,
        "query",
        value = "Analyse these data and suggest a predmicror model."
      )
      shiny::updateSelectInput(session, "query_preset", selected = "")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$restart_llm, {
      status <- predmicror_assist_restart_ollama_model(selected_model())
      llm_status(status$message)
    }, ignoreInit = TRUE)

    session$onSessionEnded(function() {
      result(NULL)
      loaded_data(NULL)
      load_error("")
    })

    shiny::observeEvent(input$load_data, {
      shiny::req(input$data_file)
      load_error("")
      path <- upload_path(input$data_file)
      ext <- tolower(tools::file_ext(input$data_file$name))
      kind <- input$data_kind %||% "text"
      if (identical(kind, "excel") && !ext %in% c("xls", "xlsx")) {
        load_error("Selected file type is Excel, but the file extension is not .xls or .xlsx.")
        return(invisible(NULL))
      }
      if (identical(kind, "text") && ext %in% c("xls", "xlsx")) {
        load_error("Selected file type is CSV / text, but the file is an Excel workbook.")
        return(invisible(NULL))
      }

      dat <- tryCatch(
        predmicror_assist_read_data(
          path,
          sheet = sheet_value(input$sheet),
          sep = sep_value(input$sep),
          dec = input$dec
        ),
        error = function(e) e
      )
      if (inherits(dat, "error")) {
        load_error(conditionMessage(dat))
        loaded_data(NULL)
        loaded_name("")
        return(invisible(NULL))
      }

      loaded_data(dat)
      loaded_name(input$data_file$name)
      result(NULL)
      load_error(sprintf("Loaded %s as %s.", input$data_file$name, current_kind_label()))
      llm_status(sprintf("Data loaded from %s.", input$data_file$name))
    }, ignoreInit = TRUE)

    automatic_profile <- shiny::reactive({
      dat <- current_data()
      if (is.null(dat)) {
        return(NULL)
      }
      predmicror_assist_profile_data(dat)
    })

    data_profile <- shiny::reactive({
      predmicror_assist_override_profile(
        automatic_profile(),
        task = clean_override(input$task_override),
        columns = list(
          time = clean_override(input$time_col),
          response = clean_override(input$response_col),
          temperature = clean_override(input$temperature_col),
          ph = clean_override(input$ph_col),
          aw = clean_override(input$aw_col),
          inhibitor = clean_override(input$inhibitor_col)
        )
      )
    })

    shiny::observeEvent(automatic_profile(), {
      dat <- current_data()
      if (is.null(dat)) {
        return(invisible(NULL))
      }
      choices <- c("Auto" = "", stats::setNames(names(dat), names(dat)))
      for (id in c("time_col", "response_col", "temperature_col", "ph_col", "aw_col", "inhibitor_col")) {
        shiny::updateSelectInput(session, id, choices = choices)
      }
      profile <- automatic_profile()
      shiny::updateSelectInput(session, "task_override", selected = "")
      if (!is.na(profile$columns$time)) shiny::updateSelectInput(session, "time_col", selected = profile$columns$time)
      if (!is.na(profile$columns$response)) shiny::updateSelectInput(session, "response_col", selected = profile$columns$response)
      if (!is.na(profile$columns$temperature)) shiny::updateSelectInput(session, "temperature_col", selected = profile$columns$temperature)
      if (!is.na(profile$columns$ph)) shiny::updateSelectInput(session, "ph_col", selected = profile$columns$ph)
      if (!is.na(profile$columns$aw)) shiny::updateSelectInput(session, "aw_col", selected = profile$columns$aw)
      if (!is.na(profile$columns$inhibitor)) shiny::updateSelectInput(session, "inhibitor_col", selected = profile$columns$inhibitor)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$ask, {
      busy(TRUE)
      on.exit(busy(FALSE), add = TRUE)
      shiny::withProgress(message = "Generating analysis", value = 0, {
        shiny::incProgress(0.2, detail = "Profiling data")
        dat <- tryCatch(current_data(), error = function(e) NULL)
        shiny::incProgress(0.7, detail = "Running predmicror assistant")
        result(tryCatch(
          predmicror_assistant(
            input$query,
            model = selected_model(),
            root = root,
            data = dat,
            backend = input$backend,
            prefer_wrappers = input$prefer_wrappers,
            return_trace = TRUE,
            task = clean_override(input$task_override),
            time = clean_override(input$time_col),
            response = clean_override(input$response_col),
            temperature = clean_override(input$temperature_col),
            ph = clean_override(input$ph_col),
            aw = clean_override(input$aw_col),
            inhibitor = clean_override(input$inhibitor_col)
          ),
          error = function(e) list(answer = paste("Error:", conditionMessage(e)))
        ))
        shiny::incProgress(1, detail = "Done")
      })
    }, ignoreInit = TRUE)

    output$data_status <- shiny::renderText({
      msg <- load_error()
      if (nzchar(msg)) {
        return(msg)
      }
      file <- input$data_file
      dat <- current_data()
      if (is.null(file)) {
        return("Choose a file, select the file type, then click Load.")
      }
      if (is.null(dat)) {
        return(sprintf("Selected %s. Click Load to import it as %s.", file$name, current_kind_label()))
      }
      sprintf("Loaded %s as %s.", loaded_name() %||% file$name, current_kind_label())
    })

    output$data_shape <- shiny::renderText({
      dat <- current_data()
      if (is.null(dat)) {
        return("No rows in session yet.")
      }
      sprintf("%d rows x %d columns", nrow(dat), ncol(dat))
    })

    output$data_hint <- shiny::renderText({
      profile <- current_profile()
      if (is.null(profile)) {
        return("Upload a file to unlock auto-detection and data-aware prompts.")
      }
      candidate <- if (length(profile$candidate)) profile$candidate[1] else "no model"
      sprintf("Auto-detected task: %s; suggested model: %s.", predmicror_assist_na_label(profile$task), candidate)
    })

    output$llm_status <- shiny::renderText({
      msg <- llm_status()
      if (busy()) {
        return("Generating analysis...")
      }
      if (nzchar(msg)) {
        return(msg)
      }
      "Assistant ready."
    })

    output$preview <- shiny::renderTable({
      dat <- current_data()
      if (is.null(dat)) {
        return(data.frame(Status = "Load a dataset to preview its first rows."))
      }
      utils::head(dat, 10)
    })

    output$profile <- shiny::renderText({
      profile <- current_profile()
      if (is.null(profile)) {
        return("No profiled dataset in the current session.")
      }
      predmicror_assist_profile_text(
        profile,
        predmicror_assist_language(input$query)
      )
    })

    output$metric_rows <- shiny::renderText({
      profile <- current_profile()
      if (is.null(profile)) "-" else profile$n_rows
    })
    output$metric_task <- shiny::renderText({
      profile <- current_profile()
      if (is.null(profile)) "-" else predmicror_assist_na_label(profile$task)
    })
    output$metric_response <- shiny::renderText({
      profile <- current_profile()
      if (is.null(profile)) "-" else predmicror_assist_na_label(profile$columns$response)
    })
    output$metric_model <- shiny::renderText({
      x <- result()
      if (is.list(x) && !is.null(x$trace$candidates) && length(x$trace$candidates)) {
        return(x$trace$candidates[1])
      }
      profile <- current_profile()
      if (is.null(profile) || !length(profile$candidate)) "-" else profile$candidate[1]
    })

    output$session_status <- shiny::renderText({
      if (busy()) {
        return("Running")
      }
      if (is.null(current_data())) {
        return("Ready")
      }
      if (is.null(result())) {
        return("Data loaded")
      }
      "Analysed"
    })

    output$mapping_status <- shiny::renderText({
      profile <- current_profile()
      if (is.null(profile)) {
        return("Mapping controls activate after load.")
      }
      cols <- profile$columns
      paste(
        sprintf("Time: %s", predmicror_assist_na_label(cols$time)),
        sprintf("Response: %s", predmicror_assist_na_label(cols$response)),
        sep = " | "
      )
    })

    output$result_caption <- shiny::renderText({
      x <- result()
      if (busy()) {
        return("The assistant is generating a recommendation and reproducible code.")
      }
      if (is.null(x)) {
        return("Load a dataset, then run the assistant to see the recommendation and code.")
      }
      trace <- x$trace %||% NULL
      if (!is.null(trace) && length(trace$candidates) > 0) {
        return(sprintf("Primary candidate: %s", trace$candidates[1]))
      }
      "Analysis completed."
    })

    output$result_backend <- shiny::renderText({
      x <- result()
      if (busy()) {
        return("running")
      }
      if (is.list(x) && !is.null(x$trace$backend)) {
        return(x$trace$backend)
      }
      "idle"
    })

    output$answer <- shiny::renderUI({
      if (busy()) {
        return(shiny::div(class = "pm-answer-state", "Generating analysis..."))
      }
      x <- result()
      if (is.null(x)) {
        return(shiny::div(class = "pm-answer-state", "No analysis yet. Load a dataset or use a general question, then run the assistant."))
      }
      ans <- if (is.list(x) && !is.null(x$answer)) x$answer else x
      shiny::div(class = "pm-answer-content", predmicror_assistant_render_answer_html(ans))
    })

    output$code <- shiny::renderUI({
      x <- result()
      code <- NULL
      if (busy()) {
        code <- "# Generating reproducible code..."
      } else if (is.list(x) && !is.null(x$trace$code) && nzchar(x$trace$code)) {
        code <- x$trace$code
      } else if (is.list(x) && !is.null(x$answer)) {
        code <- predmicror_assist_extract_code_blocks(x$answer)
      }
      shiny::div(class = "pm-code-container", predmicror_assistant_render_code_widget(code))
    })

    output$trace <- shiny::renderPrint({
      if (!isTRUE(input$show_trace)) {
        cat("Enable 'Show trace' in the sidebar to display debug metadata.")
        return(invisible(NULL))
      }
      if (busy()) {
        cat("Trace will appear when the current run finishes.")
        return(invisible(NULL))
      }
      x <- result()
      if (is.list(x) && !is.null(x$trace)) {
        utils::str(x$trace, max.level = 2)
      }
    })
  }
}

predmicror_assistant_fallback_app <- function(model, root = NULL) {
  shiny::shinyApp(
    ui = predmicror_assistant_app_ui(model),
    server = predmicror_assistant_app_server(root = root)
  )
}

predmicror_assistant_app_css <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("\n      body { background: linear-gradient(180deg, #edf2f7 0%, #f7f9fc 45%, #eef4f8 100%); color: #1f2933; font-family: Georgia, 'Times New Roman', serif; }\n      .container-fluid { max-width: 1440px; }\n      .pm-shell { padding: 10px 0 24px 0; }\n      .pm-header { margin: 10px 0 18px 0; }\n      .pm-title-row { display: flex; align-items: flex-start; justify-content: space-between; gap: 16px; }\n      .pm-title { font-size: 32px; font-weight: 700; letter-spacing: 0; color: #17212b; }\n      .pm-subtitle { color: #566576; margin-top: 4px; max-width: 720px; font-size: 15px; line-height: 1.5; }\n      .pm-session-chip { display: inline-flex; align-items: center; gap: 8px; padding: 10px 14px; border-radius: 999px; background: rgba(255,255,255,0.8); border: 1px solid #d7dee6; color: #2c3e50; font-size: 13px; text-transform: uppercase; }\n      .pm-session-dot { width: 9px; height: 9px; border-radius: 999px; background: #0f9d58; box-shadow: 0 0 0 4px rgba(15, 157, 88, 0.12); }\n      .pm-card { background: rgba(255,255,255,0.93); border: 1px solid #dde3ea; border-radius: 12px; padding: 16px 18px; box-shadow: 0 12px 30px rgba(26, 40, 56, 0.08); margin-bottom: 16px; }\n      .pm-sidebar-card { padding: 0; overflow: hidden; }\n      .pm-card-head { display: flex; align-items: center; justify-content: space-between; gap: 12px; padding: 14px 18px 10px 18px; border-bottom: 1px solid #e6ebf0; }\n      .pm-sidebar-title { margin-bottom: 0; }\n      .pm-card-toggle { padding: 0; border: 0; color: #4b5c6c; font-size: 12px; display: inline-flex; align-items: center; gap: 6px; text-decoration: none; }\n      .pm-card-toggle:hover, .pm-card-toggle:focus { color: #17212b; text-decoration: none; }\n      .pm-card-body { padding: 14px 18px 18px 18px; }\n      .pm-card-body.in { display: block; }\n      .pm-section-title { font-size: 17px; font-weight: 700; color: #17212b; margin-bottom: 12px; }\n      .pm-status { color: #4b5c6c; font-size: 13px; background: #f7f9fb; border-radius: 10px; padding: 9px 11px; margin-top: 8px; border: 1px solid #e2e8ef; }\n      .pm-inline-metrics { color: #6a7a8a; font-size: 12px; margin-top: 8px; }\n      .pm-load-row { display: grid; grid-template-columns: minmax(0, 1fr); gap: 10px; margin-top: 10px; }\n      .pm-action-stack { display: grid; gap: 10px; margin-top: 8px; }\n      .pm-action-row { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px; }\n      .pm-main-action, .pm-ghost-action, .pm-secondary-action, .pm-load-action { width: 100%; min-height: 42px; font-weight: 600; border-radius: 10px; }\n      .pm-main-action { background: linear-gradient(135deg, #1c6e8c 0%, #274c77 100%); border-color: #274c77; }\n      .pm-ghost-action, .pm-secondary-action { background: #ffffff; border-color: #cfd6de; color: #324454; }\n      .pm-metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin-bottom: 16px; }\n      .pm-metric { background: rgba(255,255,255,0.92); border: 1px solid #dde3ea; border-radius: 12px; padding: 14px 15px; box-shadow: 0 10px 24px rgba(26, 40, 56, 0.07); }\n      .pm-metric span { display: block; color: #68788a; font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 4px; }\n      .pm-metric strong { font-size: 18px; color: #111827; }\n      .pm-result-card { padding-bottom: 10px; }\n      .pm-result-header { display: flex; justify-content: space-between; gap: 16px; align-items: flex-start; margin-bottom: 10px; }\n      .pm-result-subtitle { color: #607284; font-size: 13px; margin-top: 4px; }\n      .pm-result-badge { padding: 8px 12px; border-radius: 999px; background: #e9f2f6; color: #21445b; font-size: 12px; text-transform: uppercase; border: 1px solid #cddbe3; }\n      pre { white-space: pre-wrap; word-break: break-word; background: #f8fafc; border: 1px solid #e1e7ee; border-radius: 10px; padding: 14px; color: #243444; font-size: 13px; }\n      table { background: #ffffff; }\n      .tab-content { padding-top: 14px; }\n      .nav-tabs { border-bottom: 1px solid #d6dee7; }\n      .nav-tabs > li > a { border-radius: 10px 10px 0 0; color: #546678; }\n      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover { background: rgba(255,255,255,0.95); color: #17212b; border: 1px solid #d6dee7; border-bottom-color: transparent; }\n      @media (max-width: 992px) {\n        .pm-title-row { flex-direction: column; }\n        .pm-metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }\n        .pm-action-row { grid-template-columns: 1fr; }\n      }\n      @media (max-width: 640px) {\n        .pm-metric-grid { grid-template-columns: 1fr; }\n      }\n    "))
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
