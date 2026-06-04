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
#' @examples
#' \dontrun{
#' predmicror_assistant_app()
#' }
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

predmicror_assistant_fallback_app <- function(model, root = NULL) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      predmicror_assistant_app_css(),
      shiny::div(
        class = "pm-header",
        shiny::div(class = "pm-title", "predmicror assistant"),
        shiny::div(class = "pm-subtitle", "Data-aware model guidance for predictive microbiology")
      ),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::div(
            class = "pm-card pm-sidebar-card",
            shiny::h4("1. Data"),
            shiny::fileInput(
              "data_file",
              "Optional data file",
              accept = c(".csv", ".txt", ".tsv", ".tab", ".xls", ".xlsx")
            ),
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::selectInput(
                  "sep",
                  "Text separator",
                  choices = c("Auto" = "auto", "Comma" = "comma", "Semicolon" = "semicolon", "Tab" = "tab"),
                  selected = "auto"
                )
              ),
              shiny::column(6, shiny::textInput("dec", "Decimal mark", value = "."))
            ),
            shiny::textInput("sheet", "Excel sheet", value = "1"),
            shiny::div(class = "pm-status", shiny::textOutput("data_status"))
          ),
          shiny::div(
            class = "pm-card pm-sidebar-card",
            shiny::h4("2. Assistant"),
            shiny::selectInput(
              "backend",
              "Backend",
              choices = c("auto", "deterministic", "ollama"),
              selected = "auto"
            ),
            shiny::selectInput(
              "model",
              "LLM model",
              choices = predmicror_assist_available_ollama_models(model),
              selected = model
            ),
            shiny::textInput("custom_model", "Custom Ollama model", value = ""),
            shiny::textAreaInput(
              "query",
              "Question",
              value = "Analyse these data and suggest a predmicror model.",
              rows = 5
            ),
            shiny::checkboxInput("prefer_wrappers", "Prefer fit_* wrappers", TRUE),
            shiny::checkboxInput("show_trace", "Show trace", FALSE)
          ),
          shiny::div(
            class = "pm-card pm-sidebar-card",
            shiny::h4("3. Column mapping"),
            shiny::selectInput("task_override", "Task", choices = c("Auto" = "", "Growth" = "growth", "Inactivation" = "inactivation", "Cardinal" = "cardinal")),
            shiny::selectInput("time_col", "Time column", choices = c("Auto" = "")),
            shiny::selectInput("response_col", "Response column", choices = c("Auto" = "")),
            shiny::selectInput("temperature_col", "Temperature column", choices = c("Auto" = "")),
            shiny::selectInput("ph_col", "pH column", choices = c("Auto" = "")),
            shiny::selectInput("aw_col", "aw column", choices = c("Auto" = "")),
            shiny::selectInput("inhibitor_col", "Inhibitor column", choices = c("Auto" = "")),
            shiny::actionButton("ask", "Generate analysis", class = "btn-primary pm-action")
          )
        ),
        shiny::mainPanel(
          width = 8,
          shiny::div(
            class = "pm-metric-grid",
            shiny::div(class = "pm-metric", shiny::span("Rows"), shiny::strong(shiny::textOutput("metric_rows", inline = TRUE))),
            shiny::div(class = "pm-metric", shiny::span("Task"), shiny::strong(shiny::textOutput("metric_task", inline = TRUE))),
            shiny::div(class = "pm-metric", shiny::span("Response"), shiny::strong(shiny::textOutput("metric_response", inline = TRUE))),
            shiny::div(class = "pm-metric", shiny::span("Model"), shiny::strong(shiny::textOutput("metric_model", inline = TRUE)))
          ),
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Answer",
              shiny::div(class = "pm-card", shiny::h4("Recommendation"), shiny::verbatimTextOutput("answer"))
            ),
            shiny::tabPanel(
              "Code",
              shiny::div(
                class = "pm-card",
                shiny::h4("Reproducible R code"),
                shiny::tags$p("Copy this code to an R script and replace the data-loading line if needed."),
                shiny::verbatimTextOutput("code")
              )
            ),
            shiny::tabPanel(
              "Data",
              shiny::div(class = "pm-card", shiny::h4("Preview"), shiny::tableOutput("preview")),
              shiny::div(class = "pm-card", shiny::h4("Detected profile"), shiny::verbatimTextOutput("profile"))
            ),
            shiny::tabPanel(
              "Trace",
              shiny::div(class = "pm-card", shiny::verbatimTextOutput("trace"))
            )
          )
        )
      )
    ),
    server = function(input, output, session) {
      result <- shiny::reactiveVal(NULL)

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

      uploaded_data <- shiny::reactive({
        shiny::req(input$data_file)
        predmicror_assist_read_data(
          upload_path(input$data_file),
          sheet = sheet_value(input$sheet),
          sep = sep_value(input$sep),
          dec = input$dec
        )
      })

      automatic_profile <- shiny::reactive({
        predmicror_assist_profile_data(uploaded_data())
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

      shiny::observeEvent(uploaded_data(), {
        dat <- uploaded_data()
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
        dat <- tryCatch(uploaded_data(), error = function(e) NULL)
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
      }, ignoreInit = FALSE)

      output$data_status <- shiny::renderText({
        if (is.null(input$data_file)) {
          return("No data file uploaded. The assistant can still answer general questions.")
        }
        paste("Loaded", input$data_file$name)
      })

      output$preview <- shiny::renderTable({
        utils::head(uploaded_data(), 10)
      })

      output$profile <- shiny::renderText({
        predmicror_assist_profile_text(
          data_profile(),
          predmicror_assist_language(input$query)
        )
      })

      output$metric_rows <- shiny::renderText({
        profile <- tryCatch(data_profile(), error = function(e) NULL)
        if (is.null(profile)) "-" else profile$n_rows
      })
      output$metric_task <- shiny::renderText({
        profile <- tryCatch(data_profile(), error = function(e) NULL)
        if (is.null(profile)) "-" else predmicror_assist_na_label(profile$task)
      })
      output$metric_response <- shiny::renderText({
        profile <- tryCatch(data_profile(), error = function(e) NULL)
        if (is.null(profile)) "-" else predmicror_assist_na_label(profile$columns$response)
      })
      output$metric_model <- shiny::renderText({
        x <- result()
        if (is.list(x) && !is.null(x$trace$candidates) && length(x$trace$candidates)) {
          return(x$trace$candidates[1])
        }
        profile <- tryCatch(data_profile(), error = function(e) NULL)
        if (is.null(profile) || !length(profile$candidate)) "-" else profile$candidate[1]
      })

      output$answer <- shiny::renderText({
        x <- result()
        if (is.null(x)) {
          return("")
        }
        ans <- if (is.list(x) && !is.null(x$answer)) x$answer else x
        trimws(predmicror_assist_strip_code_blocks(ans))
      })

      output$code <- shiny::renderText({
        x <- result()
        if (is.list(x) && !is.null(x$trace$code) && nzchar(x$trace$code)) {
          return(x$trace$code)
        }
        if (is.list(x) && !is.null(x$answer)) {
          return(predmicror_assist_extract_code_blocks(x$answer))
        }
        ""
      })

      output$trace <- shiny::renderPrint({
        if (!isTRUE(input$show_trace)) {
          cat("Enable 'Show trace' in the sidebar to display debug metadata.")
          return(invisible(NULL))
        }
        x <- result()
        if (is.list(x) && !is.null(x$trace)) {
          utils::str(x$trace, max.level = 2)
        }
      })
    }
  )
}

predmicror_assistant_app_css <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body { background: #f6f8fb; color: #1f2933; }
      .pm-header { margin: 18px 0 14px 0; }
      .pm-title { font-size: 30px; font-weight: 700; letter-spacing: -0.02em; }
      .pm-subtitle { color: #667085; margin-top: 3px; }
      .pm-card { background: #ffffff; border: 1px solid #e5e7eb; border-radius: 14px; padding: 16px 18px; box-shadow: 0 5px 18px rgba(15, 23, 42, 0.05); margin-bottom: 16px; }
      .pm-sidebar-card h4 { margin-top: 0; font-weight: 700; color: #1f2933; }
      .pm-status { color: #475467; font-size: 13px; background: #f9fafb; border-radius: 8px; padding: 8px 10px; margin-top: 4px; }
      .pm-action { width: 100%; margin-top: 8px; font-weight: 600; }
      .pm-metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin-bottom: 14px; }
      .pm-metric { background: #ffffff; border: 1px solid #e5e7eb; border-radius: 14px; padding: 13px 15px; box-shadow: 0 5px 18px rgba(15, 23, 42, 0.05); }
      .pm-metric span { display: block; color: #667085; font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 4px; }
      .pm-metric strong { font-size: 18px; color: #111827; }
      pre { white-space: pre-wrap; word-break: break-word; background: #f8fafc; border: 1px solid #e5e7eb; border-radius: 10px; padding: 14px; }
      table { background: #ffffff; }
      .tab-content { padding-top: 14px; }
      @media (max-width: 992px) { .pm-metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
      @media (max-width: 640px) { .pm-metric-grid { grid-template-columns: 1fr; } }
    "))
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
