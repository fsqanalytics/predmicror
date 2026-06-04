#' Launch the predmicror assistant Shiny app
#'
#' Starts the local assistant app bundled with the package. The app can read
#' delimited text and Excel files, preview uploaded data, profile columns, and
#' pass the data profile to [predmicror_assistant()]. If the bundled app is not
#' available, a fallback app is created from the installed R functions.
#'
#' @param model Ollama model name used by [predmicror_assistant()].
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

predmicror_assistant_fallback_app <- function(model, root = NULL) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("predmicror assistant"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fileInput(
            "data_file",
            "Optional data file",
            accept = c(".csv", ".txt", ".tsv", ".tab", ".xls", ".xlsx")
          ),
          shiny::selectInput(
            "sep",
            "Text separator",
            choices = c("Auto" = "auto", "Comma" = "comma", "Semicolon" = "semicolon", "Tab" = "tab"),
            selected = "auto"
          ),
          shiny::textInput("dec", "Decimal mark", value = "."),
          shiny::textInput("sheet", "Excel sheet", value = "1"),
          shiny::textAreaInput(
            "query",
            "Question",
            value = "Analyse these data and suggest a predmicror model.",
            rows = 5
          ),
          shiny::selectInput(
            "backend",
            "Backend",
            choices = c("auto", "deterministic", "ollama"),
            selected = "auto"
          ),
          shiny::checkboxInput("prefer_wrappers", "Prefer fit_* wrappers", TRUE),
          shiny::checkboxInput("return_trace", "Show trace", FALSE),
          shiny::actionButton("ask", "Ask")
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Answer", shiny::h4("Answer"), shiny::verbatimTextOutput("answer")),
            shiny::tabPanel(
              "Data",
              shiny::h4("Preview"),
              shiny::tableOutput("preview"),
              shiny::h4("Detected profile"),
              shiny::verbatimTextOutput("profile")
            ),
            shiny::tabPanel("Trace", shiny::verbatimTextOutput("trace"))
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

      uploaded_data <- shiny::reactive({
        shiny::req(input$data_file)
        predmicror_assist_read_data(
          upload_path(input$data_file),
          sheet = sheet_value(input$sheet),
          sep = sep_value(input$sep),
          dec = input$dec
        )
      })

      data_profile <- shiny::reactive({
        predmicror_assist_profile_data(uploaded_data())
      })

      shiny::observeEvent(input$ask, {
        dat <- tryCatch(uploaded_data(), error = function(e) NULL)
        result(tryCatch(
          predmicror_assistant(
            input$query,
            model = model,
            root = root,
            data = dat,
            backend = input$backend,
            prefer_wrappers = input$prefer_wrappers,
            return_trace = input$return_trace
          ),
          error = function(e) list(answer = paste("Error:", conditionMessage(e)))
        ))
      }, ignoreInit = FALSE)

      output$preview <- shiny::renderTable({
        utils::head(uploaded_data(), 10)
      })

      output$profile <- shiny::renderText({
        predmicror_assist_profile_text(
          data_profile(),
          predmicror_assist_language(input$query)
        )
      })

      output$answer <- shiny::renderText({
        x <- result()
        if (is.null(x)) {
          return("")
        }
        if (is.list(x) && !is.null(x$answer)) {
          return(x$answer)
        }
        x
      })

      output$trace <- shiny::renderPrint({
        if (!isTRUE(input$return_trace)) {
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
