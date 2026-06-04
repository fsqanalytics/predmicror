#' Launch the predmicror assistant Shiny app
#'
#' Starts the local assistant app bundled with the package. If the bundled app is
#' not available, a small fallback app is created from the installed R functions.
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
          shiny::textAreaInput(
            "query",
            "Question",
            value = "How do I fit a Huang full growth model?",
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
          shiny::h4("Answer"),
          shiny::verbatimTextOutput("answer"),
          shiny::conditionalPanel(
            "input.return_trace == true",
            shiny::h4("Trace"),
            shiny::verbatimTextOutput("trace")
          )
        )
      )
    ),
    server = function(input, output, session) {
      result <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$ask, {
        result(tryCatch(
          predmicror_assistant(
            input$query,
            model = model,
            root = root,
            backend = input$backend,
            prefer_wrappers = input$prefer_wrappers,
            return_trace = input$return_trace
          ),
          error = function(e) list(answer = paste("Error:", conditionMessage(e)))
        ))
      }, ignoreInit = FALSE)

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
        x <- result()
        if (is.list(x) && !is.null(x$trace)) {
          str(x$trace, max.level = 2)
        }
      })
    }
  )
}
