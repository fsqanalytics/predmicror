library(shiny)

assistant_model <- getOption("predmicror.assistant.model", "llama3-groq-tool-use:8b")
assistant_root <- getOption("predmicror.assistant.root", NULL)

ui <- fluidPage(
  titlePanel("predmicror assistant"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "query",
        "Question",
        value = "How do I fit a Huang full growth model?",
        rows = 5
      ),
      selectInput(
        "backend",
        "Backend",
        choices = c("auto", "deterministic", "ollama"),
        selected = "auto"
      ),
      checkboxInput("prefer_wrappers", "Prefer fit_* wrappers", TRUE),
      checkboxInput("return_trace", "Show trace", FALSE),
      actionButton("ask", "Ask")
    ),
    mainPanel(
      h4("Answer"),
      verbatimTextOutput("answer"),
      conditionalPanel(
        "input.return_trace == true",
        h4("Trace"),
        verbatimTextOutput("trace")
      )
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)

  observeEvent(input$ask, {
    result(tryCatch(
      predmicror::predmicror_assistant(
        input$query,
        model = assistant_model,
        root = assistant_root,
        backend = input$backend,
        prefer_wrappers = input$prefer_wrappers,
        return_trace = input$return_trace
      ),
      error = function(e) list(answer = paste("Error:", conditionMessage(e)))
    ))
  }, ignoreInit = FALSE)

  output$answer <- renderText({
    x <- result()
    if (is.null(x)) {
      return("")
    }
    if (is.list(x) && !is.null(x$answer)) {
      return(x$answer)
    }
    x
  })

  output$trace <- renderPrint({
    x <- result()
    if (is.list(x) && !is.null(x$trace)) {
      utils::str(x$trace, max.level = 2)
    }
  })
}

shinyApp(ui, server)
