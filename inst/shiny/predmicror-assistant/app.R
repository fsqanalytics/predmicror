library(shiny)

assistant_model <- getOption("predmicror.assistant.model", "llama3-groq-tool-use:8b")
assistant_root <- getOption("predmicror.assistant.root", NULL)

sep_from_input <- function(value) {
  switch(value,
    comma = ",",
    semicolon = ";",
    tab = "\t",
    NULL
  )
}

sheet_from_input <- function(value) {
  if (is.null(value)) value <- ""
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

upload_path_with_extension <- function(upload) {
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

ui <- fluidPage(
  titlePanel("predmicror assistant"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "data_file",
        "Optional data file",
        accept = c(".csv", ".txt", ".tsv", ".tab", ".xls", ".xlsx")
      ),
      selectInput(
        "sep",
        "Text separator",
        choices = c("Auto" = "auto", "Comma" = "comma", "Semicolon" = "semicolon", "Tab" = "tab"),
        selected = "auto"
      ),
      textInput("dec", "Decimal mark", value = "."),
      textInput("sheet", "Excel sheet", value = "1"),
      textAreaInput(
        "query",
        "Question",
        value = "Analyse these data and suggest a predmicror model.",
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
      tabsetPanel(
        tabPanel("Answer", h4("Answer"), verbatimTextOutput("answer")),
        tabPanel(
          "Data",
          h4("Preview"),
          tableOutput("preview"),
          h4("Detected profile"),
          verbatimTextOutput("profile")
        ),
        tabPanel("Trace", verbatimTextOutput("trace"))
      )
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)

  uploaded_data <- reactive({
    req(input$data_file)
    predmicror:::predmicror_assist_read_data(
      upload_path_with_extension(input$data_file),
      sheet = sheet_from_input(input$sheet),
      sep = sep_from_input(input$sep),
      dec = input$dec
    )
  })

  data_profile <- reactive({
    dat <- uploaded_data()
    predmicror:::predmicror_assist_profile_data(dat)
  })

  observeEvent(input$ask, {
    dat <- tryCatch(uploaded_data(), error = function(e) NULL)
    result(tryCatch(
      predmicror::predmicror_assistant(
        input$query,
        model = assistant_model,
        root = assistant_root,
        data = dat,
        backend = input$backend,
        prefer_wrappers = input$prefer_wrappers,
        return_trace = input$return_trace
      ),
      error = function(e) list(answer = paste("Error:", conditionMessage(e)))
    ))
  }, ignoreInit = FALSE)

  output$preview <- renderTable({
    utils::head(uploaded_data(), 10)
  })

  output$profile <- renderText({
    predmicror:::predmicror_assist_profile_text(
      data_profile(),
      predmicror:::predmicror_assist_language(input$query)
    )
  })

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
    if (!isTRUE(input$return_trace)) {
      return(invisible(NULL))
    }
    x <- result()
    if (is.list(x) && !is.null(x$trace)) {
      utils::str(x$trace, max.level = 2)
    }
  })
}

shinyApp(ui, server)
