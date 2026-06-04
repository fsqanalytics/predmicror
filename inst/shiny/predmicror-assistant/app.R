library(shiny)

assistant_model <- getOption("predmicror.assistant.model", "llama3-groq-tool-use:8b")
assistant_root <- getOption("predmicror.assistant.root", NULL)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

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

clean_override <- function(value) {
  value <- predmicror:::predmicror_assist_clean_override(value)
  if (is.null(value)) NULL else value
}

ui <- fluidPage(
  predmicror:::predmicror_assistant_app_css(),
  div(
    class = "pm-header",
    div(class = "pm-title", "predmicror assistant"),
    div(class = "pm-subtitle", "Data-aware model guidance for predictive microbiology")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(
        class = "pm-card pm-sidebar-card",
        h4("1. Data"),
        fileInput(
          "data_file",
          "Optional data file",
          accept = c(".csv", ".txt", ".tsv", ".tab", ".xls", ".xlsx")
        ),
        fluidRow(
          column(
            6,
            selectInput(
              "sep",
              "Text separator",
              choices = c("Auto" = "auto", "Comma" = "comma", "Semicolon" = "semicolon", "Tab" = "tab"),
              selected = "auto"
            )
          ),
          column(6, textInput("dec", "Decimal mark", value = "."))
        ),
        textInput("sheet", "Excel sheet", value = "1"),
        div(class = "pm-status", textOutput("data_status"))
      ),
      div(
        class = "pm-card pm-sidebar-card",
        h4("2. Assistant"),
        selectInput(
          "backend",
          "Backend",
          choices = c("auto", "deterministic", "ollama"),
          selected = "auto"
        ),
        selectInput(
          "model",
          "LLM model",
          choices = predmicror:::predmicror_assist_available_ollama_models(assistant_model),
          selected = assistant_model
        ),
        textInput("custom_model", "Custom Ollama model", value = ""),
        textAreaInput(
          "query",
          "Question",
          value = "Analyse these data and suggest a predmicror model.",
          rows = 5
        ),
        checkboxInput("prefer_wrappers", "Prefer fit_* wrappers", TRUE),
        checkboxInput("show_trace", "Show trace", FALSE)
      ),
      div(
        class = "pm-card pm-sidebar-card",
        h4("3. Column mapping"),
        selectInput("task_override", "Task", choices = c("Auto" = "", "Growth" = "growth", "Inactivation" = "inactivation", "Cardinal" = "cardinal")),
        selectInput("time_col", "Time column", choices = c("Auto" = "")),
        selectInput("response_col", "Response column", choices = c("Auto" = "")),
        selectInput("temperature_col", "Temperature column", choices = c("Auto" = "")),
        selectInput("ph_col", "pH column", choices = c("Auto" = "")),
        selectInput("aw_col", "aw column", choices = c("Auto" = "")),
        selectInput("inhibitor_col", "Inhibitor column", choices = c("Auto" = "")),
        actionButton("ask", "Generate analysis", class = "btn-primary pm-action")
      )
    ),
    mainPanel(
      width = 8,
      div(
        class = "pm-metric-grid",
        div(class = "pm-metric", span("Rows"), strong(textOutput("metric_rows", inline = TRUE))),
        div(class = "pm-metric", span("Task"), strong(textOutput("metric_task", inline = TRUE))),
        div(class = "pm-metric", span("Response"), strong(textOutput("metric_response", inline = TRUE))),
        div(class = "pm-metric", span("Model"), strong(textOutput("metric_model", inline = TRUE)))
      ),
      tabsetPanel(
        tabPanel(
          "Answer",
          div(class = "pm-card", h4("Recommendation"), verbatimTextOutput("answer"))
        ),
        tabPanel(
          "Code",
          div(
            class = "pm-card",
            h4("Reproducible R code"),
            tags$p("Copy this code to an R script and replace the data-loading line if needed."),
            verbatimTextOutput("code")
          )
        ),
        tabPanel(
          "Data",
          div(class = "pm-card", h4("Preview"), tableOutput("preview")),
          div(class = "pm-card", h4("Detected profile"), verbatimTextOutput("profile"))
        ),
        tabPanel(
          "Trace",
          div(class = "pm-card", verbatimTextOutput("trace"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)

  selected_model <- function() {
    custom <- trimws(input$custom_model %||% "")
    if (nzchar(custom)) custom else input$model
  }

  uploaded_data <- reactive({
    req(input$data_file)
    predmicror:::predmicror_assist_read_data(
      upload_path_with_extension(input$data_file),
      sheet = sheet_from_input(input$sheet),
      sep = sep_from_input(input$sep),
      dec = input$dec
    )
  })

  automatic_profile <- reactive({
    dat <- uploaded_data()
    predmicror:::predmicror_assist_profile_data(dat)
  })

  data_profile <- reactive({
    predmicror:::predmicror_assist_override_profile(
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

  observeEvent(uploaded_data(), {
    dat <- uploaded_data()
    choices <- c("Auto" = "", stats::setNames(names(dat), names(dat)))
    for (id in c("time_col", "response_col", "temperature_col", "ph_col", "aw_col", "inhibitor_col")) {
      updateSelectInput(session, id, choices = choices)
    }
    profile <- automatic_profile()
    updateSelectInput(session, "task_override", selected = "")
    if (!is.na(profile$columns$time)) updateSelectInput(session, "time_col", selected = profile$columns$time)
    if (!is.na(profile$columns$response)) updateSelectInput(session, "response_col", selected = profile$columns$response)
    if (!is.na(profile$columns$temperature)) updateSelectInput(session, "temperature_col", selected = profile$columns$temperature)
    if (!is.na(profile$columns$ph)) updateSelectInput(session, "ph_col", selected = profile$columns$ph)
    if (!is.na(profile$columns$aw)) updateSelectInput(session, "aw_col", selected = profile$columns$aw)
    if (!is.na(profile$columns$inhibitor)) updateSelectInput(session, "inhibitor_col", selected = profile$columns$inhibitor)
  }, ignoreInit = TRUE)

  observeEvent(input$ask, {
    dat <- tryCatch(uploaded_data(), error = function(e) NULL)
    result(tryCatch(
      predmicror::predmicror_assistant(
        input$query,
        model = selected_model(),
        root = assistant_root,
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

  output$data_status <- renderText({
    if (is.null(input$data_file)) {
      return("No data file uploaded. The assistant can still answer general questions.")
    }
    paste("Loaded", input$data_file$name)
  })

  output$preview <- renderTable({
    utils::head(uploaded_data(), 10)
  })

  output$profile <- renderText({
    predmicror:::predmicror_assist_profile_text(
      data_profile(),
      predmicror:::predmicror_assist_language(input$query)
    )
  })

  output$metric_rows <- renderText({
    profile <- tryCatch(data_profile(), error = function(e) NULL)
    if (is.null(profile)) "-" else profile$n_rows
  })
  output$metric_task <- renderText({
    profile <- tryCatch(data_profile(), error = function(e) NULL)
    if (is.null(profile)) "-" else predmicror:::predmicror_assist_na_label(profile$task)
  })
  output$metric_response <- renderText({
    profile <- tryCatch(data_profile(), error = function(e) NULL)
    if (is.null(profile)) "-" else predmicror:::predmicror_assist_na_label(profile$columns$response)
  })
  output$metric_model <- renderText({
    x <- result()
    if (is.list(x) && !is.null(x$trace$candidates) && length(x$trace$candidates)) {
      return(x$trace$candidates[1])
    }
    profile <- tryCatch(data_profile(), error = function(e) NULL)
    if (is.null(profile) || !length(profile$candidate)) "-" else profile$candidate[1]
  })

  output$answer <- renderText({
    x <- result()
    if (is.null(x)) {
      return("")
    }
    ans <- if (is.list(x) && !is.null(x$answer)) x$answer else x
    trimws(predmicror:::predmicror_assist_strip_code_blocks(ans))
  })

  output$code <- renderText({
    x <- result()
    if (is.list(x) && !is.null(x$trace$code) && nzchar(x$trace$code)) {
      return(x$trace$code)
    }
    if (is.list(x) && !is.null(x$answer)) {
      return(predmicror:::predmicror_assist_extract_code_blocks(x$answer))
    }
    ""
  })

  output$trace <- renderPrint({
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

shinyApp(ui, server)
