library(shiny)

assistant_model <- getOption("predmicror.assistant.model", "llama3-groq-tool-use:8b")
assistant_root <- getOption("predmicror.assistant.root", NULL)

ui <- predmicror:::predmicror_assistant_app_ui(assistant_model)
server <- predmicror:::predmicror_assistant_app_server(root = assistant_root)

shinyApp(ui, server)
