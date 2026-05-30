#' Launch the predmicror assistant Shiny app
#'
#' Starts the local assistant app bundled with the package.
#'
#' @return Runs the Shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' predmicror_assistant_app()
#' }
#'
predmicror_assistant_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package \"shiny\" must be installed to run the app.", call. = FALSE)
  }
  app_path <- system.file("shiny", "predmicror-assistant", package = "predmicror")
  if (!nzchar(app_path)) {
    stop("Shiny app not found in the installed package.", call. = FALSE)
  }
  withCallingHandlers(
    shiny::runApp(app_path),
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
