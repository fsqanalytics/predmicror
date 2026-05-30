#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
query <- if (length(args) > 0) {
  paste(args, collapse = " ")
} else {
  if (interactive()) {
    readline("Ask predmicror: ")
  } else {
    paste(readLines("stdin", warn = FALSE), collapse = " ")
  }
}

if (!nzchar(query)) {
  cat("No question provided. Example:\n")
  cat("Rscript inst/agent/predmicror_assist.R 'How do I fit Baranyi?'\n")
  quit(status = 1)
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) sub("^--file=", "", script_arg) else ""
root <- if (nzchar(script_path)) {
  normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = FALSE)
} else {
  getwd()
}
model <- Sys.getenv("OLLAMA_MODEL", "llama3-groq-tool-use:8b")

assistant_fn <- NULL
if (requireNamespace("predmicror", quietly = TRUE)) {
  assistant_fn <- predmicror::predmicror_assistant
} else {
  local_file <- file.path(root, "R", "assistant.R")
  if (!file.exists(local_file)) {
    stop("predmicror_assistant not found. Install predmicror or run from the repo root.", call. = FALSE)
  }
  utils_file <- file.path(root, "R", "assistant_utils.R")
  if (!file.exists(utils_file)) {
    stop("predmicror assistant utilities not found. Install predmicror or run from the repo root.", call. = FALSE)
  }
  source(utils_file)
  source(local_file)
  assistant_fn <- predmicror_assistant
}

answer <- assistant_fn(query, model = model, root = root, return_context = FALSE)
cat(answer, sep = "\n")
