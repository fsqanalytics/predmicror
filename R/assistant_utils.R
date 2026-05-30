predmicror_assist_extract_code_blocks <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  blocks <- list()
  current <- character(0)
  in_block <- FALSE
  for (line in lines) {
    if (grepl("^```", line)) {
      if (in_block) {
        blocks[[length(blocks) + 1]] <- current
        current <- character(0)
        in_block <- FALSE
      } else {
        in_block <- TRUE
      }
      next
    }
    if (in_block) {
      current <- c(current, line)
    }
  }
  if (length(blocks) == 0) {
    return("")
  }
  paste(vapply(blocks, function(block) paste(block, collapse = "\n"), ""), collapse = "\n\n")
}

predmicror_assist_strip_code_blocks <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  out <- character(0)
  in_block <- FALSE
  for (line in lines) {
    if (grepl("^```", line)) {
      in_block <- !in_block
      next
    }
    if (!in_block) {
      out <- c(out, line)
    }
  }
  paste(out, collapse = "\n")
}

predmicror_assist_normalize_text <- function(text) {
  if (!nzchar(text)) {
    return(text)
  }
  text <- gsub("\u201c|\u201d", "\"", text)
  text <- gsub("\u2018|\u2019", "'", text)
  gsub("\u00a0", " ", text)
}

predmicror_assist_extract_inline_code <- function(text) {
  if (!nzchar(text)) {
    return("")
  }
  text <- predmicror_assist_normalize_text(text)
  if (grepl("```", text)) {
    return("")
  }
  tokens <- c(
    "data\\(",
    "gsl_nls\\(",
    "summary\\(",
    "fitted\\(",
    "fitted_vals\\s*<-",
    "new_[A-Za-z]+\\s*<-",
    "pred\\s*<-",
    "predict\\(",
    "plot\\(",
    "lines\\(",
    "points\\(",
    "library\\(",
    "require\\("
  )
  pattern <- paste0("(?=\\b(", paste(tokens, collapse = "|"), "))")
  text <- gsub(pattern, "\n", text, perl = TRUE)
  text <- gsub("(?=\\b\\w+\\s*<-)", "\n", text, perl = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  code_like <- grepl(
    "<-|\\bdata\\(|\\bgsl_nls\\(|\\bsummary\\(|\\bfitted\\(|\\bpredict\\(|\\bplot\\(|\\blines\\(|\\bpoints\\(|\\blibrary\\(|\\brequire\\(",
    lines
  )
  code_lines <- lines[code_like]
  if (length(code_lines) == 0) {
    return("")
  }
  code_text <- paste(code_lines, collapse = "\n")
  last_paren <- max(gregexpr("[)\\]]", code_text, perl = TRUE)[[1]], na.rm = TRUE)
  if (is.finite(last_paren) && last_paren > 0) {
    code_text <- substr(code_text, 1, last_paren)
  }
  code_text
}

predmicror_assist_strip_inline_code <- function(text) {
  if (!nzchar(text)) {
    return(text)
  }
  text <- predmicror_assist_normalize_text(text)
  tokens <- c(
    "data\\(",
    "gsl_nls\\(",
    "summary\\(",
    "fitted\\(",
    "fitted_vals\\s*<-",
    "new_[A-Za-z]+\\s*<-",
    "pred\\s*<-",
    "predict\\(",
    "plot\\(",
    "lines\\(",
    "points\\(",
    "library\\(",
    "require\\("
  )
  pattern <- paste0("(?=\\b(", paste(tokens, collapse = "|"), "))")
  text <- gsub(pattern, "\n", text, perl = TRUE)
  text <- gsub("(?=\\b\\w+\\s*<-)", "\n", text, perl = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  code_like <- grepl(
    "<-|\\bdata\\(|\\bgsl_nls\\(|\\bsummary\\(|\\bfitted\\(|\\bpredict\\(|\\bplot\\(|\\blines\\(|\\bpoints\\(|\\blibrary\\(|\\brequire\\(",
    lines
  )
  out <- lines[!code_like]
  paste(out, collapse = "\n")
}

predmicror_assist_collect_context_lines <- function(files, pattern, context = 2, max_matches = 2) {
  if (!nzchar(pattern) || length(files) == 0) {
    return(character(0))
  }
  out <- character(0)
  for (path in files) {
    lines <- tryCatch(readLines(path, warn = FALSE),
      error = function(e) character(0)
    )
    if (length(lines) == 0) {
      next
    }
    matches <- which(grepl(pattern, tolower(lines), perl = TRUE))
    if (length(matches) == 0) {
      next
    }
    matches <- matches[seq_len(min(length(matches), max_matches))]
    for (idx in matches) {
      start <- max(1, idx - context)
      end <- min(length(lines), idx + context)
      snippet <- lines[start:end]
      line_nums <- start:end
      out <- c(out, sprintf("%s:%d:%s", path, line_nums, snippet))
    }
  }
  out
}
