# Apply predmicror 1.3.1 vignette hotfix
# Run from the root of the predmicror repository.

root <- normalizePath(getwd(), mustWork = TRUE)
this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, mustWork = TRUE), error = function(e) NA_character_)
if (is.na(this_file)) {
  this_file <- normalizePath("./predmicror_1_3_1_vignette_hotfix/apply_predmicror_1_3_1_vignette_hotfix.R", mustWork = TRUE)
}
overlay <- dirname(this_file)

copy_one <- function(from, to) {
  src <- file.path(overlay, "files", from)
  dst <- file.path(root, to)
  if (!file.exists(src)) stop("Missing overlay file: ", src, call. = FALSE)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  file.copy(src, dst, overwrite = TRUE)
  message("Updated ", to)
}

copy_one("vignettes/inactivation-models.Rmd", "vignettes/inactivation-models.Rmd")
copy_one("vignettes/cardinal-models.Rmd", "vignettes/cardinal-models.Rmd")

message("\nRecommended next commands:")
message("devtools::check()")
message("pkgdown::build_site()")
