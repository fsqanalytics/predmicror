test_that("assistant registry includes wrapper metadata", {
  registry <- predmicror:::predmicror_assist_registry()

  expect_true("HuangFM" %in% names(registry))
  expect_equal(registry$HuangFM$wrapper, "fit_growth")
  expect_equal(registry$WeibullM$wrapper, "fit_inactivation")
  expect_equal(registry$CMTI$wrapper, "fit_cardinal")
})

test_that("assistant deterministic backend returns verified wrapper code", {
  result <- predmicror_assistant(
    "fit a Huang full growth model",
    root = ".",
    backend = "deterministic",
    return_trace = TRUE
  )

  expect_type(result$answer, "character")
  expect_match(result$answer, "fit_growth", fixed = TRUE)
  expect_match(result$trace$code, "fit_growth", fixed = TRUE)
  expect_true(isTRUE(result$trace$validation$ok))
  expect_equal(result$trace$backend, "deterministic")
})

test_that("assistant app is bundled in inst", {
  app_dir <- system.file("shiny", "predmicror-assistant", package = "predmicror")

  expect_true(nzchar(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})
