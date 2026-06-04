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

test_that("assistant profiles uploaded-like growth data", {
  dat <- data.frame(
    Time = c(0, 1, 2, 3),
    logN = c(2.0, 2.4, 3.1, 4.2)
  )
  profile <- predmicror:::predmicror_assist_profile_data(dat)

  expect_equal(profile$n_rows, 4)
  expect_equal(profile$columns$time, "Time")
  expect_equal(profile$columns$response, "logN")
  expect_equal(profile$task, "growth")
  expect_equal(profile$candidate, "HuangFM")
})

test_that("assistant reads csv files and generates data-aware code", {
  dat <- data.frame(
    Time = c(0, 1, 2, 3),
    logN = c(7.0, 6.2, 5.1, 4.5)
  )
  path <- tempfile(fileext = ".csv")
  utils::write.csv(dat, path, row.names = FALSE)

  result <- predmicror_assistant(
    "analisa estes dados de inativacao",
    file = path,
    root = ".",
    backend = "deterministic",
    return_trace = TRUE
  )

  expect_match(result$answer, "Perfil dos dados")
  expect_match(result$trace$code, "dat <- utils::read.table", fixed = TRUE)
  expect_match(result$trace$code, "fit_inactivation", fixed = TRUE)
  expect_match(result$trace$code, "Time", fixed = TRUE)
  expect_match(result$trace$code, "logN", fixed = TRUE)
  expect_true(isTRUE(result$trace$validation$ok))
})

test_that("assistant profile overrides are respected", {
  dat <- data.frame(
    t = c(0, 1, 2, 3),
    y = c(7.0, 6.3, 5.1, 4.6),
    other = c(1, 2, 3, 4)
  )

  result <- predmicror_assistant(
    "fit this inactivation dataset",
    data = dat,
    root = ".",
    backend = "deterministic",
    return_trace = TRUE,
    task = "inactivation",
    time = "t",
    response = "y"
  )

  expect_equal(result$trace$data_profile$task, "inactivation")
  expect_equal(result$trace$data_profile$columns$time, "t")
  expect_equal(result$trace$data_profile$columns$response, "y")
  expect_match(result$trace$code, "fit_inactivation", fixed = TRUE)
  expect_match(result$trace$code, "time = \"t\"", fixed = TRUE)
  expect_match(result$trace$code, "response = \"y\"", fixed = TRUE)
})

test_that("assistant app has fallback model choices", {
  models <- predmicror:::predmicror_assist_available_ollama_models("example-model")
  expect_true("example-model" %in% models)
  expect_true(length(models) >= 1)
})

test_that("assistant generates dynamic growth code when temperature profile is present", {
  dat <- data.frame(
    Time = c(0, 2, 4, 6, 8, 10),
    logN = c(2.0, 2.1, 2.4, 3.0, 3.8, 4.6),
    Temp = c(8, 10, 12, 15, 15, 14)
  )

  result <- predmicror_assistant(
    "fit these dynamic growth data",
    data = dat,
    root = ".",
    backend = "deterministic",
    return_trace = TRUE
  )

  expect_equal(result$trace$data_profile$task, "growth")
  expect_true(isTRUE(result$trace$data_profile$dynamic))
  expect_match(result$answer, "dynamic Huang growth model", fixed = TRUE)
  expect_match(result$answer, "fit_dynamic_growth", fixed = TRUE)
  expect_match(result$trace$code, "dynamic_profile", fixed = TRUE)
  expect_match(result$trace$code, "fit_dynamic_growth", fixed = TRUE)
  expect_match(result$trace$code, "temperature = dat[[\"Temp\"]]", fixed = TRUE)
  expect_true(isTRUE(result$trace$validation$ok))
})

test_that("assistant generates dynamic inactivation code when temperature profile is present", {
  dat <- data.frame(
    Time = c(0, 2, 4, 6, 8, 10),
    logN = c(7.0, 6.6, 6.0, 5.2, 4.5, 3.9),
    Temp = c(60, 60, 62, 62, 64, 64)
  )

  result <- predmicror_assistant(
    "fit these dynamic inactivation data",
    data = dat,
    root = ".",
    backend = "deterministic",
    return_trace = TRUE
  )

  expect_equal(result$trace$data_profile$task, "inactivation")
  expect_true(isTRUE(result$trace$data_profile$dynamic))
  expect_match(result$answer, "dynamic Weibull-Peleg inactivation model", fixed = TRUE)
  expect_match(result$answer, "fit_dynamic_inactivation", fixed = TRUE)
  expect_match(result$trace$code, "dynamic_profile", fixed = TRUE)
  expect_match(result$trace$code, "fit_dynamic_inactivation", fixed = TRUE)
  expect_match(result$trace$code, "temperature = dat[[\"Temp\"]]", fixed = TRUE)
  expect_true(isTRUE(result$trace$validation$ok))
})
