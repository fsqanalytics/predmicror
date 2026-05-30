library(testthat)
library(gslnls)
library(predmicror)

test_that("WeibullPH model fits correctly", {
  data(bixina)
  initial_values <- list(Y0 = 6.0, k = 1.0, alpha = 0.2)
  
  fit <- gsl_nls(lnN ~ WeibullPH(Time, Y0, k, alpha),
    data = bixina,
    start = initial_values
  )
  
  # Check if the model converges
  expect_true(summary(fit)$convInfo$isConv)
  
  # Check if the estimated parameters are within a reasonable range
  estimated_params <- coef(fit)
  expect_true(estimated_params["Y0"] > 5 && estimated_params["Y0"] < 7)
  expect_true(estimated_params["k"] > 0 && estimated_params["k"] < 2)
  expect_true(estimated_params["alpha"] > 2 && estimated_params["alpha"] < 3)
})
