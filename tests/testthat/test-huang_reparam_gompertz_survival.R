library(testthat)
library(gslnls)
library(predmicror)

test_that("HuangRGS model fits correctly", {
  data(bixina)
  initial_values <- list(Y0 = 5.6, k = 0.37, M = 6.8)
  
  fit <- gsl_nls(lnN ~ HuangRGS(Time, Y0, k, M),
    data = bixina,
    start = initial_values
  )
  
  # Check if the model converges
  expect_true(summary(fit)$convInfo$isConv)
  
  # Check if the estimated parameters are within a reasonable range
  estimated_params <- coef(fit)
  expect_true(estimated_params["Y0"] > 5 && estimated_params["Y0"] < 6)
  expect_true(estimated_params["k"] > 0.1 && estimated_params["k"] < 0.2)
  expect_true(estimated_params["M"] > 12 && estimated_params["M"] < 13)
})
