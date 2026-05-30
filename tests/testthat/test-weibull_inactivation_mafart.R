library(testthat)
library(gslnls)
library(predmicror)

test_that("WeibullM model fits correctly with natural log", {
  data(bixina)
  initial_values <- list(Y0 = 5.75, sigma = 12.8, alpha = 2.4)
  
  fit <- gsl_nls(lnN ~ WeibullM(Time, Y0, sigma, alpha),
    data = bixina,
    start = initial_values
  )
  
  # Check if the model converges
  expect_true(summary(fit)$convInfo$isConv)
  
  # Check if the estimated parameters are within a reasonable range
  estimated_params <- coef(fit)
  expect_true(estimated_params["Y0"] > 5 && estimated_params["Y0"] < 6)
  expect_true(estimated_params["sigma"] > 12 && estimated_params["sigma"] < 13)
  expect_true(estimated_params["alpha"] > 2 && estimated_params["alpha"] < 3)
})
