library(testthat)
library(gslnls)
library(predmicror)

test_that("BaranyiFM model fits correctly", {
  data(growthfull)
  initial_values <- list(Y0 = -0.1, Ymax = 22, MUmax = 1.7, lag = 5)
  
  fit <- gsl_nls(lnN ~ BaranyiFM(Time, Y0, Ymax, MUmax, lag),
    data = growthfull,
    start = initial_values
  )
  
  # Check if the model converges
  expect_true(summary(fit)$convInfo$isConv)
  
  # Check if the estimated parameters are within a reasonable range
  estimated_params <- coef(fit)
  expect_true(estimated_params["Y0"] > -1 && estimated_params["Y0"] < 1)
  expect_true(estimated_params["Ymax"] > 20 && estimated_params["Ymax"] < 24)
  expect_true(estimated_params["MUmax"] > 1.5 && estimated_params["MUmax"] < 2)
  expect_true(estimated_params["lag"] > 4 && estimated_params["lag"] < 6)
})
