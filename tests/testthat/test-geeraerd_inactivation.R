library(testthat)
library(gslnls)
library(predmicror)

test_that("GeeraerdST model fits correctly", {
  data(mafart2005Li11)
  mafart2005Li11$lnN <- log(10) * mafart2005Li11$logN
  initial_values <- list(Y0 = 18, Yres = 2, kmax = 0.7, Sl = 4)
  
  fit <- gsl_nls(lnN ~ GeeraerdST(Time, Y0, Yres, kmax, Sl),
    data = mafart2005Li11,
    start = initial_values
  )
  
  # Check if the model converges
  expect_true(summary(fit)$convInfo$isConv)
  
  # Check if the estimated parameters are within a reasonable range
  estimated_params <- coef(fit)
  expect_true(estimated_params["Y0"] > 23 && estimated_params["Y0"] < 24)
  expect_true(estimated_params["Yres"] > 15 && estimated_params["Yres"] < 16)
  expect_true(estimated_params["kmax"] > 0.5 && estimated_params["kmax"] < 1)
  expect_true(estimated_params["Sl"] > 5 && estimated_params["Sl"] < 6)
})
