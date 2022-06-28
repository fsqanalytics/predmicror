test_that("BaranyiFM() fits a full growth model", {
  library(gslnls)
  data(growthfull)
  fit <- gsl_nls(lnN ~ BaranyiFM(Time, Y0, Ymax, MUmax, lag),
               data=growthfull,
               start =  list(Y0=0, Ymax=22, MUmax=1.7, lag=5))
  preds <- round(predict(fit), digits=2)
  expect_equal(c(-0.01,-0.01,0.11,1.86,5.44,9.17,
                 12.91,16.63,19.99,21.08,21.13,21.13,21.13), preds)
})
