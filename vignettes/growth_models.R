## -----------------------------------------------------------------------------
library(predmicror)
library(gslnls)
library(ggplot2)

## -----------------------------------------------------------------------------
data(growthfull)

## -----------------------------------------------------------------------------
growthfull
head(growthfull)

## ----eval=FALSE---------------------------------------------------------------
#  growthfull <- read.csv("growthfull.csv", sep = ";", header = TRUE)

## -----------------------------------------------------------------------------
str(growthfull)
names(growthfull)

## -----------------------------------------------------------------------------
plot(lnN ~ Time,
  data = growthfull,
  xlab = "Time (hours)", ylab = "ln N",
  xlim = c(0, 20), ylim = c(0, 22)
)

## ----eval=FALSE---------------------------------------------------------------
#  png("growthfull.png")
#  plot(lnN ~ Time,
#    data = growthfull,
#    xlab = "Time (hours)", ylab = "ln N",
#    xlim = c(0, 20), ylim = c(0, 22)
#  )
#  dev.off()

## -----------------------------------------------------------------------------
start_values <- list(Y0 = 0.02, Ymax = 22, MUmax = 1.6, lag = 5)

## -----------------------------------------------------------------------------
fit <- gsl_nls(lnN ~ HuangFM(Time, Y0, Ymax, MUmax, lag),
  data = growthfull,
  start = start_values
)

## -----------------------------------------------------------------------------
summary(fit)
coef(fit)

## -----------------------------------------------------------------------------
newTimes <- data.frame(Time = seq(0, 24, by = 0.1))
fits <- data.frame(predict(fit, newdata = newTimes, interval = "confidence", level = 0.95))
str(fits)
preds <- data.frame(predict(fit, newdata = newTimes, interval = "prediction", level = 0.95))
str(preds)

## -----------------------------------------------------------------------------
plot(newTimes$Time, fits$fit,
  type = "l", col = "blue",
  xlab = "time (days)", ylab = "logN",
  main = "Huang full model"
)
points(growthfull$Time, growthfull$lnN)
lines(newTimes$Time, fits$upr, col = "red")
lines(newTimes$Time, fits$lwr, col = "red")

## -----------------------------------------------------------------------------
plot(newTimes$Time, fits$fit,
  type = "l", col = "blue",
  xlab = "time (days)", ylab = "logN", ylim = c(-1, 22), xlim = c(0, 24),
  main = "Huang full model"
)
points(growthfull$Time, growthfull$lnN)
lines(newTimes$Time, preds$upr, col = "red")
lines(newTimes$Time, preds$lwr, col = "red")

## -----------------------------------------------------------------------------
data(growthnolag)
growthnolag

## -----------------------------------------------------------------------------
start_values <- list(Y0 = 0.01, Ymax = 22, MUmax = 1.6)
fit <- gsl_nls(lnN ~ FangNLM(Time, Y0, Ymax, MUmax),
  data = growthnolag,
  start = start_values
)
fit

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
newTimes <- data.frame(Time = seq(0, 18, by = 0.1))
fits <- data.frame(predict(fit, newdata = newTimes, interval = "confidence", level = 0.95))
str(fits)
preds <- data.frame(predict(fit, newdata = newTimes, interval = "prediction", level = 0.95))

## -----------------------------------------------------------------------------
plot(newTimes$Time, fits$fit,
  type = "l", col = "blue",
  xlab = "time (days)", ylab = "logN",
  main = "Fang no lag model"
)
points(growthnolag$Time, growthnolag$lnN)
lines(newTimes$Time, fits$upr, col = "red")
lines(newTimes$Time, fits$lwr, col = "red")

## -----------------------------------------------------------------------------
plot(newTimes$Time, fits$fit,
  type = "l", col = "blue",
  xlab = "time (days)", ylab = "logN", ylim = c(-1, 22), xlim = c(0, 18),
  main = "Fang no lag model"
)
points(growthnolag$Time, growthnolag$lnN)
lines(newTimes$Time, preds$upr, col = "red")
lines(newTimes$Time, preds$lwr, col = "red")

## -----------------------------------------------------------------------------
data(growthred)
growthred

## -----------------------------------------------------------------------------
start_values <- list(Y0 = 0.01, MUmax = 1.6, lag = 5)
fit <- gsl_nls(lnN ~ BuchananRM(Time, Y0, MUmax, lag),
  data = growthred,
  start = start_values
)

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
newTimes <- data.frame(Time = seq(0, 16, by = 0.1))
fits <- data.frame(predict(fit, newdata = newTimes, interval = "confidence", level = 0.95))
str(fits)
preds <- data.frame(predict(fit, newdata = newTimes, interval = "prediction", level = 0.95))

## -----------------------------------------------------------------------------
plot(newTimes$Time, fits$fit,
  type = "l", col = "blue",
  xlab = "time (days)", ylab = "logN",
  main = "Buchanan reduced model"
)
points(growthred$Time, growthred$lnN)
lines(newTimes$Time, fits$upr, col = "red")
lines(newTimes$Time, fits$lwr, col = "red")

## -----------------------------------------------------------------------------
plot(newTimes$Time, fits$fit,
  type = "l", col = "blue",
  xlab = "time (days)", ylab = "logN", ylim = c(-1, 22), xlim = c(0, 16),
  main = "Buchanan reduced model"
)
points(growthred$Time, growthred$lnN)
lines(newTimes$Time, preds$upr, col = "red")
lines(newTimes$Time, preds$lwr, col = "red")

