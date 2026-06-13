
# predmicror

[![R-CMD-check](https://github.com/fsqanalytics/predmicror/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fsqanalytics/predmicror/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/fsqanalytics/predmicror/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/fsqanalytics/predmicror/actions/workflows/pkgdown.yaml)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

`predmicror` provides predictive microbiology model functions and
convenience wrappers for fitting primary growth, microbial inactivation,
and cardinal parameter models to experimental data. It also supports
**omnibus** (nonlinear mixed-effects) and **dynamic** (ODE-based
time-varying) modelling.

The package can be used in two complementary ways:

- call the exported model equations directly inside `gslnls::gsl_nls()`;
- use the higher-level `fit_*()` wrappers for routine fitting,
  diagnostics, and model comparison.

## Installation

``` r
install.packages("devtools")
devtools::install_github("fsqanalytics/predmicror")
```

## Model catalogue

List all supported models with:

``` r
library(predmicror)

predmicror_models()
predmicror_models("growth")
predmicror_models("inactivation")
predmicror_models("cardinal")
```

| Workflow                | Wrapper                             | Response scale              | Examples                              |
|-------------------------|-------------------------------------|-----------------------------|---------------------------------------|
| Primary growth          | `fit_growth()`                      | natural log (`lnN`)         | `HuangFM`, `BaranyiFM`, `RossoFM`     |
| Microbial inactivation  | `fit_inactivation()`                | base-10 log (`logN`)        | `WeibullM`, `GeeraerdST`, `WeibullPH` |
| Cardinal parameters     | `fit_cardinal()`                    | sqrt growth rate (`sqrtGR`) | `CMTI`, `CMPH`, `CMAW`, `CMInh`       |
| Dynamic (time-varying)  | `fit_dynamic_growth/inactivation()` | natural log (`lnN`)         | Huang ODE, Weibull-Peleg ODE          |
| Omnibus (mixed-effects) | `fit_omnibus_growth/inactivation()` | per primary model           | any primary + secondary covariates    |

## Quick start: growth

``` r
library(predmicror)

data(growthfull)

fit <- fit_growth(
  data = growthfull,
  model = "HuangFM",
  time = "Time",
  response = "lnN",
  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
)

summary(fit)
coef(fit)
plot(fit)
```

## Inactivation

``` r
data(bixina)

fit <- fit_inactivation(
  data = bixina,
  model = "WeibullM",
  time = "Time",
  response = "lnN",
  start = list(Y0 = 5.6, sigma = 12, alpha = 1)
)

fit_metrics(fit)
plot(fit)
```

## Cardinal parameter model

``` r
data(salmonella)

fit <- fit_cardinal(
  data = salmonella,
  model = "CMTI",
  x = Temp,
  response = sqrtGR,
  start = list(Tmax = 42, Tmin = 1, MUopt = 1, Topt = 37)
)

coef(fit)
```

## Model comparison

``` r
huang <- fit_growth(growthfull, model = "HuangFM", time = "Time",
  response = "lnN", start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5))
baranyi <- fit_growth(growthfull, model = "BaranyiFM", time = "Time",
  response = "lnN", start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5))

compare_models(HuangFM = huang, BaranyiFM = baranyi, sort_by = "AIC")
```

## Dynamic (time-varying environment)

``` r
profile <- dynamic_profile(
  time = c(0, 5, 10),
  temperature = c(12, 20, 25)
)

pred <- predict_dynamic_growth(
  profile = profile,
  start = list(logN0 = 2, logNmax = 8, a = 0.08, Tmin = 5, lag = 1),
  times = seq(0, 10, length.out = 50)
)

plot(pred$time, pred$logN, type = "l")
```

## Omnibus (mixed-effects)

``` r
# Each group (Condition) has its own curve
dat <- data.frame(
  Condition = rep(1:3, each = 7),
  Time = rep(seq(0, 6), 3),
  lnN = HuangNLM(rep(seq(0, 6), 3), Y0 = 2, Ymax = 8, MUmax = 0.6)
)

fit <- fit_omnibus_growth(
  data = dat,
  primary = "HuangNLM",
  time = "Time",
  response = "lnN",
  group = "Condition",
  random = Y0 ~ 1,
  start = c(Y0 = 2, Ymax = 8, MUmax = 0.6)
)
```

## Response scale conventions

The fitting wrappers do **not** transform the response automatically:

- **Growth models**: natural logarithm of concentration (`lnN`)
- **Inactivation models**: base-10 logarithm (`logN`)
- **Cardinal models**: square root of growth rate (`sqrtGR`)
- **Dynamic models**: natural logarithm (`lnN`)

## Documentation

Full reference and vignettes:

<https://fsqanalytics.github.io/predmicror/>

## Citation

``` r
citation("predmicror")
```

## Bugs

<https://github.com/fsqanalytics/predmicror/issues>
