
# predmicror

[![R-CMD-check](https://github.com/fsqanalytics/predmicror/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fsqanalytics/predmicror/actions/workflows/R-CMD-check.yaml)

`predmicror` provides predictive microbiology model functions and
convenience wrappers for fitting primary growth, microbial inactivation,
and cardinal parameter models to experimental data.

## Installation

You can install the development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("fsqanalytics/predmicror")
```

## Basic use

The exported model functions can still be used directly inside
`gslnls::gsl_nls()`. For routine analyses, the `fit_*()` wrappers
provide a smaller and safer interface:

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

You can list the models available through the wrappers with:

``` r
predmicror_models()
predmicror_models("growth")
predmicror_models("inactivation")
predmicror_models("cardinal")
```

## Model diagnostics and comparison

After fitting a model, use `predmicror_augment()` to extract observed
values, fitted values, and residuals:

``` r
augmented <- predmicror_augment(fit)
head(augmented)
```

Use `fit_metrics()` for one fitted model:

``` r
fit_metrics(fit)
```

Use `compare_models()` to compare alternative fitted models on the same
response scale:

``` r
huang <- fit_growth(
  growthfull,
  model = "HuangFM",
  time = "Time",
  response = "lnN",
  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
)

baranyi <- fit_growth(
  growthfull,
  model = "BaranyiFM",
  time = "Time",
  response = "lnN",
  start = list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5)
)

compare_models(huang = huang, baranyi = baranyi, sort_by = "AIC")
```

## Response scale conventions

The fitting wrappers do not transform your response automatically. Use
these scales before fitting:

- growth models: natural logarithm of microbial concentration, usually
  `lnN`;
- inactivation models: base 10 logarithm of microbial concentration,
  usually `logN`;
- cardinal models: square root of the growth rate, usually `sqrtGR`.

More examples are available on the package website:
<https://fsqanalytics.github.io/predmicror/>.

## Reporting bugs

Please report bugs at
<https://github.com/fsqanalytics/predmicror/issues>.
