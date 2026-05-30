# predmicror 1.1.0

* Add `fit_growth()`, `fit_inactivation()`, and `fit_cardinal()` wrappers around `gslnls::gsl_nls()`.
* Add `predmicror_models()` to list wrapper-supported models and required starting parameters.
* Add the `predmicror_fit` class with `print()`, `summary()`, `coef()`, `fitted()`, `residuals()`, `predict()`, `plot()`, `vcov()`, `logLik()`, `AIC()`, and `BIC()` methods.
* Add input validation for data columns and starting values in the fitting wrappers.
* Update README content, package metadata, `.Rbuildignore`, and CI workflow templates.
* Complete the `WeibullMM()` example and fix the `HuangNLM()` example label.
* Configure testthat edition 3.

# predmicror 1.0.1

* Fix Rosso full model parameter order and Baranyi reduced formulation.
* Add numeric stability guards (log1p/expm1, bounded sqrt) across growth and cardinal models.
* Expose Richards shape parameter and align examples with log10 inactivation scales.
* Correct dataset documentation and row counts.
* Add testthat coverage for growth, cardinal, and inactivation models.

# predmicror 1.0.0

* First release.
* Primary growth models.
* Inactivation models.
