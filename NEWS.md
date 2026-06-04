# predmicror 1.3.1.9000

* Improve `predmicror_assistant()` with a deterministic model registry, wrapper-first code generation, static code validation, and `return_trace` diagnostics.
* Make the assistant fall back to registry-based output when Ollama is unavailable instead of failing immediately.
* Bundle `inst/shiny/predmicror-assistant/app.R` and extend `predmicror_assistant_app()` with model, root, host, port, and browser arguments plus a fallback app.
* Add assistant tests for registry metadata, deterministic output, and Shiny app bundling.
* Add data-aware assistant support for data frames and uploaded `.csv`, `.tsv`, `.xls`, and `.xlsx` files, including automatic profiling, column detection, and data-specific wrapper code.
* Improve the assistant Shiny app with a card-based layout, separate Answer/Code/Data/Trace views, local Ollama model selection, and manual task/column overrides.

# predmicror 1.3.1

* Polish the GitHub README to reflect the current fitting, diagnostics, and model-comparison API.
* Add explicit package website and issue tracker links to the README.
* Make applied inactivation and cardinal-model vignettes more robust by using explicit fitted/residual column access.
* Group applied workflow articles in the pkgdown articles index.

# predmicror 1.3.0

## Documentation and applied workflows

* Added an applied vignette for microbial inactivation models using `fit_inactivation()`, `predmicror_augment()`, `fit_metrics()`, and `compare_models()`.
* Added an applied vignette for cardinal parameter models using `fit_cardinal()`, diagnostics helpers, and model comparison tools.
* Expanded examples showing safer post-fitting workflows and prediction over new data grids.

# predmicror 1.2.1

* Register default S3 methods for `predmicror_augment()` and `fit_metrics()` in roxygen documentation.
* Add the pkgdown site URL to `DESCRIPTION`.
* Add the new fitting and diagnostic topics to the pkgdown reference index.
* Ignore temporary phase overlay folders created while applying local hotfixes.

# predmicror 1.2.0

* Add `predmicror_augment()` to extract original data, fitted values, residuals, model name, and model type from `predmicror_fit` objects.
* Add `as.data.frame.predmicror_fit()` as a lightweight base-R shortcut for `predmicror_augment()`.
* Add `fit_metrics()` to calculate residual diagnostics and information criteria for fitted models, including SSE, RMSE, MAE, bias, residual standard error, R2, adjusted R2, log-likelihood, AIC, BIC, and convergence status.
* Add `compare_models()` to combine diagnostics across multiple fitted models and sort by AIC, BIC, RMSE, or MAE.
* Add tests for diagnostic extraction, model metrics, and model comparison.
* Add a model-comparison vignette showing how to compare alternative fitted predictive microbiology models.

# predmicror 1.1.3

* Declare `shiny` as a suggested package for assistant functions.
* Import `utils::tail()` for assistant history formatting.
* Keep `R CMD check` free of errors and warnings, apart from environment-specific timestamp notes.

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
* Add numeric stability guards with `log1p()`, `expm1()`, and bounded square roots across growth and cardinal models.
* Expose the Richards shape parameter and align examples with log10 inactivation scales.
* Correct dataset documentation and row counts.
* Add testthat coverage for growth, cardinal, and inactivation models.

# predmicror 1.0.0

* First release.
* Primary growth models.
* Inactivation models.
