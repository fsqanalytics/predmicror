---
title: 'predmicror: Fitting predictive microbiology models and assisted workflows in R'
authors:
  - name: Vasco Cadavez
    orcid: 0000-0002-3077-7414
    corresponding: true
    affiliation: "1"
  - name: Ursula Gonzales-Barron
    orcid: 0000-0002-8462-9775
    affiliation: "1"
affiliations:
  - index: 1
    name: "TODO: add affiliation(s)"
date: 21 December 2025
bibliography: paper.bib
tags:
  - predictive microbiology
  - growth models
  - inactivation models
  - R
---

# Summary

Predictive microbiology uses mathematical models to describe how microbial
populations grow or decline under controlled conditions. The `predmicror`
package provides an R interface for fitting widely used predictive
microbiology models to experimental data, supporting research in food safety,
shelf-life estimation, and process design. It includes primary growth models
(e.g., Huang, Baranyi, and reparameterized Gompertz) [@Huang2008; @Baranyi1993Modelling; @Zwietering1990],
inactivation models (e.g., Weibull-type survival curves) [@Boekel2002], and
cardinal models that describe the effects of temperature, pH, water activity,
or inhibitors on growth rates [@Rosso1995]. The package standardizes response
variables on the natural log scale and ships with example datasets and
vignettes, enabling reproducible model fitting and visualization. An optional
local assistant (CLI and Shiny app) helps users select models and draft
`gsl_nls` workflows for parameter estimation.

# Statement of need

Predictive microbiology models are a cornerstone of quantitative microbial
risk assessment and food process validation, but researchers often re-implement
model equations and fitting workflows in ad hoc scripts. This practice makes
results difficult to reproduce and compare across studies. While tools such as
GInaFiT provide accessible inactivation model fitting in spreadsheet
workflows [@Geeraerd2005], they are limited in scope and do not integrate well
with R-based analysis pipelines. `predmicror` addresses this gap by offering a
single, consistent R API for growth, inactivation, and cardinal models drawn
from the canonical literature [@Baranyi1993Modelling; @Huang2008; @Zwietering1990; @Boekel2002; @Rosso1995].
The package is designed to be used directly with nonlinear regression
procedures (via `gsl_nls`), which supports transparent and reproducible
parameter estimation workflows [@Dolan2013]. Included datasets and vignettes
provide reference analyses for common experimental designs, and the optional
assistant lowers the entry barrier for new users by translating natural
language questions into runnable R code.

`predmicror` is suitable for classroom demonstrations, exploratory analysis,
and reproducible research pipelines. The built-in vignettes and datasets
illustrate full workflows for growth, inactivation, and cardinal modeling
across common conditions (e.g., temperature, pH, water activity), and provide
ready-to-adapt templates for new studies.

# Acknowledgements

This work received no specific funding. TODO: update this section with any
relevant funding sources or acknowledgements.

# References
