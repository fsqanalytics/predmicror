## Resubmission

This is a resubmission. In this version I have:

* removed the redundant "in R" from the `Title` field;
* added method references to the `Description` field of `DESCRIPTION` in CRAN
  DOI format;
* removed `\dontrun{}` usage by unwrapping fast executable examples and
  keeping the Shiny app example interactive-only.

## Test environments

* local Ubuntu 24.04.4 LTS, R 4.3.3

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

## Submission notes

This is the first CRAN submission of predmicror.

The package provides predictive microbiology model functions and convenience
wrappers for fitting primary growth, microbial inactivation, dynamic, omnibus,
and cardinal parameter models to experimental data.
