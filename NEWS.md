---
output: 
  html_document: 
    keep_md: yes
---

# predmicror 1.0.1

* Fix Rosso full model parameter order and Baranyi reduced formulation.
* Add numeric stability guards (log1p/expm1, bounded sqrt) across growth and cardinal models.
* Expose Richards shape parameter and align examples with log10 inactivation scales.
* Correct dataset documentation and row counts.
* Add testthat coverage for growth, cardinal, and inactivation models.

# predmicror 1.0.0

* First release
* Primary growth models
* Inactivation models
