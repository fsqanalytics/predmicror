.onAttach <- function( lib, pkg ) {
   packageStartupMessage(
      paste0( "\n Please cite the 'predmicror' package as: \n",
         "Vasco Cadavez and Ursula Gonzales-Barron  (2022). ",
         "predmicror: Fitting predictive microbiology models in r \n",
         "If you have questions, suggestions, or comments \n",
         "regarding the 'predmicror' package \n",
          "https://github.com/fsqanalytics/predmicror"),
      domain = NULL,  appendLF = TRUE )
}
