#' Cardinal model for pH
#'
#' \code{CMPH} function to fit the pH cardinal model (Rosso et al, 1993).
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' The model's inputs are:
#'
#' \code{x}: pH
#'
#' \code{sqrtGR}: the square root of the growth rate ($time^{-1}$)
#'
#' Users should make sure that the growth rate input is entered after a square root transformation, \code{sqrGR = sqrt(GR)}.
#'
#' @param x is a numeric vector indicating the pH of the experiment
#'
#' @param pHmax is the maximum pH for growth
#'
#' @param pHmin is the minimum pH for growth
#'
#' @param MUopt is the optimum growth rate
#'
#' @param pHopt is the optimum pH for growth
#'
#' @return An object of nls class with the fitted parameters of the model
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Cardinal Rosso Temperature
#'
#' @references
#' \insertRef{Rosso1995}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(ph)
#' initial_values <- list(pHmax = 9, pHmin = 3, MUopt = 1.0, pHopt = 7)
#' fit <- gsl_nls(sqrtGR ~ CMPH(pH, pHmax, pHmin, MUopt, pHopt),
#'   data = ph,
#'   start = initial_values
#' )
#' summary(fit)
#'
CMPH <- function(x, pHmax, pHmin, MUopt, pHopt) {
  if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }

  CMph <- ifelse(x < pHmin | x > pHmax, 0, MUopt * ((x - pHmax) * (x - pHmin)) / ((pHopt - pHmin) * (x - pHopt) - (pHopt - pHmax) * (pHmin - x)))
  result <- sqrt(CMph)
  return(result)
}
