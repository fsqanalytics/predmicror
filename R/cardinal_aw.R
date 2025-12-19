#' Cardinal model for water activity
#'
#' \code{CMAW} function to fit the water activity cardinal model (Rosso et al., 1993).
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' The model's inputs are:
#'
#' \code{x}: Water activity
#'
#' \code{sqrtGR}: the square root of the growth rate ($h^{-1}$)
#'
#' Users should make sure that the growth rate input is entered after a square root transformation, \code{sqrGR = sqrt(GR)}.
#'
#' @param x is a numeric vector indicating the water activity of the experiment
#'
#' @param AWmin is minimum water activity for growth
#'
#' @param MUopt is the optimum growth rate
#'
#' @param AWopt is optimum water activity for growth
#'
#' @return A numeric vector with the fitted values
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Cardinal Rosso water activity
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
#' data(aw)
#' initial_values <- list(AWmin = 0.89, MUopt = 1.0, AWopt = 0.98)
#' fit <- gsl_nls(sqrtGR ~ CMAW(aw, AWmin, MUopt, AWopt),
#'   data = aw,
#'   start = initial_values
#' )
#' summary(fit)
#'
CMAW <- function(x, AWmin, MUopt, AWopt) {
  CMaw <- numeric(length(x))
  idx <- x >= AWmin & x <= 1.000
  if (any(idx)) {
    num <- (x[idx] - 1.000) * (x[idx] - AWmin)^2
    den <- (AWopt - AWmin) * ((AWopt - AWmin) * (x[idx] - AWopt) -
      (AWopt - 1.000) * (AWopt + AWmin - 2 * x[idx]))
    CMaw[idx] <- MUopt * (num / den)
  }
  result <- sqrt(pmax(CMaw, 0))
  result
}
