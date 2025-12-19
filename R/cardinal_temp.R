#' Cardinal model for temperature
#'
#' \code{CMTI} function to fit the temperature cardinal model (Rosso et al, 1993).
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' The model's inputs are:
#'
#' \code{x}: Temperature
#'
#' \code{sqrtGR}: the square root of the growth rate ($h^{-1}$)
#'
#' Users should make sure that the growth rate input is entered after a square root transformation, \code{sqrGR = sqrt(GR)}.
#'
#' @param x is a numeric vector indicating the temperature of the experiment
#'
#' @param Tmax maximum temperature for growth
#'
#' @param Tmin is minimum temperature for growth
#'
#' @param MUopt is the optimum growth rate
#'
#' @param Topt is optimum temperature for growth
#'
#' @return A numeric vector with the fitted values
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
#' data(salmonella)
#' initial_values <- list(Tmax = 42, Tmin = 1, MUopt = 1.0, Topt = 37)
#' fit <- gsl_nls(sqrtGR ~ CMTI(Temp, Tmax, Tmin, MUopt, Topt),
#'   data = salmonella,
#'   start = initial_values
#' )
#' summary(fit)
#'
#' plot(salmonella$Temp, salmonella$sqrtGR^2)
#' lines(salmonella$Temp, fitted(fit)^2, col = "green")
#' plot(salmonella$Temp, salmonella$sqrtGR)
#' lines(salmonella$Temp, fitted(fit), col = "red")
#'
CMTI <- function(x, Tmax, Tmin, MUopt, Topt) {
  CMT <- numeric(length(x))
  idx <- x > Tmin & x < Tmax
  if (any(idx)) {
    num <- (x[idx] - Tmax) * (x[idx] - Tmin)^2
    den <- (Topt - Tmin) * ((Topt - Tmin) * (x[idx] - Topt) -
      (Topt - Tmax) * (Topt + Tmin - 2 * x[idx]))
    CMT[idx] <- MUopt * (num / den)
  }
  result <- sqrt(pmax(CMT, 0))
  result
}
