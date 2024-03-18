#' Weibull inactivation model Mafart
#'
#' \code{WeibullM} inactivation model for microbial inactivation curve.
#' Returns the model parameters estimated according to data collected in microbial inactivation experiments.
#'
#' The model's inputs are:
#'
#' \code{t}: time, assuming time zero as the beginning of the experiment.
#'
#' \code{Y(t)}: the base 10 logarithm of the bacterial concentration ($log10(N(t)$) measured at time t.
#'
#' Users should make sure that the bacterial concentration input is entered
#'  in base 10 logarithm, \code{Y(t) = log10(N(t))}.
#'
#' @param x is a numeric vector indicating the heating time under a constant temperature of the experiment
#'
#' @param Y0 is the base 10 logarithm of the initial (time=0) bacterial concentration (N0)
#'
#' @param sigma is the time of first decimal reduction
#'
#' @param alpha which is a shape parameter
#'
#' @return An object of nls class with the fitted parameters of the model
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Weibull Inactivation Mafart
#'
#' @references
#' \insertRef{Mafart2002}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(bixina)
#' initial_values <- list(Y0 = 2.5, sigma = 2, alpha = 2)
#' bixina$N <- exp(bixina$lnN)
#' bixina$logN <- log10(bixina$N)
#' fit <- gsl_nls(logN ~ WeibullM(Time, Y0, sigma, alpha),
#'   data = bixina,
#'   start = initial_values
#' )
#' summary(fit)
#'
#' plot(logN ~ Time, data = bixina)
#' lines(bixina$Time, predict(fit), col = "blue")
#'
WeibullM <- function(x, Y0, sigma, alpha) {
  if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }
  result <- Y0 - (x / sigma)^alpha
  return(result)
}
