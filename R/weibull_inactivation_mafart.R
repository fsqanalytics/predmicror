#' Weibull inactivation model Mafart
#'
#' \code{WeibullM} inactivation model for microbial inactivation curve.
#' Returns the model parameters estimated according to data collected in microbial inactivation experiments.
#'
#' The model's inputs are:
#'
#' \code{t}: time, assuming time zero as the beginning of the experiment.
#'
#' \code{Y(t)}: the natural logarithm of the bacterial concentration (\code{ln(N(t))}) measured at time t.
#'
#' Users should make sure that the bacterial concentration input is entered
#'  in natural logarithm, \code{Y(t) = ln(N(t))}.
#'
#' @param x is a numeric vector indicating the heating time under a constant temperature of the experiment
#'
#' @param Y0 is the natural logarithm of the initial (time=0) bacterial concentration (N0)
#'
#' @param sigma is the time of first decimal reduction
#'
#' @param alpha which is a shape parameter
#'
#' @return A numeric vector with the fitted values
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Weibull Inactivation Mafart
#'
#' @references
#' \Sexpr[results=rd,stage=build]{Rdpack::insert_ref(key="Mafart2002",package="predmicror")}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(bixina)
#' initial_values <- list(Y0 = 5.75, sigma = 12.8, alpha = 2.4)
#' fit <- gsl_nls(lnN ~ WeibullM(Time, Y0, sigma, alpha),
#'   data = bixina,
#'   start = initial_values
#' )
#' summary(fit)
#'
#' plot(lnN ~ Time, data = bixina)
#' lines(bixina$Time, predict(fit), col = "blue")
#'
WeibullM <- function(x, Y0, sigma, alpha) {
  result <- Y0 - log(10) * (x / sigma)^alpha
  result
}
