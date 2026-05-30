#'  Weibull inactivation model Peleg and Huang
#'
#' \code{WeibullPH} inactivation model for microbial inactivation curve.
#' Returns the model parameters estimated according to data collected in microbial inactivation experiments.
#'
#' The model's inputs are:
#'
#' t: time, assuming time zero as the beginning of the experiment.
#'
#' Y(t): the natural logarithm of the bacterial concentration X(t) measured at time t.
#'
#' Users should make sure that the bacterial concentration input is entered in natural logarithm, Y(t) = ln(X(t)).
#'
#' The following parameters can be estimated using Weibull function:
#'
#' \code{t}: is heating time under a constant temperature
#'
#' \code{Y0}: is the initial (time=0) bacterial counts in natural logarithm of the initial bacterial counts;
#'
#' \code{k}: is the inactivation rate (ln units/time)
#'
#' \code{alpha}: is the shape parameter of the survival curve
#'
#' @param t is a numeric vector indicating the time of the experiment
#'
#' @param Y0 is the natural logarithm of the initial (time=0) bacterial concentration
#'
#' @param k is the inactivation rate (ln units/time)
#'
#' @param alpha is the shape parameter
#'
#' @return A numeric vector with the fitted values
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Weibull Inactivation
#'
#' @references
#' \Sexpr[results=rd,stage=build]{Rdpack::insert_ref(key="Huang2009Inac",package="predmicror")}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(bixina)
#' initial_values <- list(Y0 = 6.0, k = 1.0, alpha = 0.2)
#' fit <- gsl_nls(lnN ~ WeibullPH(Time, Y0, k, alpha),
#'   data = bixina,
#'   start = initial_values
#' )
#' summary(fit)
#'
#' plot(lnN ~ Time, data = bixina)
#' lines(bixina$Time, predict(fit), col = "blue")
#'
WeibullPH <- function(t, Y0, k, alpha) {
  result <- Y0 - log(10) * k * t^alpha
  result
}
