#'  Huang reparameterized Gompertz survival model
#'
#' \code{HuangRGS} reparametrized Gompertz survival model for microbial inactivation.
#' Returns the model parameters estimated according to data collected in microbial inactivation experiments.
#'
#' The model's inputs are:
#'
#' \code{t}: time, assuming time zero as the beginning of the experiment.
#'
#' \code{Y(t)}: the natural logarithm of the bacterial concentration (\code{ln(X(t))}) measured at time t.
#'
#' Users should make sure that the bacterial concentration input is entered
#'  in natural logarithm, \code{Y(t) = ln(X(t))}.
#'
#' @param x is a numeric vector indicating the heating time under a constant temperature of the experiment
#'
#' @param Y0 is the initial microbial concentration (ln(cfu 1/g))
#'
#' @param k is the inactivation rate (1/s)
#'
#' @param M is a time constant (s)
#'
#' @return A numeric vector with the fitted values
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Huang Gompertz Survival Inactivation
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
#' initial_values <- list(Y0 = 5.6, k = 0.37, M = 6.8)
#' fit <- gsl_nls(lnN ~ HuangRGS(Time, Y0, k, M),
#'   data = bixina,
#'   start = initial_values
#' )
#' summary(fit)
#'
#' plot(lnN ~ Time, data = bixina)
#' lines(bixina$Time, predict(fit), col = "blue")
#'
HuangRGS <- function(x, Y0, k, M) {
  result <- Y0 * (1 - exp(-exp(-k * (x - M))))
  result
}
