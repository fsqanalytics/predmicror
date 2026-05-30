#' Geeraerd inactivation model
#'
#' \code{GeeraerdST} inactivation model for microbial inactivation curve.
#' Returns the model parameters estimated according to data collected in microbial inactivation experiments.
#'
#' The model's inputs are:
#'
#' \code{t}: time, assuming time zero as the beginning of the experiment.
#'
#' \code{N(t)}: the bacterial concentration measured at time t.
#'
#' Users should make sure that the bacterial concentration input is entered
#'  in natural logarithm, \code{Y(t) = ln(N(t))}.
#'
#' @param x is a numeric vector indicating the heating time under a constant temperature of the experiment
#'
#' @param Y0 is the initial (time=0) bacterial concentration (ln(N0))
#'
#' @param Yres is a low asymptote reflecting the presence of a resistant sub-population (ln(Nres))
#'
#' @param kmax is the maximum inactivation rate
#'
#' @param Sl represents shoulder phase preceding the sharp inactivation slope of the curve
#'
#' @return A numeric vector with the fitted values
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Inactivation Geeraerd Tail Shoulder
#'
#' @references
#' \Sexpr[results=rd,stage=build]{Rdpack::insert_ref(key="Geeraerd2005",package="predmicror")}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(mafart2005Li11)
#' mafart2005Li11$lnN <- log(10) * mafart2005Li11$logN
#' initial_values <- list(Y0 = 18, Yres = 2, kmax = 0.7, Sl = 4)
#' fit <- gsl_nls(lnN ~ GeeraerdST(Time, Y0, Yres, kmax, Sl),
#'   data = mafart2005Li11,
#'   start = initial_values
#' )
#' summary(fit)
#'
#' plot(lnN ~ Time, data = mafart2005Li11)
#' lines(mafart2005Li11$Time, predict(fit), col = "blue")
#'
GeeraerdST <- function(x, Y0, Yres, kmax, Sl) {
  N0 <- exp(Y0)
  Nres <- exp(Yres)
  Nt <- (N0 - Nres) * exp(-kmax * x) * ((exp(kmax * Sl)) / (1 + (exp(kmax * Sl) - 1) * exp(-kmax * x))) + Nres
  result <- log(Nt)
  result
}
