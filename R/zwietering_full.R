#' Zwietering full growth model
#'
#' \code{ZwieteringFM} function to fit the Zwietering full growth model to a complete microbial growth curve.
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' Model's inputs are:
#'
#' \code{t}: time, assuming time zero as the beginning of the experiment.
#'
#' \code{Y(t)}: the natural logarithm of the microbial concentration (\code{ln(N(t)}) measured at time t.
#'
#' Users should make sure that the microbial concentration input is entered in natural logarithm, \code{Y(t) = ln(X(t))}.
#'
#' @param t is a numeric vector indicating the time of the experiment
#'
#' @param Y0 is the natural logarithm of the initial microbial concentration (`ln(N0)`) at time=0
#'
#' @param Ymax is the natural logarithm of the maximum concentration (`ln(Nmax)`) reached by the microorganism
#'
#' @param MUmax is the maximum specific growth rate given in time units
#'
#' @param lag is the duration of the lag phase in time units
#'
#' @return A numeric vector with the fitted values
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords fullmodel Zwietering
#'
#' @references
#' \insertRef{Zwietering1990}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' ## Example: Zwietering full model
#' library(gslnls)
#' data(growthfull) # simulated data set.
#' initial_values <- list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5) # define the initial values
#' fit <- gsl_nls(lnN ~ ZwieteringFM(Time, Y0, Ymax, MUmax, lag),
#'   data = growthfull,
#'   start = initial_values
#' )
#' summary(fit)
#'
ZwieteringFM <- function(t, Y0, Ymax, MUmax, lag) {
  result <- Y0 + (Ymax - Y0) *
    exp(-exp((MUmax * exp(1) * (lag - t)) / (Ymax - Y0) + 1))
  result
}
