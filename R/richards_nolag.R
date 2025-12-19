#' Richards no lag growth model
#'
#' \code{RichardsNLM} function to fit the Richards no lag growth model to an incomplete microbial growth curve.
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' Model's inputs are:
#'
#' \code{t}: time, assuming time zero as the beginning of the experiment.
#'
#' \code{Y(t)}: the natural logarithm of the microbial concentration (\code{ln(N(t)}) measured at time t.
#'
#' Users should make sure that the microbial concentration input is entered in natural logarithm, \code{Y(t) = ln(N(t))}.
#'
#' @param t is a numeric vector indicating the time of the experiment
#'
#' @param Y0 is the natural logarithm of the initial microbial concentration (`ln(N0)`) at time=0
#'
#' @param Ymax is the natural logarithm of the maximum concentration (`ln(Nmax)`) reached by the microorganism
#'
#' @param MUmax is the maximum specific growth rate given in time units
#'
#' @param m is the shape parameter of the Richards model (default = 1)
#'
#' @return A numeric vector with the fitted values
#'
#' @author Vasco Cadavez, \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron, \email{ubarron@ipb.pt}
#'
#' @keywords nolagmodel Growth Richards
#'
#' @references
#' \insertRef{Richards1959}{predmicror}
#'
#' @export
#'
#' @examples
#' ## Example: Richards no lag model
#' library(gslnls)
#' data(growthnolag) # simulated data set.
#' initial_values <- list(Y0 = 0, Ymax = 22, MUmax = 1.7)
#' ## Fitting the function to the experimental data
#' fit <- gsl_nls(lnN ~ RichardsNLM(Time, Y0, Ymax, MUmax),
#'   data = growthnolag,
#'   start = initial_values
#' )
#' summary(fit)
#'
RichardsNLM <- function(t, Y0, Ymax, MUmax, m = 1) {
  result <- Y0 + MUmax * t -
    (1 / m) * log1p(expm1(m * MUmax * t) * exp(-m * (Ymax - Y0)))
  result
}
