#' Huang no lag growth model
#'
#' \code{HuangNLM} function to fit the Huang no lag growth model to an incomplete microbial growth curve.
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
#' @return An object of nls class
#'
#' @author Vasco Cadavez, \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron, \email{ubarron@ipb.pt}
#'
#' @keywords nolagmodel Growth Huang
#'
#' @references
#' \insertRef{Huang2013}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' ## Example: Fang no lag model
#' library(gslnls)
#' data(growthnolag) # simulated data set.
#' initial_values <- list(Y0 = 0, Ymax = 22, MUmax = 1.7) # define the initial values
#' ## Call the fitting function
#' fit <- gsl_nls(lnN ~ HuangNLM(Time, Y0, Ymax, MUmax),
#'   data = growthnolag,
#'   start = initial_values
#' )
#' summary(fit)
#'
HuangNLM <- function(t, Y0, Ymax, MUmax) {
  if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }

  result <- Y0 - log(exp(Y0 - Ymax) + (1 - exp(Y0 - Ymax)) * exp(-MUmax * t))
  return(result)
}
