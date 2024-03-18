#' Buchanan reduced growth model
#'
#' \code{BuchananRM} function to fit the Buchanan reduced growth model to a reduced microbial growth curve.
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' Model's inputs are:
#'
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
#' @param MUmax is the maximum specific growth rate given in time units
#'
#' @param lag is the duration of the lag phase in time units
#'
#' @return An object of nls class
#'
#' @author Vasco Cadavez, \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron, \email{ubarron@ipb.pt}
#'
#' @keywords Reduced Buchanan
#'
#' @references
#' \insertRef{Buchanan1997}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' ## Example: Buchanan reduced model
#' library(gslnls)
#' data(growthred) # simulated data set.
#' initial_values <- list(Y0 = 0, MUmax = 1.7, lag = 5) # define the initial values
#' fit <- gsl_nls(lnN ~ BuchananRM(Time, Y0, MUmax, lag),
#'   data = growthred,
#'   start = initial_values
#' )
#' summary(fit)
#'
BuchananRM <- function(t, Y0, MUmax, lag) {
  if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }
  result <- ifelse(t <= lag, Y0, Y0 + MUmax * (t - lag))
  return(result)
}
