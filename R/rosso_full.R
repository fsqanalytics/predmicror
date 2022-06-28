#' Rosso full growth model
#'
#' \code{RossoFM} function to fit the Rosso full growth model to complete microbial growth curve.
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' Model's inputs are:s
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
#' @return An object of nls class
#'
#' @author Vasco Cadavez (\email{vcadavez@ipb.pt}) and Ursula Gonzales-Barron (\email{ubarron@ipb.pt})
#' @keywords full models
#'
#' @references 
#' \insertRef{Rosso1996}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' ## Example: Rosso full model
#' library(gslnls)
#' data(growthfull)  # simulated data set.
#' initial_values = list(Y0=0.04, Ymax=21, MUmax=1.9, lag=5.0) # define the initial values
#' fit <- gsl_nls(lnN ~ RossoFM(Time, Y0, Ymax, MUmax, lag),
#'            data=growthfull,
#'            start =  initial_values)
#' summary(fit)
#'
RossoFM <- function(t, Y0, MUmax, Ymax, lag){

  if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
    result <- ifelse( t <= lag, Y0, Ymax-log(1+(exp(Ymax)/exp(Y0)-1)*exp(-MUmax*(t-lag))) )
  return(result)
}
