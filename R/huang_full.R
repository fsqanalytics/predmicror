#' Huang full growth model
#'
#' \code{HuangFM} function to fit the Huang full growth model to complete microbial growth curve.
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
#' @return An object of nls class
#'
#' @author Vasco Cadavez, \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron, \email{ubarron@ipb.pt}
#'
#' @keywords fullmodel Huang
#'
#' @references 
#' \insertRef{Huang2008}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#' @importFrom Rdpack reprompt
#' 
#' @export
#'
#' @examples
#' ## Example: Huang full model
#' library(gslnls)
#' data(growthfull)  # simulated data set.
#' initial_values = list(Y0=0, Ymax=22, MUmax=1.7, lag=5) # define the initial values
#' ## Call the fitting function
#' fit <- gsl_nls(lnN ~ HuangFM(Time, Y0, Ymax, MUmax, lag),
#'            data=growthfull,
#'            start =  initial_values)
#' summary(fit)
#'
#' confint(fit)
#'
#' preds <- data.frame(predict(fit, interval = "prediction", level = 0.95))
#' plot(lnN ~ Time, data=growthfull, ylim=c(-1,22))
#' lines(growthfull$Time, preds$fit, col="blue")
#' lines(growthfull$Time, preds$upr, col="red")
#' lines(growthfull$Time, preds$lwr, col="red")
#'
HuangFM <- function(t, Y0, Ymax, MUmax, lag){

  if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  Beta <- t + 1/4*log( (1 + exp(-4*(t-lag)))/(1 + exp(4*lag)) )
  result <- Y0 + Ymax - log(exp(Y0) + (exp(Ymax) - exp(Y0))*exp(-MUmax*Beta))
  return(result)
}
