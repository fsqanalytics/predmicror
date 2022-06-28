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
#'  in base 10 logarithm, \code{Y(t) = log10(N(t))}.
#' 
#' @param x is a numeric vector indicating the heating time under a constant temperature of the experiment
#' 
#' @param Y0 is the initial (time=0) bacterial concentration (log10(N0))
#' 
#' @param Yres is a low asymptote reflecting the presence of a resistant sub-population (log10(Nres))
#' 
#' @param kmax is the maximum inactivation rate
#' 
#' @param Sl represents shoulder phase preceding the sharp inactivation slope of the curve
#' 
#' @return An object of nls class with the fitted parameters of the model
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Inactivation Geeraerd Tail Shoulder
#'
#' @references 
#' \insertRef{Geeraerd2005}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(mafart2005Li11)
#' initial_values = list(Y0=10, Yres=7, kmax=0.7, Sl=4)
#' fit <- gsl_nls(logN ~  GeeraerdST(Time,Y0,Yres,kmax,Sl),
#'                data=mafart2005Li11,
#'                start =  initial_values)
#' summary(fit)
#' 
#' plot(logN ~ Time, data=mafart2005Li11)
#' lines(mafart2005Li11$Time, predict(fit), col="blue")
#'
GeeraerdST <- function(x,Y0,Yres,kmax,Sl){

if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
         "Package \"gslnls\" must be installed to use this function.",
         call. = FALSE
    )
  }
result <- log10( (10^Y0 - 10^Yres)*exp(-kmax*x)*( (exp(kmax*Sl))/(1+(exp(kmax*Sl)-1)*exp(-kmax*x)) ) + 10^Yres )
  return(result)
}
