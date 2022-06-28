#' Weibull inactivation modified model Mafart
#'
#' \code{WeibullMM} inactivation model for microbial inactivation curve.
#' Returns the model parameters estimated according to data collected in microbial inactivation experiments.
#'
#' The model's inputs are:
#'
#' \code{t}: time, assuming time zero as the beginning of the experiment.
#' 
#' \code{Y(t)}: the bacterial concentration ($Y(t)$) measured at time t.
#' 
#' Users should make sure to use the base 10 logarithm bacterial concentration (Y(t)) as input.
#' 
#' @param x is a numeric vector indicating the heating time under a constant temperature of the experiment
#' 
#' @param Y0 is the log10 of the initial bacterial concentration (at time t=0)
#' 
#' @param Yres is the log10 of the residual bacterial concentration (at the end of the experiment)
#' 
#' @param sigma represents the time of the first decimal reduction concentration
#'  for the part of the population not belonging to Yres
#' 
#' @param alpha is the shape parameter and allows to catch the curve concavity or convexity
#' 
#' @return An object of nls class with the fitted parameters of the model
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Weibull Inactivation Modified Mafart
#'
#' @references 
#' \insertRef{Mafart2005}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(bixina)
#'
WeibullMM <- function(x, Y0, Yres, sigma, alpha){

if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
         "Package \"gslnls\" must be installed to use this function.",
         call. = FALSE
    )
  }
result <- log10((10^Y0-10^Yres)*10^(-(x/sigma)^alpha)+10^Yres)
  return(result)
}
