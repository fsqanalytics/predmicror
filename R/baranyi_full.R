#' Baranyi and Roberts full growth model
#'
#' \code{BaranyiFM} function to fit the Baranyi & Roberts full growth model to a complete microbial growth curve.
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' The model's inputs are:
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
#' @param lag is the duration of the lag phase in time units
#'
#' @return An object of nls class with the fitted parameters of the model
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords full model Baranyi Roberts
#'
#' @references
#' \insertRef{Baranyi1994}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(growthfull)
#' initial_values = list(Y0=-0.1, Ymax=22, MUmax=1.7, lag=5)
#' fit <- gsl_nls(lnN ~ BaranyiFM(Time, Y0, Ymax, MUmax, lag),
#'                data=growthfull,
#'                start =  initial_values)
#' summary(fit)
#'
BaranyiFM <- function(t, Y0, Ymax, MUmax, lag){

if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }
result <- Y0 + MUmax*(t + 1/MUmax*log(exp(-MUmax*t)+exp(-MUmax*lag)-exp(-MUmax*(t+lag)) )) - log( 1 + (exp(MUmax*(t + 1/MUmax*log(exp(-MUmax*t)+exp(-MUmax*lag)-exp(-MUmax*(t+lag)) ))) -1)/exp(Ymax-Y0) )
return(result)
}
