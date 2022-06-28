#' Cardinal model for growth inhibitors
#'
#' \code{CMInh} function to fit the growth inhibitors cardinal model (Rosso et al, 1993).
#' Returns the model parameters estimated according to data collected in microbial growth experiments.
#'
#' The model's inputs are:
#' 
#' \code{x}: growth inhibitor concentration
#' 
#' \code{sqrtGR}: the square root of the growth rate ($time^{-1}$)
#' 
#' Users should make sure that the growth rate input is entered after a square root transformation, $sqrGR = sqrt(GR)$.
#'
#' @param x is a numeric vector indicating the inhibitor concentration of the experiment
#' 
#' @param MIC is the minimum inhibitory concentration (mM or %, accordingly)
#' 
#' @param MUopt is the optimum growth rate
#' 
#' @param alpha is the shape parameter of the curve (alpha = 1 the shape is linear;
#'  alpha > 1 the shape is downward concave; and alpha < 1 the shape is upward concave)
#' 
#' @return An object of nls class with the fitted parameters of the model
#'
#' @author Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
#'
#' @keywords Cardinal Rosso Inhibitor
#'
#' @references 
#' \insertRef{Rosso1995}{predmicror}
#'
#' @importFrom gslnls gsl_nls
#'
#' @export
#'
#' @examples
#' library(gslnls)
#' data(inh)
#' initial_values = list(MIC=0.89, MUopt=1.0, alpha=1)
#' fit <- gsl_nls(sqrtGR ~ CMInh(Conce,MIC,MUopt,alpha),
#'                data=inh,
#'                start =  initial_values)
#' summary(fit)
#'
CMInh <- function(x, MIC, MUopt, alpha){
  
if (!requireNamespace("gslnls", quietly = TRUE)) {
    stop(
      "Package \"gslnls\" must be installed to use this function.",
      call. = FALSE
    )
  }

  CMinh <- ifelse (x > MIC, 0, MUopt*(1 - (x/MIC )^alpha) )
  result <- sqrt(CMinh)
  return(result)
}

 
 
