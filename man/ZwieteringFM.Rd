% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zwietering_full.R
\name{ZwieteringFM}
\alias{ZwieteringFM}
\title{Zwietering full growth model}
\usage{
ZwieteringFM(t, Y0, Ymax, MUmax, lag)
}
\arguments{
\item{t}{is a numeric vector indicating the time of the experiment}

\item{Y0}{is the natural logarithm of the initial microbial concentration (\code{ln(N0)}) at time=0}

\item{Ymax}{is the natural logarithm of the maximum concentration (\code{ln(Nmax)}) reached by the microorganism}

\item{MUmax}{is the maximum specific growth rate given in time units}

\item{lag}{is the duration of the lag phase in time units}
}
\value{
An object of nls class
}
\description{
\code{ZwieteringFM} function to fit the Zwietering full growth model to a complete microbial growth curve.
Returns the model parameters estimated according to data collected in microbial growth experiments.
}
\details{
Model's inputs are:

\code{t}: time, assuming time zero as the beginning of the experiment.

\code{Y(t)}: the natural logarithm of the microbial concentration (\code{ln(N(t)}) measured at time t.

Users should make sure that the microbial concentration input is entered in natural logarithm, \code{Y(t) = ln(X(t))}.
}
\examples{
## Example: Zwietering full model
library(gslnls)
data(growthfull) # simulated data set.
initial_values <- list(Y0 = 0, Ymax = 22, MUmax = 1.7, lag = 5) # define the initial values
fit <- gsl_nls(lnN ~ ZwieteringFM(Time, Y0, Ymax, MUmax, lag),
  data = growthfull,
  start = initial_values
)
summary(fit)

}
\references{
\insertRef{Zwietering1990}{predmicror}
}
\author{
Vasco Cadavez \email{vcadavez@ipb.pt} and Ursula Gonzales-Barron \email{ubarron@ipb.pt}
}
\keyword{Zwietering}
\keyword{fullmodel}
