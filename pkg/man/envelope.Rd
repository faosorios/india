\name{envelope}
\alias{envelope}
\alias{envelope.lm}
\alias{envelope.lad}
\alias{envelope.ols}
\alias{envelope.nls}
\alias{envelope.ridge}
\title{QQ-plot of residuals with simulated envelope}
\usage{
envelope(object, \dots)
\method{envelope}{lm}(object, reps = 50, conf = 0.95, 
  type = c("quantile", "standard", "student"), plot.it = TRUE, \dots)
\method{envelope}{lad}(object, reps = 50, conf = 0.95, plot.it = TRUE, \dots)
\method{envelope}{ols}(object, reps = 50, conf = 0.95, 
  type = c("quantile", "standard", "student"), plot.it = TRUE, \dots)
\method{envelope}{nls}(object, reps = 50, conf = 0.95, plot.it = TRUE, \dots)
\method{envelope}{ridge}(object, reps = 50, conf = 0.95, plot.it = TRUE, \dots)
}
\arguments{
  \item{object}{ an \R object, returned by \code{\link{lm}}, \code{\link[L1pack:lad]{lad}}, \code{\link[fastmatrix:ols]{ols}}, 
    \code{\link{nls}} or \code{\link[fastmatrix:ridge]{ridge}}.}
  \item{reps}{ number of simulated point patterns to be generated when computing the envelopes. 
    The default number is 50, a larger number of replications will produce a smoother band, although 
    it takes more time.}
  \item{conf}{ the confidence level required for the construction of the envelope. The default is 
    to find 95\% confidence envelopes.}
  \item{type}{ a character string indicating the type of residuals that should be used in the 
    construction of the envelope. The available options are randomized quantile (\code{"quantile"}),
    standardized (\code{"standard"}) and studentized (\code{"student"}) residuals. Standardized and 
    studentized residuals are only available for objects of class \code{"lm"} and \code{"ols"}; otherwise, 
    quantile residuals are used.}
  \item{plot.it}{ if \code{TRUE} it will draw the corresponding plot, if \code{FALSE} it will only 
    return the computed values.}
  \item{\dots}{ further arguments passed to or from other methods.}
}
\description{
  Constructs a normal QQ-plot with simulated envelope of residuals from a fitted model object.
}
\value{
  a list containing the following elements:

  \item{residuals}{ a vector with the selected (see \code{type} argument) residuals.}
  \item{envelope}{ a matrix with two columns corresponding to the values of the lower and upper pointwise 
    confidence envelope.}
}
\references{
  Atkinson, A.C. (1985).
  \emph{Plots, Transformations and Regression}.
  Oxford University Press, Oxford.

  Osorio, F. (2026).
  On the mean-shift outlier model for LAD regression.
  Working paper.

  Venables, W.N., Ripley, B.D. (1999).
  \emph{Modern Applied Statistics with S-PLUS, 3rd Ed}.
  Springer, New York.
}
\examples{
# QQ-plot with envelope for linear regression
fm <- ols(stack.loss ~ ., data = stackloss)
z <- envelope(fm, reps = 500)

# QQ-plot with envelope for LAD regression
data(ereturns)
fm <- lad(m.marietta ~ CRSP, data = ereturns)
z <- envelope(fm, reps = 500)

# QQ-plot with envelope for ridge regression
data(portland)
fm <- ridge(y ~ ., data = portland)
z <- envelope(fm, reps = 500)

# QQ-plot with envelope nonlinear regression
data(skeena)
model <- recruits ~ b1 * spawners * exp(-b2 * spawners)
fm <- nls(model, data = skeena, start = list(b1 = 3, b2 = 0))
z <- envelope(fm, reps = 500)
}
\keyword{regression}
