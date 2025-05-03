\name{leverages}
\alias{leverages}
\alias{hatvalues.nls}
\alias{hatvalues.ols}
\alias{hatvalues.ridge}
\alias{leverages.lm}
\alias{leverages.nls}
\alias{leverages.ols}
\alias{leverages.ridge}
\title{Leverages}
\usage{
leverages(model, \dots)
\method{leverages}{lm}(model, infl = lm.influence(model, do.coef = FALSE), \dots)
\method{leverages}{nls}(model, \dots)
\method{leverages}{ols}(model, \dots)
\method{leverages}{ridge}(model, \dots)

\method{hatvalues}{nls}(model, \dots)
\method{hatvalues}{ols}(model, \dots)
\method{hatvalues}{ridge}(model, \dots)
}
\arguments{
  \item{model}{ an \R object, returned by \code{\link{lm}}, \code{\link{nls}}, \code{ols} or \code{ridge}.}
  \item{infl}{ influence structure as returned by \code{\link{lm.influence}}.}
  \item{\dots}{ further arguments passed to or from other methods.}
}
\description{
  Computes leverage measures from a fitted model object.
}
\value{
  A vector containing the diagonal of the prediction (or \sQuote{hat}) matrix. 

  For linear regression (i.e., for \code{"lm"} or \code{"ols"} objects) the prediction matrix assumes 
  the form 
  \deqn{\bold{H} = \bold{X}(\bold{X}^T\bold{X})^{-1}\bold{X}^T,}
  in which case, \eqn{h_{ii} = \bold{x}_i^T(\bold{X}^T\bold{X})^{-1}\bold{x}_i} for \eqn{i=1,\dots,n}. Whereas
  for ridge regression, the prediction matrix is given by 
  \deqn{\bold{H}(\lambda) = \bold{X}(\bold{X}^T\bold{X} + \lambda\bold{I})^{-1}\bold{X}^T,}
  where \eqn{\lambda} represents the ridge parameter. Thus, the diagonal elements of \eqn{\bold{H}(\lambda)},
  are \eqn{h_{ii}(\lambda) = \bold{x}_i^T(\bold{X}^T\bold{X} + \lambda\bm{I})^{-1}\bold{x}_i}, \eqn{i=1,\dots,n}.

  In nonlinear regression, the tangent plane leverage matrix is given by 
  \deqn{\hat{\bold{H}} = \hat{\bold{F}}(\hat{\bold{F}}^T\hat{\bold{F}})^{-1}\hat{\bold{F}}^T,}
  where \eqn{\bold{F} = \bold{F}(\bold{\beta})} is the \eqn{n\times p} local model matrix with \emph{i}th 
  row \eqn{\partial f_i(\bold{\beta})/\partial\bold{\beta}} and \eqn{\hat{\bold{F}} = \bold{F}(\hat{\bold{\beta}})}.
}
\note{
  This function never creates the prediction matrix and only obtains its diagonal elements from 
  the singular value decomposition of \eqn{\bold{X}} or \eqn{\hat{\bold{F}}}.

  Function \code{\link{hatvalues}} only is a wrapper for function \code{leverages}.
}
\references{
  Chatterjee, S., Hadi, A.S. (1988).
  \emph{Sensivity Analysis in Linear Regression}.
  Wiley, New York.

  Cook, R.D., Weisberg, S. (1982).
  \emph{Residuals and Influence in Regression}.
  Chapman and Hall, London.

  Ross, W.H. (1987).
  The geometry of case deletion and the assessment of influence in nonlinear regression.
  \emph{The Canadian Journal of Statistics} \bold{15}, 91-103.
  \doi{10.2307/3315198}

  St. Laurent, R.T., Cook, R.D. (1992).
  Leverage and superleverage in nonlinear regression.
  \emph{Journal of the Amercian Statistical Association} \bold{87}, 985-990.
  \doi{10.1080/01621459.1992.10476253}

  Walker, E., Birch, J.B. (1988).
  Influence measures in ridge regression.
  \emph{Technometrics} \bold{30}, 221-227.
  \doi{10.1080/00401706.1988.10488370}
}
\examples{
# Leverages for linear regression
fm <- ols(stack.loss ~ ., data = stackloss)
lev <- leverages(fm)
cutoff <- 2 * mean(lev)
plot(lev, ylab = "Leverages", ylim = c(0,0.45))
abline(h = cutoff, lty = 2, lwd = 2, col = "red")
text(17, lev[17], label = as.character(17), pos = 3)

# Leverages for ridge regression
data(portland)
fm <- ridge(y ~ ., data = portland)
lev <- leverages(fm)
cutoff <- 2 * mean(lev)
plot(lev, ylab = "Leverages", ylim = c(0,0.7))
abline(h = cutoff, lty = 2, lwd = 2, col = "red")
text(10, lev[10], label = as.character(10), pos = 3)

# Leverages for nonlinear regression
data(skeena)
model <- recruits ~ b1 * spawners * exp(-b2 * spawners)
fm <- nls(model, data = skeena, start = list(b1 = 3, b2 = 0))
lev <- leverages(fm)
plot(lev, ylab = "Leverages", ylim = c(0,0.25))
obs <- c(1,9)
text(obs, lev[obs], label = as.character(obs), pos = 3)
}
\keyword{regression}
