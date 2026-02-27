\name{rquantile}
\alias{rquantile}
\alias{rquantile.lm}
\alias{rquantile.lad}
\alias{rquantile.ols}
\alias{rquantile.nls}
\alias{rquantile.ridge}
\title{Randomized quantile residuals}
\usage{
rquantile(model, \dots)
\method{rquantile}{lm}(model, \dots)
\method{rquantile}{lad}(model, \dots)
\method{rquantile}{ols}(model, \dots)
\method{rquantile}{nls}(model, \dots)
\method{rquantile}{ridge}(model, \dots)
}
\arguments{
  \item{model}{ an \R object, returned by \code{\link{lm}}, \code{\link[L1pack:lad]{lad}}, \code{\link[fastmatrix:ols]{ols}}, 
    \code{\link{nls}} or \code{\link[fastmatrix:ridge]{ridge}}.}
  \item{\dots}{ further arguments passed to or from other methods.}
}
\description{
  Compute randomized quantile residuals from a fitted model object.
}
\value{
  a vector containing standard normal deviates representing standardized residuals. This kind of 
  residuals are exactly normal.
}
\references{
  Dunn, P.K., Smyth, G.K. (1996). 
  Randomized quantile residuals. 
  \emph{Journal of Computational and Graphical Statistics} \bold{5}, 236-244. 
  \doi{10.1080/10618600.1996.10474708}

  Osorio, F. (2026).
  On the mean-shift outlier model for LAD regression.
  Working paper.
}
\examples{
# quantile residuals for linear regression
fm <- ols(stack.loss ~ ., data = stackloss)
res <- rquantile(fm)
plot(res, ylim = c(-3,3), ylab = "quantile residuals")
abline(h = 0, lwd = 2, col = "gray75")
abline(h = c(-2,2), lwd = 2, lty = 2, col = "red")
text(21, res[21], as.character(21), pos = 1)

# quantile residuals for LAD regression
data(ereturns)
fm <- lad(m.marietta ~ CRSP, data = ereturns)
res <- rquantile(fm)
plot(res, ylim = c(-2,4.5), ylab = "quantile residuals")
abline(h = 0, lwd = 2, col = "gray75")
abline(h = c(-2,2), lwd = 2, lty = 2, col = "red")
obs <- c(8,15,34)
text(obs, res[obs], as.character(obs), pos = 3)

# quantile residuals for ridge regression
data(portland)
fm <- ridge(y ~ ., data = portland)
res <- rquantile(fm)
plot(res, ylim = c(-2,2), ylab = "quantile residuals")
abline(h = 0, lwd = 2, col = "gray75")

# quantile residuals for nonlinear regression
data(skeena)
model <- recruits ~ b1 * spawners * exp(-b2 * spawners)
fm <- nls(model, data = skeena, start = list(b1 = 3, b2 = 0))
res <- rquantile(fm)
plot(res, ylim = c(-3,3), ylab = "quantile residuals")
abline(h = 0, lwd = 2, col = "gray75")
abline(h = c(-2,2), lwd = 2, lty = 2, col = "red")
text(5, res[5], as.character(5), pos = 3)
}
\keyword{regression}
