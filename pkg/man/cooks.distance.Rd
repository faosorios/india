\name{cooks.distance}
\alias{cooks.distance.lad}
\alias{cooks.distance.ols}
\alias{cooks.distance.ridge}
\title{Cook's distances}
\usage{
  \method{cooks.distance}{lad}(model, \dots)
  \method{cooks.distance}{ols}(model, \dots)
  \method{cooks.distance}{ridge}(model, type = "cov", \dots)
}
\arguments{
  \item{model}{ an \R object, returned by \code{\link{ols}}, \code{\link{lad}} or \code{\link{ridge}}.}
  \item{type}{ only required for \code{'ridge'} objects, options available are \code{"1st"}, 
    \code{"cov"} and \code{"both"} to obtain the Cook's distance based on Equation (2.5), (2.6) or 
    both by Walker and Birch (1988), respectively.}
  \item{\dots}{ further arguments passed to or from other methods.}
}
\description{
  Cook's distance is a measure to assess the influence of the \emph{i}th observation on the 
  model parameter estimates. This function computes the Cook's distance based on leave-one-out 
  cases deletion for ordinary least squares, lad and ridge regression.
}
\value{
  A vector whose \emph{i}th element contains the Cook's distance,
  \deqn{D_i(\bold{M},c) = \frac{(\hat{\bold{\beta}}_{(i)} - \hat{\bold{\beta}})^T\bold{M}
  (\hat{\bold{\beta}}_{(i)} - \hat{\bold{\beta}})}{c},}
  for \eqn{i = 1,\dots,n}, with \eqn{\bold{M}} a positive definite matrix and \eqn{c > 0}. Specific 
  choices of \eqn{\bold{M}} and \eqn{c} are done for objects of class \code{ols}, \code{lad} and 
  \code{ridge}.
}
\references{
  Cook, R.D., Weisberg, S. (1980).
  Characterizations of an empirical influence function for detecting influential cases in regression.
  \emph{Technometrics} \bold{22}, 495-508.
  \doi{10.1080/00401706.1980.10486199}

  Cook, R.D., Weisberg, S. (1982).
  \emph{Residuals and Influence in Regression}.
  Chapman and Hall, London.

  Sun, R.B., Wei, B.C. (2004).
  On influence assessment for LAD regression.
  \emph{Statistics & Probability Letters} \bold{67}, 97-110.
  \doi{10.1016/j.spl.2003.08.018}.

  Walker, E., Birch, J.B. (1988).
  Influence measures in ridge regression.
  \emph{Technometrics} \bold{30}, 221-227.
  \doi{10.1080/00401706.1988.10488370}
}
\examples{
# Cook's distances for linear regression
fm <- ols(stack.loss ~ ., data = stackloss)
CD <- cooks.distance(fm)
plot(CD, ylab = "Cook's distances", ylim = c(0,0.8))
text(21, CD[21], label = as.character(21), pos = 3)

# Cook's distances for LAD regression
fm <- lad(stack.loss ~ ., data = stackloss)
CD <- cooks.distance(fm)
plot(CD, ylab = "Cook's distances", ylim = c(0,0.4))
text(17, CD[17], label = as.character(17), pos = 3)

# Cook's distances for ridge regression
data(portland)
fm <- ridge(y ~ ., data = portland)
CD <- cooks.distance(fm)
plot(CD, ylab = "Cook's distances", ylim = c(0,0.5))
text(8, CD[8], label = as.character(8), pos = 3)
}
\keyword{regression}
