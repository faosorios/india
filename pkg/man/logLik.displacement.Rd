\name{logLik.displacement}
\alias{logLik.displacement}
\alias{logLik.displacement.lm}
\alias{logLik.displacement.ols}
\alias{logLik.displacement.lad}
\alias{logLik.displacement.ridge}
\title{Likelihood Displacement}
\usage{
  logLik.displacement(model, \dots)
  \method{logLik.displacement}{lm}(model, pars = "full", \dots)
  \method{logLik.displacement}{ols}(model, pars = "full", \dots)
  \method{logLik.displacement}{lad}(model, method = "quasi", pars = "full", \dots)
  \method{logLik.displacement}{ridge}(model, pars = "full", \dots)
}
\arguments{
  \item{model}{ an \R object, returned by \code{\link{lm}}, \code{\link{ols}}, \code{\link{lad}} 
    or \code{\link{ridge}}.}
  \item{pars}{ should be considered the whole vector of parameters (\code{pars = "full"}), 
    or only the vector of coefficients (\code{pars = "coef"}).}
  \item{method}{ only required for \code{'lad'} objects, options available are \code{"quasi"} 
    and \code{"BR"} to obtain the likelihood displacement based on Sun and Wei (2004) and Elian 
    et al. (2000) approaches, respectively.}
  \item{\dots}{ further arguments passed to or from other methods.}
}
\description{
  Compute the likelihood displacement influence measure based on leave-one-out cases deletion 
  for linear models, lad and ridge regression.
}
\value{
  A vector whose \emph{i}th element contains the distance between the likelihood functions,
  \deqn{LD_i(\bold{\beta},\sigma^2) = 2\{l(\hat{\bold{\beta}},\hat{\sigma}^2) - 
  l(\hat{\bold{\beta}}_{(i)},\hat{\sigma}^2_{(i)})\},}
  for \code{pars = "full"}, where \eqn{\hat{\bold{\beta}}_{(i)}} and \eqn{\hat{\sigma}^2_{(i)}} 
  denote the estimates of \eqn{\bold{\beta}} and \eqn{\sigma^2} when the \emph{i}th observation is 
  removed from the dataset. If we are interested only in \eqn{\bold{\beta}} (i.e. \code{pars = "coef"})
  the likelihood displacement becomes
  \deqn{LD_i(\bold{\beta}|\sigma^2) = 2\{l(\hat{\bold{\beta}},\hat{\sigma}^2) - 
  \max_{\sigma^2} l(\hat{\bold{\beta}}_{(i)},\hat{\sigma}^2)\}.}
}
\references{
  Cook, R.D., Weisberg, S. (1982).
  \emph{Residuals and Influence in Regression}.
  Chapman and Hall, London.

  Cook, R.D., Pena, D., Weisberg, S. (1988).
  The likelihood displacement: A unifying principle for influence measures.
  \emph{Communications in Statistics - Theory and Methods} \bold{17}, 623-640.
  \doi{10.1080/03610928808829645}.

  Elian, S.N., Andre, C.D.S., Narula, S.C. (2000).
  Influence measure for the L1 regression.
  \emph{Communications in Statistics - Theory and Methods} \bold{29}, 837-849.
  \doi{10.1080/03610920008832518}.

  Sun, R.B., Wei, B.C. (2004).
  On influence assessment for LAD regression.
  \emph{Statistics & Probability Letters} \bold{67}, 97-110.
  \doi{10.1016/j.spl.2003.08.018}.
}
\examples{
# Likelihood displacement for linear regression
fm <- ols(stack.loss ~ ., data = stackloss)
LD <- logLik.displacement(fm)
plot(LD, ylab = "Likelihood displacement", ylim = c(0,9))
text(21, LD[21], label = as.character(21), pos = 3)

# Likelihood displacement for LAD regression
fm <- lad(stack.loss ~ ., data = stackloss)
LD <- logLik.displacement(fm)
plot(LD, ylab = "Likelihood displacement", ylim = c(0,1.5))
text(17, LD[17], label = as.character(17), pos = 3)

# Likelihood displacement for ridge regression
data(portland)
fm <- ridge(y ~ ., data = portland)
LD <- logLik.displacement(fm)
plot(LD, ylab = "Likelihood displacement", ylim = c(0,4))
text(8, LD[8], label = as.character(8), pos = 3)
}
\keyword{regression}
