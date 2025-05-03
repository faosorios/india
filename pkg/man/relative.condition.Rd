\name{relative.condition}
\alias{relative.condition}
\title{Relative change in the condition number}
\usage{
relative.condition(x)
}
\arguments{
  \item{x}{ the model matrix \eqn{\bold{X}}.}
}
\description{
  Compute the relative condition index to identify collinearity-influential points in linear models.
}
\value{
  To assess the influence of the \emph{i}th row of \eqn{\bold{X}} on the condition index of \eqn{\bold{X}},
  Hadi (1988) proposed the relative change,
  \deqn{\delta_i = \frac{\kappa_{(i)} - \kappa}{\kappa},}
  for \eqn{i=1,\dots,n}, where \eqn{\kappa = \kappa(\bold{X})} and \eqn{\kappa_{(i)} = \kappa(\bold{X}_{(i)})} 
  denote the (scaled) condition index for \eqn{\bold{X}} and \eqn{\bold{X}_{(i)}}, respectively.
}
\references{
  Chatterjee, S., Hadi, A.S. (1988).
  \emph{Sensivity Analysis in Linear Regression}.
  Wiley, New York.

  Hadi, A.S. (1988).
  Diagnosing collinerity-influential observations.
  \emph{Computational Statistics & Data Analysis} \bold{7}, 143-159. 
  \doi{10.1016/0167-9473(88)90089-8}.
}
\examples{
data(portland)
fm <- ridge(y ~ ., data = portland, x = TRUE)
x <- fm$x
rel <- relative.condition(x)
plot(rel, ylab = "Relative condition number", ylim = c(-0.1,0.4))
abline(h = 0, lty = 2, lwd = 2, col = "red")
text(3, rel[3], label = as.character(3), pos = 3)
}
\keyword{regression}
