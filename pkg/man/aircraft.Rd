\name{aircraft}
\docType{data}
\alias{aircraft}

\title{Aircraft data}
\description{
  This dataset is presented in Rousseeuw and Leroy (1987, pp. 154), and the aim is to model the 
  cost of 23 single-engine aircraft (in unit of $100,000) as a function of the following explanatory 
  variables: aspect ratio, lift-to-drag ratio, weight of the plane, in pounds, and maximal thrust.
}
\usage{data(aircraft)}
\format{
  A data frame with 23 observations on the following 5 variables.
  \describe{
    \item{aspect}{ aspect ratio.}
    \item{lift2drag}{ lift-to-drag ratio.}
    \item{weight}{ weight of the plane (in pounds).}
    \item{thrust}{ maximal thrust.}
    \item{cost}{ cost of 23 single-engine aircraft (in unit of $100,000).}
  }
}
\source{
  Rousseeuw, P.J., Leroy, A.M. (1987).
  \emph{Robust Regression and Outlier Detection}.
  Wiley, New York.
}
\keyword{datasets}