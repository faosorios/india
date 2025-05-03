## ID: hatvalues.R, last updated 2025-05-01, F.Osorio

hatvalues.nls <- function(model, ...)
{ ## leverages for nonlinear regression
  hats <- leverages.nls(model)
  hats
}

hatvalues.ols <- function(model, ...)
{ ## leverages for ordinary least squares
  hats <- leverages.ols(model)
  hats
}

hatvalues.ridge <- function(model, ...)
{ ## leverages for ridge regression
  hats <- leverages.ridge(model)
  hats
}
