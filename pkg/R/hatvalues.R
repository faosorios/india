## ID: hatvalues.R, last updated 2026-04-04, F.Osorio

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
