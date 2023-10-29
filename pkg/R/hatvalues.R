## ID: hatvalues.R, last updated 2023-10-25, F.Osorio

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
