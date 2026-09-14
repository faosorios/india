## ID: hatvalues.R, last updated 2026-09-13, F.Osorio

hatvalues.lad <- function(model, ...)
{ ## leverages for lad regression
  hats <- leverages.lad(model)
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
