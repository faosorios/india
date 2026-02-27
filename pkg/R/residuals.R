## ID: residuals.R, last updated 2026-02-18, F.Osorio

rquantile  <- function(model, ...) UseMethod("rquantile")

rquantile.lm <- function(model, ...)
{ ## Randomized quantile residuals for 'lm' objects
  if (!inherits(model, "lm"))
    stop("Use only with 'lm' objects")
  z <- model
  y <- model.response(z$model)
  r <- resid(z)
  mu <- fitted(z)
  n <- length(y)
  dispersion <- minkowski(r) / sqrt(n) 
  logp <- pnorm(y, mean = mu, sd = dispersion, log.p = TRUE)
  res <- qnorm(logp, log.p = TRUE)
  names(res) <- as.character(1:n)
  res
}

rquantile.ols <- function(model, ...)
{ ## Randomized quantile residuals for ordinary least squares
  if (!inherits(model, "ols"))
    stop("Use only with 'ols' objects")
  z <- model
  y <- model.response(z$model)
  r <- resid(z)
  mu <- fitted(z)
  n <- length(y)
  dispersion <- sqrt(z$RSS / n) 
  logp <- pnorm(y, mean = mu, sd = dispersion, log.p = TRUE)
  res <- qnorm(logp, log.p = TRUE)
  names(res) <- as.character(1:n)
  res
}

rquantile.lad <- function(model, ...)
{ ## Randomized quantile residuals for least absolute deviation
  ## regression. The original code was extracted from the L1pack 
  ## package.
  if (!inherits(model, "lad"))
    stop("Use only with 'lad' objects")
  z <- model
  y <- model.response(z$model)
  r <- resid(z)
  mu <- fitted(z)
  n <- length(y)
  dispersion <- z$scale
  logp <- plaplace(y, location = mu, scale = dispersion, log.p = TRUE)
  res <- qnorm(logp, log.p = TRUE)
  names(res) <- as.character(1:n)
  res
}

rquantile.nls <- function(model, ...)
{ ## Randomized quantile residuals for nonlinear least squares
  if (!inherits(model, "nls"))
    stop("Use only with 'nls' objects")
  y <- model$m$lhs()
  r <- resid(model)
  attr(r, "label") <- NULL
  mu <- fitted(model)
  attr(mu, "label") <- NULL
  cf <- coef(model)
  n <- length(y)
  p <- length(cf)
  dispersion <- minkowski(r) / sqrt(n - p)
  logp <- pnorm(y, mean = mu, sd = dispersion, log.p = TRUE)
  res <- qnorm(logp, log.p = TRUE)
  names(res) <- as.character(1:n)
  res
}

rquantile.ridge <- function(model, ...)
{ ## Randomized quantile residuals for ridge regression
  if (!inherits(model, "ridge"))
    stop("Use only with 'ridge' objects")
  z <- model
  y <- model.response(z$model, "numeric")
  r <- resid(z)
  mu <- fitted(z)
  n <- length(y)
  dispersion <- sqrt(z$scale) 
  logp <- pnorm(y, mean = mu, sd = dispersion, log.p = TRUE)
  res <- qnorm(logp, log.p = TRUE)
  names(res) <- as.character(1:n)
  res
}
