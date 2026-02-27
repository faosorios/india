## ID: leverages.R, last updated 2025-05-18, F.Osorio

leverages <- function(model, ...) ## leverages (AKA "hatvalues")
UseMethod("leverages")

leverages.lm <- function(model, infl = lm.influence(model, do.coef=FALSE), ...) ## for completeness only
infl$hat

leverages.nls <- function(model, ...)
{ ## leverages for nonlinear regression
  if (!inherits(model, "nls"))
    stop("Use only with 'nls' objects")
  obj <- model
  F <- obj$m$gradient()
  n <- nrow(F)
  p <- ncol(F)

  rs <- svd(F, nv = 0)
  levs <- rowSums(rs$u^2)
  names(levs) <- as.character(1:n)
  levs
}

leverages.ols <- function(model, ...)
{ ## leverages for ordinary least squares
  if (!inherits(model, "ols"))
    stop("Use only with 'ols' objects")
  obj <- model
  x <- model.matrix(obj$terms, obj$model, obj$contrast)
  n <- nrow(x)
  p <- ncol(x)

  rs <- svd(x, nv = 0)
  levs <- rowSums(rs$u^2)
  names(levs) <- as.character(1:n)
  levs
}

leverages.ridge <- function(model, ...)
{ ## leverages for ridge regression
  if (!inherits(model, "ridge"))
    stop("Use only with 'ridge' objects")
  obj <- model
  x <- model.matrix(obj$terms, obj$model, obj$contrast)
  n <- nrow(x)
  p <- ncol(x)

  lambda <- obj$lambda
  rs <- svd(x, nv = 0)
  root <- rs$d / sqrt(rs$d^2 + lambda)
  u <- rs$u %*% diag(root)
  levs <- rowSums(u^2)
  names(levs) <- as.character(1:n)
  levs
}
