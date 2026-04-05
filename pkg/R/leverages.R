## ID: leverages.R, last updated 2026-04-04, F.Osorio

leverages <- function(model, ...) ## leverages (AKA "hatvalues")
UseMethod("leverages")

leverages.lm <- function(model, infl = lm.influence(model, do.coef=FALSE), ...) ## for completeness only
infl$hat

leverages.nls <- function(model, type = "tangent", ...)
{ ## leverages for nonlinear regression
  ## Ross (1987), Can. J. Stat. 15, 91-103.
  ## St. Laurent & Cook (1992), J. Am. Stat. Assoc. 87, 985-990.
  ## St. Laurent & Cook (1993), Biometrika B 80, 99-106.
  if (!inherits(model, "nls"))
    stop("Use only with 'nls' objects")
  obj <- model
  F <- obj$m$gradient()
  n <- nrow(F)

  switch(type,
         # tangent plane leverage (Ross, 1987; St. Laurent & Cook, 1992)
         "tangent" = {
          Q0 <- qr.Q(qr(F)) # incomplete QR descomposition
          levs <- rowSums(Q0^2)
         },
         # Jacobian leverage (St. Laurent & Cook, 1992, 1993)
         "jacobian" = {
          # maximum effort begins..
          Call <- obj$call
          data <- eval(Call$data, sys.frame(0))
          cf <- coef(obj)
          p <- length(cf)
          pnames <- names(cf)
          ff <- obj$m$formula()
          rhs <- asOneSidedFormula(ff[[3]])
          second <- deriv(rhs, pnames, hessian = TRUE)
          env <- c(as.list(cf), as.list(data))
          val <- eval(second, env)
          hess <- attr(val, "hessian")
          res <- resid(obj)
          attr(res, "label") <- NULL
          prod <- bracket.prod(t(res), hess)
          prod <- matrix(prod, nrow = p, ncol = p)
          Rmat <- chol(crossprod(F) - prod)
          F <- F %*% solve(Rmat)
          levs <- rowSums(F^2)
         },
         stop(paste("unimplemented option:", type))
  )
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

  Q0 <- qr.Q(qr(x), complete = FALSE) # incomplete QR descomposition
  levs <- rowSums(Q0^2)
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
