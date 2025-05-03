## ID: cooks.R, last updated 2025-05-02, F.Osorio

cooks.distance.nls <- function(model, ...) 
{ ## Linear approximation of Cook's distance for nonlinear regression
  ## Ross (1987), Can. J. Stat. 15, 91-103.
  if (!inherits(model, "nls"))
    stop("Use only with 'nls' objects")
  obj <- model
  lev <- leverages(obj)
  res <- residuals(obj)
  attr(res, "label") <- NULL
  RSS <- deviance(obj)
  n <- length(res)
  p <- length(coef(obj))
  s2 <- RSS / (n - p)

  # using deletion formulas
  elim <- res / (1 - lev)
  cooks <- elim^2 * lev / (p * s2)
  
  names(cooks) <- as.character(1:n)
  cooks
}

cooks.distance.ols <- function(model, ...) 
{ ## Cook's distance for ordinary least squares
  if (!inherits(model, "ols"))
    stop("Use only with 'ols' objects")
  obj <- model
  lev <- leverages(obj)
  res <- residuals(obj)
  RSS <- deviance(obj)
  n <- obj$dims[1]
  p <- obj$dims[2]
  s2 <- RSS / (n - p)

  # using deletion formulas
  elim <- res / (1 - lev)
  cooks <- elim^2 * lev / (p * s2)
  
  names(cooks) <- as.character(1:n)
  cooks
}

cooks.distance.lad <- function(model, ...) 
{ ## Cook's distance for LAD regression
  ## Sun & Wei (2004), Stat. Prob. Lett. 67, 97-110.
  if (!inherits(model, "lad"))
    stop("Use only with 'lad' objects")
  obj <- model
  y <- model.response(obj$model, "numeric")
  x <- model.matrix(obj$terms, obj$model, obj$contrast)
  n <- obj$dims[1]
  p <- obj$dims[2]

  if (obj$method != "BR")
    stop("Use only with objects fitted by 'BR' option")

  # estimates using the full data
  SAD <- obj$SAD
  R <- obj$R
  omega <- SAD / (n - p) # proposal by Sun & Wei (2004)

  cooks <- rep(0, n)
  # estimation removing the i-th observation 
  for (i in 1:n) {
    cf <- lad.fit.BR(x[-i,], y[-i])$coef
    diff <- obj$coef - cf
    z <- c(R %*% diff)
    cooks[i] <- (minkowski(z) / omega)^2
  }
  
  names(cooks) <- as.character(1:n)
  cooks
}

cooks.distance.ridge <- function(model, type = "cov", ...) 
{ ## Cook's distance for ridge regression
  ## Walker & Birch (1988), Technometric 30, 221-227.
  if (!inherits(model, "ridge"))
    stop("Use only with 'ridge' objects")
  obj <- model
  y <- model.response(obj$model, "numeric")
  x <- model.matrix(obj$terms, obj$model, obj$contrast)
  n <- obj$dims[1]
  p <- obj$dims[2]

  # SVD of model matrix
  rs <- svd(x)

  # computing s2 from LS estimation
  z <- crossprod(rs$u, y)
  res <- c(y - rs$u %*% z)
  s2 <- sum(res^2) / (n - p)

  # leverages (code re-use)
  lambda <- obj$lambda
  Delta <- rs$d^2 / (rs$d^2 + lambda) 
  u <- rs$u %*% diag(sqrt(Delta))
  levs <- rowSums(u^2)
  hats <- rowSums(rs$u^2) # leverages from LS estimation

  # computation of Cook's distances (versions 1 and 2)
  res <- obj$residuals
  sqr <- (res / (1 - levs))^2  
  u <- rs$u %*% diag(Delta)
  CD1 <- (sqr / p) * rowSums(u^2) / s2
  CD2 <- (sqr / p) * hats / s2

  switch(type,
         "1st" = {
          cooks <- CD1 # Eq. (2.5), Walker & Birch (1988)
          names(cooks) <- as.character(1:n)
         },
         "cov" = {
          cooks <- CD2 # Eq. (2.6), Walker & Birch (1988)
          names(cooks) <- as.character(1:n)
         },
         "both" = {
          cooks <- cbind(CD1, CD2)
          colnames(cooks) <- c("1st", "cov")
          rownames(cooks) <- as.character(1:n)
         },
         stop(paste("unimplemented option:", type))
  )
  cooks
}
