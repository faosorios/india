## ID: LD.R, last updated 2023-10-24, F.Osorio

logLik.displacement <- function(model, ...) 
UseMethod("logLik.displacement")

logLik.displacement.lm <- function(model, pars = "full", ...) 
{ ## Likelihood displacement for linear regression
  if (!inherits(model, "lm"))
    stop("Use only with 'lm' objects")
  obj <- model
  lev <- leverages(obj)
  res <- residuals(obj)
  RSS <- deviance(obj)
  n <- length(res)

  b <- res^2 / (RSS * (1 - lev))
  switch(pars,
         full = {
          ratio <- n * (1 - b) / (n - 1)
          rel <- b / (1 - b)
          LD <- n * log(ratio) + (n - 1) * rel / (1 - lev) - 1
         },
         coef = {
          rel <- lev / (1 - lev)
          LD <- n * log(b * rel + 1)
         },
         stop(paste("unimplemented option:", pars))
  )
  names(LD) <- as.character(1:n)
  LD
}

logLik.displacement.ols <- function(model, pars = "full", ...) 
{ ## Likelihood displacement for ordinary LS (code re-use)
  if (!inherits(model, "ols"))
    stop("Use only with 'ols' objects")
  obj <- model
  lev <- leverages(obj)
  res <- residuals(obj)
  RSS <- deviance(obj)
  n <- length(res)

  b <- res^2 / (RSS * (1 - lev))
  switch(pars,
         full = {
          ratio <- n * (1 - b) / (n - 1)
          rel <- b / (1 - b)
          LD <- n * log(ratio) + (n - 1) * rel / (1 - lev) - 1
         },
         coef = {
          rel <- lev / (1 - lev)
          LD <- n * log(b * rel + 1)
         },
         stop(paste("unimplemented option:", pars))
  )
  names(LD) <- as.character(1:n)
  LD
}

logLik.displacement.lad <- function(model, method = "quasi", pars = "full", ...) 
{ ## Likelihood displacement for LAD regression
  if (!inherits(model, "lad"))
    stop("Use only with 'lad' objects")
  obj <- model
  y <- model.response(obj$model, "numeric")
  x <- model.matrix(obj$terms, obj$model, obj$contrast)
  n <- nrow(x)

  switch(method,
    quasi = {
      # Quasi-likelihood displacement for LAD regression
      # Sun & Wei (2004), Stat. Prob. Lett. 67, 97-110
      SAD <- SAD.deleted(x, y)$SAD
      LD <- 2 * (SAD - obj$SAD)
    },
    BR = {
      # Likelihood displacement for LAD regression
      # Elian et al. (2000), Commun. Stat. Theory 29, 837-849
      z <- SAD.deleted(x, y)
      switch(pars,
             full = {
              ratio <- z$scales / obj$scale
              LD <- 2 * n * (log(ratio) + sqrt(2) * z$SAD / (n * z$scales) - 1)
             },
             coef = {
              ratio <- z$SAD / obj$SAD
              LD <- 2 * n * log(ratio)
             },
             stop(paste("unimplemented option:", pars))
      )
    },
    stop(paste("unimplemented method:", method))
  )
  names(LD) <- as.character(1:n)
  LD
}

SAD.deleted <- function(x, y)
{ ## computation of SAD without i-th observation
  n <- nrow(x)
  SAD <- scales <- rep(0, n)

  # removing the i-th observation
  for (i in 1:n) {
    z <- lad.fit.BR(x[-i,], y[-i])
    scales[i] <- z$scale
    cf  <- z$coef
    dev <- c(y - x %*% cf)
    SAD[i] <- minkowski(dev, p = 1)
  }

  o <- list(SAD = SAD, scales = scales)
  o
}

logLik.displacement.ridge <- function(model, pars = "full", ...) 
{ ## Likelihood displacement for ridge regression
  if (!inherits(model, "ridge"))
    stop("Use only with 'ridge' objects")
  obj <- model
  n <- obj$dims[1]
  lambda <- obj$lambda
  s2 <- obj$scale
  
  z <- ridge.deleted(obj)
  switch(pars,
         full = {
          scale <- z$RSS + lambda * z$PEN
          ratio <- z$s2 / s2
          LD <- n * (log(ratio) - 1) + scale / z$s2
         },
         coef = {
          scale <- (z$RSS + lambda * z$PEN) / n
          ratio <- scale / s2
          LD <- n * log(ratio)
         },
         stop(paste("unimplemented option:", pars))
  )
  names(LD) <- as.character(1:n)
  LD
}

ridge.deleted <- function(obj)
{ ## removing the i-th observation in ridge estimation
  y <- model.response(obj$model, "numeric")
  x <- model.matrix(obj$terms, obj$model, obj$contrast)
  cl <- obj$call
  n  <- obj$dims[1]
  lambda <- obj$lambda
  RSS <- PEN <- s2 <- rep(0, n)
  obs <- 1:n

  # setting the fitter
  cl$method <- "none"
  cl$lambda <- lambda

  # removing the i-th observation
  for (i in 1:n) {
    cl$subset <- obs[-i]
    z <- eval(cl)
    s2[i] <- z$scale
    cf <- z$coef
    dev <- c(y - x %*% cf)
    RSS[i] <- minkowski(dev)^2
    PEN[i] <- minkowski(cf)^2
  }

  o <- list(RSS = RSS, PEN = PEN, s2 = s2)
  o
}
