## ID: envelope.R, last updated 2026-02-03, F.Osorio

envelope <- function(object, ...) UseMethod("envelope")

envelope.lm <- function(object, reps = 50, conf = 0.95, type = c("quantile", "standard", "student"), plot.it = TRUE, ...)
{ ## simulated envelope for linear regression
  envel <- function(n, x, mu, dispersion, levs, type, reps, conf) {
    conf <- 1 - conf
    # initialize progress bar
    cat(" Progress:\n")
    pb <- txtProgressBar(min = 0, max = reps, style = 3)
    elims <- matrix(0, nrow = n, ncol = reps)
    for (i in 1:reps) {
      rsp <- rnorm(n, mean = mu, sd = dispersion) 
      fit <- lm.fit(x, rsp)
      ftd  <- fit$fitted
      res <- fit$resid
      rdf <- fit$df.residual
      s2  <- sum(res^2) / rdf
      switch(type, 
             "quantile" = {
               logp <- pnorm(rsp, mean = ftd, sd = sqrt(s2), log.p = TRUE)
               res <- qnorm(logp, log.p = TRUE)
             },
             "standard" = {
               res <- res / sqrt(s2 * (1 - levs))
             },
             "student"  = {
               res <- res / sqrt(s2 * (1 - levs))
               res <- res * sqrt((rdf - 1) / (rdf - res^2))

             })
      elims[,i] <- sort(res)
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
    band <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n)
      band[i,] <- quantile(elims[i,], probs = c(conf / 2, 1 - conf / 2))
    band
  }

  type <- match.arg(type)
  if (!inherits(object, "lm"))
    stop("Use only with 'lm' objects")
  x <- model.matrix(object$terms, object$model, object$contrast)
  mu <- fitted(object)
  res <- resid(object)
  n <- nrow(x)
  s <- minkowski(res) / sqrt(object$df.residual)

  # leverages
  rs <- svd(x, nv = 0)
  levs <- rowSums(rs$u^2)

  # selected statistic and envelope
  z <- switch(type, 
              "quantile" = rquantile(object),
              "standard" = rstandard(object),
              "student"  = rstudent(object))
  band <- envel(n, x, mu, s, levs, type, reps, conf)

  if (plot.it) {
    lab <-  switch(type, "quantile" = "quantile", "standard" = "standardized", 
                         "student" = "studentized")
    ylim <- range(z, band)
    qqnorm(z, ylim = ylim, main = paste("Q-Q plot of", lab, "residuals"))
    par(new = TRUE)
    qqnorm(band[,1], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
    par(new = TRUE)
    qqnorm(band[,2], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
  }

  output <- list(residuals = z, envelope = band)
  invisible(output)
}

envelope.ols <- function(object, reps = 50, conf = 0.95, type = c("quantile", "standard", "student"), plot.it = TRUE, ...)
{ ## simulated envelope for ordinary least squares
  envel <- function(n, x, mu, dispersion, levs, type, reps, conf) {
    conf <- 1 - conf
    # initialize progress bar
    cat(" Progress:\n")
    pb <- txtProgressBar(min = 0, max = reps, style = 3)
    elims <- matrix(0, nrow = n, ncol = reps)
    for (i in 1:reps) {
      rsp <- rnorm(n, mean = mu, sd = dispersion) 
      fit <- ols.fit(x, rsp)
      ftd  <- fit$fitted
      res <- fit$resid
      rdf <- n - fit$dims[2]
      s2  <- fit$RSS / rdf
      switch(type, 
             "quantile" = {
               logp <- pnorm(rsp, mean = ftd, sd = sqrt(s2), log.p = TRUE)
               res <- qnorm(logp, log.p = TRUE)
             },
             "standard" = {
               res <- res / sqrt(s2 * (1 - levs))
             },
             "student"  = {
               res <- res / sqrt(s2 * (1 - levs))
               res <- res * sqrt((rdf - 1) / (rdf - res^2))

             })
      elims[,i] <- sort(res)
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
    band <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n)
      band[i,] <- quantile(elims[i,], probs = c(conf / 2, 1 - conf / 2))
    band
  }

  type <- match.arg(type)
  if (!inherits(object, "ols"))
    stop("Use only with 'ols' objects")
  x <- model.matrix(object$terms, object$model, object$contrast)
  mu <- fitted(object)
  res <- resid(object)
  n <- object$dims[1]
  p <- object$dims[2]
  s <- sqrt(object$RSS / (n - p))

  # leverages
  rs <- svd(x, nv = 0)
  levs <- rowSums(rs$u^2)

  # selected statistic and envelope
  switch(type, 
         "quantile" = {
           z <- rquantile(object)
         },
         "standard" = {
           z <- res / (s * sqrt(1 - levs))
         },
         "student" = {
           z <- res / (s * sqrt(1 - levs))
           z <- z * sqrt((n - p - 1) / (n - p - z^2))
         })
  band <- envel(n, x, mu, s, levs, type, reps, conf)

  if (plot.it) {
    lab <-  switch(type, "quantile" = "quantile", "standard" = "standardized", 
                         "student" = "studentized")
    ylim <- range(z, band)
    qqnorm(z, ylim = ylim, main = paste("Q-Q plot of", lab, "residuals"))
    par(new = TRUE)
    qqnorm(band[,1], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
    par(new = TRUE)
    qqnorm(band[,2], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
  }

  output <- list(residuals = z, envelope = band)
  invisible(output)
}

envelope.lad <- function(object, reps = 50, conf = 0.95, plot.it = TRUE, ...)
{ ## simulated envelope for LAD regression
  envel <- function(n, x, mu, dispersion, reps, conf) {
    conf <- 1 - conf
    # initialize progress bar
    cat(" Progress:\n")
    pb <- txtProgressBar(min = 0, max = reps, style = 3)
    elims <- matrix(0, nrow = n, ncol = reps)
    for (i in 1:reps) {
      rsp <- rlaplace(n, location = mu, scale = dispersion)
      fit <- lad.fit(x, rsp)
      ftd  <- fit$fitted
      # computing quantile residual
      logp <- plaplace(rsp, location = ftd, scale = fit$scale, log.p = TRUE)
      res <- qnorm(logp, log.p = TRUE)
      elims[,i] <- sort(res)
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
    band <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n)
      band[i,] <- quantile(elims[i,], probs = c(conf / 2, 1 - conf / 2))
    band
  }

  if (!inherits(object, "lad"))
    stop("Use only with 'lad' objects")
  if (object$method != "BR")
    stop("Use only with objects fitted by 'BR' option")
  x <- model.matrix(object$terms, object$model, object$contrast)
  mu <- fitted(object)
  n <- object$dims[1]
  scale <- object$scale

  # selected statistic and envelope
  z <- rquantile(object)
  band <- envel(n, x, mu, scale, reps, conf)

  if (plot.it) {
    ylim <- range(z, band)
    qqnorm(z, ylim = ylim, main = paste("Q-Q plot of quantile residuals"))
    par(new = TRUE)
    qqnorm(band[,1], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
    par(new = TRUE)
    qqnorm(band[,2], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
  }

  output <- list(residuals = z, envelope = band)
  invisible(output)
}

envelope.nls <- function(object, reps = 50, conf = 0.95, plot.it = TRUE, ...)
{ ## simulated envelope for nonlinear least squares
  envel <- function(n, form, data, start, mu, dispersion, reps, conf) {
    conf <- 1 - conf
    # initialize progress bar
    cat(" Progress:\n")
    pb <- txtProgressBar(min = 0, max = reps, style = 3)
    elims <- matrix(0, nrow = n, ncol = reps)
    for (i in 1:reps) {
      rsp <- rnorm(n, mean = mu, sd = dispersion)
      data$rsp <- rsp
      fit <- nls(formula = formula(form), data = data, start = start)
      ftd  <- fitted(fit)
      r <- resid(fit)
      attr(r, "label") <- NULL
      cf <- coef(fit)
      p <- length(cf) 
      scale <- minkowski(r) / sqrt(n - p)
      # computing quantile residual
      logp <- pnorm(rsp, mean = ftd, sd = scale, log.p = TRUE)
      res <- qnorm(logp, log.p = TRUE)
      elims[,i] <- sort(res)
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
    band <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n)
      band[i,] <- quantile(elims[i,], probs = c(conf / 2, 1 - conf / 2))
    band
  }

  if (!inherits(object, "nls"))
    stop("Use only with 'nls' objects")
  Call <- object$call
  data <- eval(Call$data, sys.frame(0))
  cf <- coef(object)
  start0 <- eval(Call$start, sys.frame(0))
  start1 <- relist(cf, skeleton = start0)
  res <- residuals(object)
  attr(res, "label") <- NULL
  RSS <- deviance(object)
  n <- length(res)
  p <- length(cf)
  s2 <- RSS / (n - p)
  mu <- fitted(object)
  attr(mu, "label") <- NULL
  ff <- object$m$formula()
  rhs <- asOneSidedFormula(ff[[3]])
  rhs <- as.character(rhs)[-1]
  form <- paste("rsp ~ ", rhs)

  # selected statistic and envelope
  z <- rquantile(object)
  band <- envel(n, form, data, start1, mu, sqrt(s2), reps, conf)

  if (plot.it) {
    ylim <- range(z, band)
    qqnorm(z, ylim = ylim, main = paste("Q-Q plot of quantile residuals"))
    par(new = TRUE)
    qqnorm(band[,1], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
    par(new = TRUE)
    qqnorm(band[,2], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
  }

  output <- list(residuals = z, envelope = band)
  invisible(output)
}

envelope.ridge <- function(object, reps = 50, conf = 0.95, plot.it = TRUE, ...)
{ ## simulated envelope for ridge regression
  envel <- function(n, form, db, mu, dispersion, lambda, reps, conf) {
    conf <- 1 - conf
    # initialize progress bar
    cat(" Progress:\n")
    pb <- txtProgressBar(min = 0, max = reps, style = 3)
    elims <- matrix(0, nrow = n, ncol = reps)
    for (i in 1:reps) {
      rsp <- rnorm(n, mean = mu, sd = dispersion)
      db$rsp <- rsp
      fit <-ridge(formula = form, data = db, lambda = lambda, method = "none")
      ftd  <- fit$fitted
      # computing quantile residual
      logp <- pnorm(rsp, mean = ftd, sd = sqrt(fit$scale), log.p = TRUE)
      res <- qnorm(logp, log.p = TRUE)
      elims[,i] <- sort(res)
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
    band <- matrix(0, nrow = n, ncol = 2)
    for (i in 1:n)
      band[i,] <- quantile(elims[i,], probs = c(conf / 2, 1 - conf / 2))
    band
  }

  if (!inherits(object, "ridge"))
    stop("Use only with 'ridge' objects")
  mu <- fitted(object)
  n <- object$dims[1]
  scale <- object$scale
  lambda <- object$lambda

  # extracting predictor variables
  lhs <- as.character(object$call$formula[[3]])
  ff <- as.formula(paste("rsp ~ ", lhs))
  db <- object$model

  # selected statistic and envelope
  z <- rquantile(object)
  band <- envel(n, ff, db, mu, scale, lambda, reps, conf)

  if (plot.it) {
    ylim <- range(z, band)
    qqnorm(z, ylim = ylim, main = paste("Q-Q plot of quantile residuals"))
    par(new = TRUE)
    qqnorm(band[,1], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
    par(new = TRUE)
    qqnorm(band[,2], axes = F, main = "", xlab = "", ylab = "", ylim = ylim, type = "l", lwd = 2, col = "red")
  }

  output <- list(residuals = z, envelope = band)
  invisible(output)
}
