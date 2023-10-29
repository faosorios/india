## ID: condition.R, last updated 2023-10-24, F.Osorio

relative.condition <- function(x)
{ ## relative condition number (Hadi, CSDA 7, 143-159, 1988)
  ## Hadi (1988), Comput. Stat. Data An. 7, 143-159
  nobs <- nrow(x)
  cn  <- rep(0, nobs)
  cn0 <- scaled.condition(x, scales = FALSE)
  for (i in 1:nobs) 
    cn[i] <- scaled.condition(x[-i,], scales = FALSE)
  rel <- (cn - cn0) / cn0
  attr(rel, 'scaled condition') <- cn0
  names(rel) <- as.character(1:nobs)
  rel
}
