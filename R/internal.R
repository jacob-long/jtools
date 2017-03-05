#### Internal function
### This is taken from pscl package, I don't want to list it as import for
### this alone. The return object needs tweaking for me anyway
pR2Work <- function(llh,llhNull,n){
  McFadden <- 1 - llh/llhNull
  G2 <- -2*(llhNull-llh)
  r2ML <- 1 - exp(-G2/n)
  r2ML.max <- 1 - exp(llhNull*2/n)
  r2CU <- r2ML/r2ML.max
  out <- NULL
  out$llh <- llh
  out$llhNull <- llhNull
  out$G2 <- G2
  out$McFadden <- McFadden
  out$r2ML <- r2ML
  out$r2CU <- r2CU
  out
}

pR2 <- function(object){
  llh <- suppressWarnings(logLik(object))
  objectNull <- suppressWarnings(update(object, ~ 1))
  llhNull <- logLik(objectNull)
  n <- dim(object$model)[1]
  pR2Work(llh,llhNull,n)
}

# Weighted std. dev. used in gscale
wtd.sd <- function(x, w) {
  xm <- weighted.mean(x, w)
  out <- sum((w * (x - xm)^2)/(sum(w)-1))
  out <- sqrt(out)
  return(out)
}

