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

# Stolen from sjstats so I don't have to list it as import

icc <- function(fit, obj.name) {
  # check if suggested package is available
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.", call. = FALSE)
  }

  # get family
  fitfam <- stats::family(fit)$family
  # is neg. binomial? Dropped sjstats' internal function in favor of regexp
  # is_negbin <- sjmisc::str_contains(fitfam, "Negative Binomial", ignore.case = TRUE)
  is_negbin <- grepl(ignore.case = TRUE, pattern = "Negative Binomial",
                     x = fitfam)

  # random effects variances
  # for details on tau and sigma, see
  # Aguinis H, Gottfredson RK, Culpepper SA2013. Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. Journal of Management 39(6): 1490–1528. doi:10.1177/0149206313478188.
  reva <- lme4::VarCorr(fit)
  # retrieve only intercepts
  vars <- lapply(reva, function(x) x[[1]])

  # random intercept-variances, i.e.
  # between-subject-variance (tau 00)
  tau.00 <- sapply(vars, function(x) x[1])

  # random slope-variances (tau 11)
  tau.11 <- unlist(lapply(reva, function(x) diag(x)[-1]))

  # residual variances, i.e.
  # within-cluster-variance (sigma^2)
  if (inherits(fit, "glmerMod") && fitfam == "binomial") {
    # for logistic models, we use pi / 3
    resid_var <- (pi ^ 2) / 3
  } else if (inherits(fit, "glmerMod") && is_negbin) {
    # for negative binomial models, we use 0
    resid_var <- 0
  } else {
    # for linear and poisson models, we have a clear
    # residual variance
    resid_var <- attr(reva, "sc") ^ 2
  }
  # total variance, sum of random intercept and residual variances
  total_var <- sum(sapply(vars, sum), resid_var)
  # check whether we have negative binomial
  if (is_negbin) {
    # for negative binomial models, we also need the intercept...
    beta <- as.numeric(lme4::fixef(fit)["(Intercept)"])
    # ... and the theta value to compute the ICC
    r <- lme4::getME(fit, "glmer.nb.theta")
    ri.icc <-
      (exp(tau.00) - 1) /
      ((exp(total_var) - 1) + (exp(total_var) / r) + exp(-beta - (total_var / 2)))
  } else {
    # random intercept icc
    ri.icc <- tau.00 / total_var
  }

  # get random slope random intercep correlations
  # do we have any rnd slopes?
  has_rnd_slope <- unlist(lapply(reva, function(x) dim(attr(x, "correlation"))[1] > 1))
  tau.01 <- rho.01 <- NULL

  # get rnd slopes
  if (any(has_rnd_slope)) {
    rnd_slope <- reva[has_rnd_slope]
    # get slope-intercept-correlations
    cor_ <- lapply(rnd_slope, function(x) attr(x, "correlation")[1, 2])
    # get standard deviations, multiplied
    std_ <- lapply(rnd_slope, function(x) prod(attr(x, "stddev")))
    # bind to matrix
    tau.01 <- apply(as.matrix(cbind(unlist(cor_), unlist(std_))), MARGIN = 1, FUN = prod)
    rho.01 <- unlist(cor_)
  }

  # name values
  names(ri.icc) <- names(reva)

  # add attributes, for print method
  class(ri.icc) <- c("icc.lme4", class(ri.icc))
  attr(ri.icc, "family") <- stats::family(fit)$family
  attr(ri.icc, "link") <- stats::family(fit)$link
  attr(ri.icc, "formula") <- stats::formula(fit)
  attr(ri.icc, "model") <- ifelse(inherits(fit, "glmerMod"), "Generalized linear mixed model", "Linear mixed model")
  attr(ri.icc, "tau.00") <- tau.00
  attr(ri.icc, "tau.01") <- tau.01
  attr(ri.icc, "rho.01") <- rho.01
  attr(ri.icc, "tau.11") <- tau.11
  attr(ri.icc, "sigma_2") <- resid_var
  # finally, save name of fitted model object. May be needed for
  # the 'se()' function, which accesses the global environment
  ## Not sure what sjstats is going for here but breaks my code -- Jacob
  # attr(ri.icc, ".obj.name") <- obj.name
  # return results
  return(ri.icc)

}
