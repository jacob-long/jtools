
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

##################################################################
# GLMM r-squared -- lifted from MuMIN package to avoid dependency
##################################################################

`r.squaredGLMM` <-
  function(x)
    UseMethod("r.squaredGLMM")

`r.squaredGLMM.default` <-
  function(x) .NotYetImplemented()

`r.squaredGLMM.lme` <-
  function(x) {
    VarFx <- var(fitted(x, level = 0L))
    mmRE <- model.matrix(x$modelStruct$reStruct,
                         data = x$data[rownames(x$fitted), , drop = FALSE])
    n <- nrow(mmRE)

    sigma2 <- x$sigma^2
    reStruct <- x$modelStruct$reStruct
    if((m <- length(reStruct)) > 1L) {
      nams <- names(reStruct)
      for(i in seq.int(m))
        attr(reStruct[[i]], "Dimnames")[[2L]] <-
          paste(nams[[i]], attr(reStruct[[i]], "Dimnames")[[2L]], sep = ".")
    }

    varRe <- sum(sapply(reStruct, function(z) {
      sig <- nlme::pdMatrix(z) * sigma2
      mm1 <-  mmRE[, rownames(sig), drop = FALSE]
      #sum(diag(mm1 %*% sig %*% t(mm1))) / n
      sum(matmultdiag(mm1 %*% sig, ty = mm1)) / n
    }))

    varTot <- sum(VarFx, varRe)
    res <- c(VarFx, varTot) / (varTot + sigma2)
    names(res) <- c("R2m", "R2c")
    res
  }

## extracts random effect formula. e.g:
## ~ ... + (a | ...) + (b + c | ...) --> ~ a + b + c
ranform <- function (form) {
  ans <- update.formula(reformulate(vapply(lapply(.findbars(form),
                                                  "[[", 2L), deparse, "")), ~ . + 1)
  environment(ans) <- environment(form)
  ans
}

`.findbars` <- function (term) {
  if (is.name(term) || !is.language(term))
    return(NULL)
  if (term[[1L]] == as.name("("))
    return(.findbars(term[[2L]]))
  if (!is.call(term))
    stop("term must be of class call")
  if (term[[1L]] == as.name("|"))
    return(term)
  if (length(term) == 2L)
    return(.findbars(term[[2L]]))
  c(.findbars(term[[2L]]), .findbars(term[[3L]]))
}

matmultdiag <-
  function(x, y, ty = t(y)) {
    if(ncol(x) != ncol(ty)) stop('non-conformable arguments')
    if(nrow(x) != nrow(ty)) stop('result is not a square matrix')
    return(rowSums(x * ty))
  }

`r.squaredGLMM.merMod` <-
  function(x) {
    fam <- family(x)
    useObsLevVar <- (fam$family == "poisson" && fam$link == "log") || fam$family == "binomial"
    ## for poisson(log) and binomial(*), update 'x' to include individual-level
    ## variance (1 | 1:nobs(x)):
    if (useObsLevVar && !any(sapply(x@flist, nlevels) == nobs(x))) {
      cl <- get_call(x)
      frm <- formula(x)
      nRowData <- eval(call("eval", as.expression(call("NROW", cl$formula[[2L]])),
                            envir = cl$data), envir = environment(frm),
                       enclos = parent.frame())
      fl <- length(frm)
      frx <- . ~ . + 1
      frx[[3L]][[3L]] <- call("(", call("|", 1, call("gl", nRowData, 1)))
      cl$formula <- update.formula(frm, frx)
      x <- tryCatch(eval(cl, envir = environment(frm), enclos = parent.frame()),
                    error = function(e) {
                      cry(conditionCall(e), conditionMessage(e), warn = TRUE)
                      cry(cl, "fitting model with the observation-level random effect term failed. Add the term manually")
                    })
      message("The result is correct only if all data used by the model ",
              "has not changed since model was fitted.", domain = "R-MuMIn")
    }


    mmAll <- model.matrix(ranform(formula(x)), data = model.frame(x))
    ##Note: Argument 'contrasts' can only be specified for fixed effects
    ##contrasts.arg = eval(cl$contrasts, envir = environment(formula(x))))

    vc <- lme4::VarCorr(x)

    n <- nrow(mmAll)
    fx <- lme4::fixef(x) # fixed effect estimates
    fxpred <- as.vector(model.matrix(x) %*% fx)

    if (useObsLevVar) {
      vname <- names(x@flist)[sapply(x@flist, nlevels) == n][1L]
      varResid <- vc[[vname]][1L]
      beta0 <- mean(fxpred)
      vc <- vc[names(vc) != vname]
    } else {
      varResid <- attr(vc, "sc")^2
      beta0 <- NULL
    }

    if(!all(unlist(sapply(vc, rownames), use.names = FALSE) %in% colnames(mmAll)))
      stop("random term names do not match those in model matrix. \n",
           "Have 'options(contrasts)' changed since the model was fitted?")

    varRe <- if(length(vc) == 0L) 0L else
      sum(sapply(vc, function(sig) {
        mm1 <-  mmAll[, rownames(sig), drop = FALSE]
        # sum(matmult(mm1 %*% sig, t(mm1), diag.only = TRUE)) / n
        sum(matmultdiag(mm1 %*% sig, ty = mm1)) / n
        #sum(diag(mm1 %*% sig %*% t(mm1))) / n
      }))

    #varRe0 <- if(length(vc) == 0L) 0L else
    #          sum(sapply(vc, function(sig) sig[[1]]))

    .rsqGLMM(fam = family(x), varFx = var(fxpred), varRe = varRe,
             varResid = varResid, beta0 = beta0)
  }

`r.squaredGLMM.glmmML` <-
  function(x) {
    if(is.null(x$x))
      stop("glmmML must be fitted with 'x = TRUE'")

    fam <- family(x)
    useObsLevVar <- (fam$family == "poisson" && fam$link == "log") || fam$family == "binomial"
    if(useObsLevVar) {
      cry(, "cannot calculate 'unit variance' in glmmML")
    }
    fxpred <- as.vector(x$x %*% coef(x))
    .rsqGLMM(family(x), varFx = var(fxpred), varRe = x$sigma^2, varResid = NULL,
             beta0 = mean(fxpred))
  }

`r.squaredGLMM.lm` <-
  function(x) {
    fam <- family(x)
    .rsqGLMM(fam,
             varFx = var(as.vector(model.matrix(x) %*% coef(x))),
             #varFx = var(fitted(x)),
             varRe = 0,
             varResid = sum(if(is.null(x$weights)) resid(x)^2 else
               resid(x)^2 * x$weights) / df.residual(x),
             beta0 = if(fam$family == "poisson" && fam$link == "log")
               log(mean(model.response(model.frame(x)))) else
                 NULL
    )
  }

`.rsqGLMM` <-
  function (fam, varFx, varRe, varResid, beta0) {
    varDistr <- switch(paste(fam$family, fam$link, sep = "."),
                       gaussian.identity = 0,
                       binomial.logit = 3.28986813369645,
                       binomial.probit = 1,
                       poisson.log = {
                         expBeta0 <- exp(beta0)
                         if (expBeta0 < 6) cry(sys.call(-1L), "exp(beta0) of %0.1f is too close to zero, estimate may be unreliable \n",
                                               expBeta0, warn = TRUE)
                         log1p(1 / expBeta0)
                       },
                       poisson.sqrt = 0.25,
                       cry(sys.call(-1L), "do not know how to calculate variance for this family/link combination")
    )

    #print(c(Sf = varFx, Sl = varRe, Se = varResid, Sd = varDistr))
    #  total.var <- [Sf + Sl] + [Se + Sd]
    varTot <- sum(varFx, varRe)
    res <- c(varFx, varTot) / (varTot + varDistr + varResid)
    names(res) <- c("R2m", "R2c")
    res
  }

`cry` <-
  function(Call = NA, Message, ..., warn = FALSE, domain = paste0("R-", .packageName)) {
    if (is.character(Call)) {
      Call <- call(Call[1L], sys.call(-1L)[[1L]])
    } else if(is.numeric(Call)) {
      Call <- sys.call(Call - 1L)
    } else if(!is.call(Call)) Call <- sys.call(-1L)
    if(warn) warning(simpleWarning(gettextf(Message, ..., domain = domain), Call)) else
      stop(simpleError(gettextf(Message, ..., domain = domain), Call))
  }

`get_call` <- function(x) {
  rval <-
    if(isS4(x)) {
      if(any(i <- (sln <- c("call", "CALL", "Call")) %in% methods::slotNames(x)))
        slot(x, sln[i][1L]) else
          if(!is.null(attr(x, "call")))
            attr(x, "call") else NULL
    } else {
      if(!is.atomic(x) && (i <- match("call", names(x), nomatch = 0L)) != 0L) {
        x[[i]]
      } else if(!is.null(attr(x, "call"))) {
        attr(x, "call")
      } else
        NULL
    }
  if(is.null(rval)) stats::getCall(x) else rval
}

