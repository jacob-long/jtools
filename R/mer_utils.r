
# Taken from sjstats so I don't have to list it as import

icc <- function(fit, obj.name) {
  # check if suggested package is available
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # get family
  fitfam <- stats::family(fit)$family
  # is neg. binomial? Dropped sjstats' internal function in favor of regexp
  # is_negbin <-
  # sjmisc::str_contains(fitfam, "Negative Binomial", ignore.case = TRUE)
  is_negbin <- grepl(ignore.case = TRUE, pattern = "Negative Binomial",
                     x = fitfam)

  # random effects variances
  # for details on tau and sigma, see
  # Aguinis H, Gottfredson RK, Culpepper SA2013. Best-Practice Recommendations
  # for Estimating Cross-Level Interaction Effects Using Multilevel Modeling.
  # Journal of Management 39(6): 1490–1528. doi:10.1177/0149206313478188.
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
      ((exp(total_var) - 1) + (exp(total_var) / r)
       + exp(-beta - (total_var / 2)))
  } else {
    # random intercept icc
    ri.icc <- tau.00 / total_var
  }

  # get random slope random intercep correlations
  # do we have any rnd slopes?
  has_rnd_slope <-
    unlist(lapply(reva, function(x) dim(attr(x, "correlation"))[1] > 1))
  tau.01 <- rho.01 <- NULL

  # get rnd slopes
  if (any(has_rnd_slope)) {
    rnd_slope <- reva[has_rnd_slope]
    # get slope-intercept-correlations
    cor_ <- lapply(rnd_slope, function(x) attr(x, "correlation")[1, 2])
    # get standard deviations, multiplied
    std_ <- lapply(rnd_slope, function(x) prod(attr(x, "stddev")))
    # bind to matrix
    tau.01 <- apply(as.matrix(cbind(unlist(cor_), unlist(std_))),
                    MARGIN = 1, FUN = prod)
    rho.01 <- unlist(cor_)
  }

  # name values
  names(ri.icc) <- names(reva)

  # add attributes, for print method
  class(ri.icc) <- c("icc.lme4", class(ri.icc))
  attr(ri.icc, "family") <- stats::family(fit)$family
  attr(ri.icc, "link") <- stats::family(fit)$link
  attr(ri.icc, "formula") <- stats::formula(fit)
  attr(ri.icc, "model") <- ifelse(inherits(fit, "glmerMod"),
                                  "Generalized linear mixed model",
                                  "Linear mixed model")
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

get_re_tables_mer <- function(model, re.variance, groups, ngroups, iccs) {
  ## Make a table summarizing grouping vars
  gvmat <- matrix(ncol = 3, nrow = length(ngroups))
  colnames(gvmat) <- c("Group","# groups","ICC")
  for (i in seq_len(length(ngroups))) {
    gvmat[i,1] <- groups[i]
    gvmat[i,2] <- ngroups[i]
    gvmat[i,3] <- iccs[i]
  }

  ## Make table explaining random coefs
  rcmat <- as.data.frame(lme4::VarCorr(model))
  rcmat <- rcmat[is.na(rcmat$var2),]
  re_variance <- switch(re.variance, sd = "sdcor", var = "vcov")
  re_var_lab <- switch(re.variance, sd = "Std. Dev.", var = "Var.")
  rcmat <- rcmat[, names(rcmat) %in% c("grp", "var1", re_variance)]
  rcmat <- as.matrix(rcmat)
  colnames(rcmat) <- c("Group", "Parameter", re_var_lab)
  attr(rcmat, "variance") <- re_var_lab

  return(list(gvmat = gvmat, rcmat = rcmat))
}

##########################################################################
# GLMM r-squared -- adapted from piecewiseSEM package
##########################################################################

### piecewisesem implementation
#' @importFrom stats var

pR2_merMod <- function(model) {

  ret <- list()

  # Get R2 for class == merMod
  if (any(class(model) %in% c("lmerMod", "lmerModLmerTest"))) {

    # Get variance of fixed effects by multiplying coefficients by design matrix
    varF <- var(as.vector(lme4::fixef(model) %*% t(model@pp$X)))

    # Check to see if random slopes are present as fixed effects
    random.slopes <- if ("list" %in% class(lme4::ranef(model))) { # nolint
      unique(as.vector(sapply(lme4::ranef(model), colnames)))
    } else {
      colnames(lme4::ranef(model))
    }

    # Separate observation variance from variance of random effects
    n.obs <- names(
      unlist(
        lapply(
          lme4::ranef(model), nrow))[
            !unlist(lapply(lme4::ranef(model), nrow)) == nrow(model@pp$X)
          ]
      )

    # Get variance of random effects
    varRand <- sum(
      sapply(lme4::VarCorr(model)[n.obs], function(Sigma) {
        X <- model.matrix(model)
        Z <- X[, rownames(Sigma), drop = FALSE]
        Z.m <- Z %*% Sigma
        sum(diag(crossprod(Z.m, Z))) / nrow(X)
      } )
    )

    # Get residual variance
    varResid <- attr(lme4::VarCorr(model), "sc")^2
    # Calculate R2 values
    ret$Marginal <- varF / (varF + varRand + varResid)
    ret$Conditional <- (varF + varRand) / (varF + varRand + varResid)

  }

  # Get R2 for class == "glmerMod"
  if (any(class(model) %in% c("glmerMod"))) {

    # Classify model family
    ret$Family <- summary(model)$family

    # Classify link function
    ret$Link <- summary(model)$link

    # Get variance of fixed effects by multiplying coefficients by
    # design matrix
    varF <- var(as.vector(lme4::fixef(model) %*% t(model@pp$X)))

    # Check to see if random slopes are present as fixed effects
    random.slopes <- if ("list" %in% class(lme4::ranef(model))) {
      unique(as.vector(sapply(lme4::ranef(model), colnames)))
    } else {
        colnames(lme4::ranef(model))
    }

    # Separate observation variance from variance of random effects
    n.obs <- names(
      unlist(
        lapply(
          lme4::ranef(model), nrow)
        )[!unlist(lapply(lme4::ranef(model), nrow)) == nrow(model@pp$X)]
      )

    # Get variance of random effects
    varRand <- sum(

      sapply(lme4::VarCorr(model)[n.obs], function(Sigma) {
        X <- model.matrix(model)
        Z <- X[, rownames(Sigma), drop = FALSE]
        Z.m <- Z %*% Sigma
        sum(diag(crossprod(Z.m, Z))) / nrow(X)
      })

    )

    # Get overdispersion variance
    obs <- names(
      unlist(
        lapply(
          lme4::ranef(model), nrow))[unlist(
            lapply(lme4::ranef(model), nrow)) == nrow(model@pp$X)]
      )

    if (length(obs) == 0) {

      varDisp <- 0

    } else {

      varDisp <-  sum(
        sapply(lme4::VarCorr(model)[obs], function(Sigma) {
          X <- model.matrix(model)
          Z <- X[, rownames(Sigma)]
          Z.m <- Z %*% Sigma
          sum(diag(crossprod(Z.m, Z))) / nrow(X)
        })
      )

    }

    # Get distribution-specific variance
    if (ret$Family == "binomial") {

      if (ret$Link == "logit") {
      varDist <- (pi^2)/3
      } else if (ret$Link == "probit") {
        varDist <- 1
      } else {
        warning(paste("Model link '", summary(model)$link,
                      "' is not yet supported for the ",
                      summary(model)$family, "distribution"))
        varDist <- NA
      }

    } else if (ret$Family == "poisson" || grepl("Negative Binomial", ret$Family)) {

      # Generate null model (intercept and random effects only,
      # no fixed effects)
      null.model <-
        update(model, formula = paste(". ~ ", get.random.formula(model, "~1")))

      # Get the fixed effects of the null model
      null.fixef <- as.numeric(lme4::fixef(null.model))
      if (ret$Link == "log") {varDist <- log(1 + 1/exp(null.fixef))}

    } else if (ret$Link == "sqrt") {
      varDist <- 0.25
    } else {
      warning(paste("Model link '", summary(model)$link,
                    "' is not yet supported for the ",
                    summary(model)$family, "distribution"))
      varDist <- NA
    }

    # Calculate R2 values
    ret$Marginal <- varF / (varF + varRand + varDisp + varDist)
    ret$Conditional <- (varF + varRand) / (varF + varRand + varDisp + varDist)

  }
  return(ret)
}

get.random.formula <- function(model, rhs) {

  if (class(rhs) == "formula") {rhs <- Reduce(paste, deparse(rhs))}

  # Get random formula from model
  random.formula <- if (any(class(model) %in% c("lmerMod", "lmerModLmerTest",
                                                "glmerMod", "glmmTMB"))) {

        paste("(", lme4::findbars(formula(model)), ")", collapse = " + ")

  }

  # Get random structure(s)
  random.structure <- if (any(class(model) %in% c("lmerMod", "lmerModLmerTest",
                                                  "glmerMod", "glmmTMB"))) {

      ran.ef.splt <- strsplit(random.formula, "\\+.")[[1]]

      sapply(ran.ef.splt[sapply(ran.ef.splt, function(x) grepl("\\|", x))],

             function(x)

               gsub(" ", "", gsub(".*\\|(.*)\\)", "\\1", x))

      )

  }

  random.structure <- unname(random.structure[!duplicated(random.structure)])

  # Get random slopes in the model list, otherwise return vector of
  # terms to drop
  random.slopes <- if (any(class(model) %in% c("glmerMod", "lmerModLmerTest",
                                               "glmmTMB"))) {

    ran.ef <- ifelse(any(class(lme4::ranef(model)) != "list"),
                     list(lme4::ranef(model)),
                     lme4::ranef(model))

    as.vector(sapply(ran.ef, function(x) colnames(x)))

  }

  random.slopes <- unname(random.slopes[!duplicated(random.slopes) &
                          random.slopes != "(Intercept)"])

  # Define new random slopes
  new.random.slopes <- random.slopes[which(random.slopes %in%
                                             unlist(strsplit(rhs, ".\\+.")))]

  if (length(new.random.slopes) == 0) {
    new.random.slopes <- 1
  } else {
    new.random.slopes <- paste0(new.random.slopes, collapse = " + ")
  }

  # Replace random slopes if any variables in model formula appear in
  # random slopes
  if (length(random.slopes) != 0) {

    if (any(class(model) %in% c("glmerMod", "lmerModLmerTest", "glmmTMB"))) {

        paste(
          sapply(random.structure, function(x)
            paste("(", new.random.slopes, " | ", x, ")") ),
          collapse = " + ")

    }

  } else if (length(random.slopes) == 0) {

    if (is.list(random.structure)) {

      eval(parse(text = gsub("*\\~(.*)", paste0("~ ", new.random.slopes, "))"),
                             random.formula)))

    } else {

      random.formula
    }

  }

}

#### lmer p values ###########################################################

get_df_kr <- function(model) {
  L <- diag(rep(1, length(lme4::fixef(model))))
  L <- as.data.frame(L)
  dfs <- sapply(L, pbkrtest::get_Lb_ddf, object = model)
  names(dfs) <- names(lme4::fixef(model))
  return(dfs)
}

# get_df_all_kr <- function(model) {
#   pbkrtest::get_ddf_Lb(model, lme4::fixef(model))
# }

get_se_kr <- function(model) {
  vcov_adj <- pbkrtest::vcovAdj(model)

  fes <- lme4::fixef(model)
  len <- length(fes)
  Lmat <- diag(len)

  qform <- function(x, A) sum(x * (A %*% x))

  ses <- sapply(1:len, function(.x) {
    sqrt(qform(Lmat[.x, ], as.matrix(vcov_adj)))
    })
  names(ses) <- names(fes)

  return(ses)
}

get_all_sat <- function(model) {
  if ("lmerModLmerTest" %nin% class(model)) {
    new_mod <- lmerTest::as_lmerModLmerTest(model)
  } else {new_mod <- model}
  coefs <- summary(new_mod)$coefficients
  return(coefs)
}


#### merMod prediction #######################################################
#' @title Alternative interface for `merMod` predictions
#' @description This function generates predictions for `merMod` models, but
#'  with the ability to get standard errors as well. 
#' @inheritParams lme4:::predict.merMod
#' @param se.fit Include standard errors with the predictions? Note that
#'  these standard errors by default include only fixed effects variance.
#'  See details for more info. Default is FALSE.
#' @param use.re.var If `se.fit` is TRUE, include random effects variance in
#'  standard errors? Default is FALSE.
#' @param boot Use bootstrapping (via [lme4::bootMer()]) to estimate 
#'  variance for `se.fit`? Default is FALSE
#' @param sims If `boot` is TRUE, how many simulations should be run? Default 
#'  is 100.
#' @param prog.arg If `boot` and `se.fit` are TRUE, a character string - 
#'  type of progress bar to display. Default is "none"; the function will look
#'  for a relevant *ProgressBar function, so "txt" will work in general;
#'  "tk" is available if the tcltk package is loaded; or "win" on Windows 
#'  systems. Progress bars are disabled (with a message) for parallel operation.
#' @param ... When `boot` and `se.fit` are TRUE, any additional arguments are
#'  passed to `lme4::bootMer()`.
#' @details The developers of \pkg{lme4} omit an `se.fit` argument for a 
#'  reason, which is that it's not perfectly clear how best to estimate 
#'  the variance for these models. This solution is a logical one, but perhaps
#'  not perfect. Bayesian models are one way to do better. 
#'  
#'  The method used here is based on the one described here:
#'  \url{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions}
#' @export 
#' @importFrom stats vcov model.frame terms delete.response na.omit
predict_merMod <- function(object, newdata = NULL, se.fit = FALSE,
                           use.re.var = FALSE, allow.new.levels = FALSE, 
                           type = c("link", "response", "terms"), 
                           na.action = na.pass, re.form = NULL, 
                           boot = FALSE, sims = 100, prog.arg = "none", ...) {

  type <- match.arg(type, c("link", "response", "terms"))
  if (is.null(newdata) & is.null(re.form)) {
    ## raw predict() call, just return fitted values
    ##   (inverse-link if appropriate)
    if (lme4::isLMM(object) || lme4::isNLMM(object)) {
      ## make sure we do *NOT* have NAs in fitted object
      fit <- na.omit(fitted(object))
    } else { ## inverse-link
      fit <-  switch(type, response = object@resp$mu, ## == fitted(object),
                     link = object@resp$eta)
      if (is.null(nm <- rownames(model.frame(object)))) {
        nm <- seq_along(fit)
      }
      names(fit) <- nm
    }
    fit.na.action <- NULL

    # Need this for SEs
    X <- lme4::getME(object, "X")
    X.col.dropped <- attr(X, "col.dropped")

    ## flow jumps to end for na.predict
  } else { ## newdata and/or re.form

    fit.na.action <- attr(object@frame, "na.action")  ## original NA action

    nobs <- if (is.null(newdata)) nrow(object@frame) else nrow(newdata)
    fit <- rep(0,nobs)

    X <- lme4::getME(object, "X")
    X.col.dropped <- attr(X, "col.dropped")
    ## modified from predict.glm ...
    if (is.null(newdata)) {
      ## Use original model 'X' matrix and offset
      ## orig. offset: will be zero if there are no matches ...
      offset <- model.offset(model.frame(object))
      if (is.null(offset)) offset <- 0
    } else {  ## new data specified
      ## evaluate new fixed effect
      RHS <- formula(substitute(~R,
                                list(R = RHSForm(formula(object,
                                                         fixed.only = TRUE)))))
      Terms <- terms(object, fixed.only = TRUE)
      mf <- model.frame(object, fixed.only = TRUE)
      isFac <- vapply(mf, is.factor, FUN.VALUE = TRUE)
      ## ignore response variable
      isFac[attr(Terms,"response")] <- FALSE
      orig_levs <- if (length(isFac) == 0) NULL else lapply(mf[isFac], levels)

      mfnew <- suppressWarnings(
        model.frame(delete.response(Terms), newdata,
                    na.action = na.action, xlev = orig_levs))

      X <- model.matrix(RHS, data = mfnew,
                        contrasts.arg = attr(X,"contrasts"))
      ## hack to remove unused interaction levels?
      ## X <- X[,colnames(X0)]

      offset <- 0 # rep(0, nrow(X))
      tt <- terms(object)
      if (!is.null(off.num <- attr(tt, "offset"))) {
        for (i in off.num)
          offset <- offset + eval(attr(tt,"variables")[[i + 1]], newdata)
      }
      fit.na.action <- attr(mfnew, "na.action")
      ## only need to drop if new data specified ...
      if (is.numeric(X.col.dropped) && length(X.col.dropped) > 0) {
        X <- X[, -X.col.dropped, drop = FALSE]
      }

      fit <- drop(X %*% lme4::fixef(object))
      fit <- fit + offset

    }

  }  ## newdata/newparams/re.form

  if (se.fit == TRUE & boot == FALSE) {
    .vcov <- as.matrix(vcov(object))
    fit.ses <- sqrt(diag(X %*% .vcov %*% t(X)))

    if (lme4::isGLMM(object)) {
      switch(type, response = {
        fit.ses <- fit.ses * abs(family(object)$mu.eta(fit))
        fit <- object@resp$family$linkinv(fit)
      }, link = , terms = )
    } else {
      fit.ses <- fit.ses * abs(family(object)$mu.eta(fit))
    }

    re_vcov <- lme4::VarCorr(object)
    # Sum each random intercept variance
    re_variances <- sum(sapply(re_vcov, function(x) {x[1]}))

    if (use.re.var == TRUE) {
      fit.ses <- fit.ses + re_variances
    }

  } else if (se.fit == TRUE & boot == TRUE) {

    bootfun <- function(object) {
      drop( X  %*% lme4::fixef(object) )
    }

    bo <- lme4::bootMer(object, FUN = bootfun, nsim = sims,
                        .progress = prog.arg, ...)
    fit <- bo$t

    if (lme4::isGLMM(object)) {
      switch(type, response = {
        fit <- object@resp$family$linkinv(fit)
      }, link = , terms = )
    }

  }

  if (!noReForm(re.form)) {
    if (is.null(re.form))
      re.form <- reOnly(formula(object)) # RE formula only
    rfd <- if (is.null(newdata)) {object@frame} else {newdata}
    newRE <- mkNewReTrms(object, rfd, re.form, na.action = na.action,
                         allow.new.levels = allow.new.levels)
    REvals <- base::drop(methods::as(newRE$b %*% newRE$Zt, "matrix"))
    fit <- fit + REvals
  }

  if (se.fit == FALSE & lme4::isGLMM(object)) {
    switch(type, response = {
      fit <- object@resp$family$linkinv(fit)
    }, link = , terms = )
  }

  if (se.fit == TRUE & boot == FALSE) {
    return(list(fit = fit, se.fit = fit.ses))
  } else if (se.fit == TRUE & boot == TRUE) {
    return(list(fit = fit))
  } else {
    return(fit)
  }


}

# Use same old internal interface
predict_mer <- function(object, use_re_var = FALSE, prog_arg = "none", ...) {
  predict_merMod(object, use.re.var = use_re_var, prog.arg = prog_arg, ...)
}

# # adapted from https://stackoverflow.com/a/49403210/5050156
# pred_terms_merMod <- function(model, newdata = NULL) {
#   tt <- terms(model)
#   beta <- lme4::fixef(model)
  
#   if (is.null(newdata)) {
#     newdata <- get_data(model, warn = FALSE)
#   }
  
#   mm <- model.matrix(tt, newdata)
#   aa <- attr(mm, "assign")
#   ll <- attr(tt, "term.labels")
#   hasintercept <- attr(tt, "intercept") > 0L
#   if (hasintercept) 
#     ll <- c("(Intercept)", ll)
#   aaa <- factor(aa, labels = ll)
#   asgn <- split(order(aa), aaa)
#   if (hasintercept) {
#     asgn$"(Intercept)" <- NULL
#     avx <- colMeans(mm)
#     termsconst <- sum(avx * beta)
#   }
#   nterms <- length(asgn)
#   if (nterms > 0) {
#     predictor <- matrix(ncol = nterms, nrow = NROW(mm))
#     dimnames(predictor) <- list(rownames(mm), names(asgn))
#     if (hasintercept) 
#       mm <- sweep(mm, 2L, avx, check.margin = FALSE)
#     for (i in seq.int(1L, nterms, length.out = nterms)) {
#       idx <- asgn[[i]]
#       predictor[, i] <- mm[, idx, drop = FALSE] %*% beta[idx]
#     }
#   } else {
#     # Not sure if NROW(mm) is right
#     predictor <- ip <- matrix(0, NROW(mm), 0L)
#   }
#   attr(predictor, "constant") <- if (hasintercept) termsconst else 0
#   predictor
# }

# ### Add random effects to df ################################################
#
# add_ranefs <- function(model, data = NULL) {
#
#   the_res <- lme4::ranef(model)
#
#   for (i in seq_along(the_res)) {
#
#     grp_var <- names(the_res)[i]
#
#     for (j in seq_along(the_res[[grp_var]])) {
#
#       # Name of the predictor with random effect
#       ran_var <- names(the_res[[grp_var]])[j]
#       # Name of the column to be added to original data
#       new_var <- paste0(grp_var, "_", ran_var)
#       # If it's the intercept, give it a syntactically valid name
#       new_var <- gsub("(Intercept)", "intercept", new_var, fixed = TRUE)
#
#       # Create 1-column data frame with the random effects
#       grps_plus_effects <- the_res[[grp_var]][, j, drop = FALSE]
#       # Add the rownames --- the value of the grouping var
#       grps_plus_effects[grp_var] <- rownames(grps_plus_effects)
#
#       # Going to coerce grouping factor to character in both DFs
#       temp_dat <- data
#       temp_dat[[grp_var]] <- as.character(temp_dat[[grp_var]])
#
#       # Match the ranefs to observations by inner joining
#       new_dat <- merge(temp_dat, grps_plus_effects, by = grp_var)
#       # Keep just the new column, and keep it separate
#       new_col <- new_dat[names(new_dat) %nin% names(temp_dat)]
#       # Append new column to original data frame with pre-specified var name
#       data[[new_var]] <- unlist(new_col)
#
#     }
#
#   }
#
#   return(data)
#
# }
#

### Lifted from lme4 ########################################################

noReForm <- function(re.form) {
  (!is.null(re.form) && !methods::is(re.form, "formula") && is.na(re.form)) ||
  (methods::is(re.form, "formula") && length(re.form) == 2 &&
     identical(re.form[[2]], 0))
}

reOnly <- function(f, response = FALSE) {
  response <- if (response && length(f) == 3) { f[[2]] } else { NULL }
  reformulate(paste0("(", vapply(lme4::findbars(f), safeDeparse,
                                 ""), ")"), response = response)
}

safeDeparse <- function (x, collapse = " ") {
  paste(deparse(x, 500L), collapse = collapse)
}

mkNewReTrms <- function(object, newdata, re.form = NULL, na.action = na.pass,
                         allow.new.levels = FALSE) {

  # # Fix CRAN complaint, I think
  Lind <- NULL

  if (is.null(newdata)) {
    rfd <- mfnew <- model.frame(object)
  }
  else {
    mfnew <- model.frame(delete.response(terms(object, fixed.only = TRUE)),
                         newdata, na.action = na.action)
    old <- FALSE
    if (old) {
      rfd <- na.action(newdata)
      if (is.null(attr(rfd, "na.action")))
        attr(rfd, "na.action") <- na.action
    }
    else {
      newdata.NA <- newdata
      if (!is.null(fixed.na.action <- attr(mfnew, "na.action"))) {
        newdata.NA <- newdata.NA[-fixed.na.action, ]
      }
      tt <- delete.response(terms(object, random.only = TRUE))
      rfd <- model.frame(tt, newdata.NA, na.action = na.pass)
      if (!is.null(fixed.na.action))
        attr(rfd, "na.action") <- fixed.na.action
    }
  }
  if (inherits(re.form, "formula")) {
    if (length(fit.na.action <- attr(mfnew, "na.action")) >
        0) {
      newdata <- newdata[-fit.na.action, ]
    }
    ReTrms <- lme4::mkReTrms(lme4::findbars(re.form[[2]]), rfd)
    ReTrms <- within(ReTrms, Lambdat@x <- unname(lme4::getME(object,
                                                       "theta")[Lind]))
    if (!allow.new.levels && any(vapply(ReTrms$flist, anyNA,
                                        NA)))
      stop("NAs are not allowed in prediction data",
           " for grouping variables unless allow.new.levels is TRUE")
    ns.re <- names(re <- lme4::ranef(object))
    nRnms <- names(Rcnms <- ReTrms$cnms)
    if (!all(nRnms %in% ns.re))
      stop("grouping factors specified in re.form that were not present in original model")
    new_levels <- lapply(ReTrms$flist, function(x) levels(factor(x)))
    re_x <- Map(function(r, n) levelfun(r, n, allow.new.levels = allow.new.levels),
                re[names(new_levels)], new_levels)
    re_new <- lapply(seq_along(nRnms), function(i) {
      rname <- nRnms[i]
      if (!all(Rcnms[[i]] %in% names(re[[rname]])))
        stop("random effects specified in re.form that were not present in original model")
      re_x[[rname]][, Rcnms[[i]]]
    })
    re_new <- unlist(lapply(re_new, t))
  }
  Zt <- ReTrms$Zt
  attr(Zt, "na.action") <- attr(re_new, "na.action") <- attr(mfnew,
                                                             "na.action")
  list(Zt = Zt, b = re_new, Lambdat = ReTrms$Lambdat)
}

levelfun <- function(x, nl.n, allow.new.levels = FALSE) {
  if (!all(nl.n %in% rownames(x))) {
    if (!allow.new.levels)
      stop("new levels detected in newdata")
    newx <- as.data.frame(
      matrix(0, nrow = length(nl.n), ncol = ncol(x),
             dimnames = list(nl.n, names(x)))
      )
    newx[rownames(x), ] <- x
    x <- newx
  }
  if (!all(r.inn <- rownames(x) %in% nl.n)) {
    x <- x[r.inn, , drop = FALSE]
  }
  return(x)
}

RHSForm <- function (form, as.form = FALSE) {
  rhsf <- form[[length(form)]]
  if (as.form)
    reformulate(deparse(rhsf))
  else rhsf
}
