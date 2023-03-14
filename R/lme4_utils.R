# The following function, predict_merMod, is adapted from the predict method
# implemented in the lme4 package. The original code is licensed under the 
# license GPL-2-or-later. 
# (C) 2022 Ben Bolker, Douglas Bates, Martin Maechler, and Steven Walker
# The modified code is released under GPL-3-or-later (C) 2022 Jacob A. Long

#' @title Alternative interface for `merMod` predictions
#' @description This function generates predictions for `merMod` models, but
#'  with the ability to get standard errors as well. 
#' @inheritParams lme4::predict.merMod
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
  if (is.null(newdata) && is.null(re.form)) {
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
    fit.na.action <- NULL # nolint

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

  if (se.fit == TRUE && boot == FALSE) {
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
      drop(X  %*% lme4::fixef(object))
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

### Copied lme4 ###############################################################
## These are internal functions in lme4 that I need for my reimplementations ##
## They are taken near-verbatim from the lme4 package. The code is released
## with the license GPL-2-or-later. 
## (C) 2022 Ben Bolker, Douglas Bates, Martin Maechler, and Steven Walker

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
      stop_wrap("NAs are not allowed in prediction data for grouping variables 
                unless allow.new.levels is TRUE")
    ns.re <- names(re <- lme4::ranef(object))
    nRnms <- names(Rcnms <- ReTrms$cnms)
    if (!all(nRnms %in% ns.re))
      stop_wrap("grouping factors specified in re.form that were not present in
                 original model")
    new_levels <- lapply(ReTrms$flist, function(x) levels(factor(x)))
    re_x <- Map(function(r, n) levelfun(r, n, allow.new.levels = allow.new.levels),
                re[names(new_levels)], new_levels)
    re_new <- lapply(seq_along(nRnms), function(i) {
      rname <- nRnms[i]
      if (!all(Rcnms[[i]] %in% names(re[[rname]])))
        stop_wrap("random effects specified in re.form that were not present in
                   original model")
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
