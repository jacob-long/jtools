
# Stolen from sjstats so I don't have to list it as import

icc <- function(fit, obj.name) {
  # check if suggested package is available
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # get family
  fitfam <- stats::family(fit)$family
  # is neg. binomial? Dropped sjstats' internal function in favor of regexp
  # is_negbin <- sjmisc::str_contains(fitfam, "Negative Binomial", ignore.case = TRUE)
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
    tau.01 <-
      apply(as.matrix(cbind(unlist(cor_), unlist(std_))), MARGIN = 1, FUN = prod)
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

##########################################################################
# GLMM r-squared -- adapted from piecewiseSEM package
##########################################################################

### piecewisesem implementation
#' @importFrom stats var

pR2_merMod = function(model) {

  ret <- list()

  # Get R2 for class == merMod
  if (any(class(model) %in% c("lmerMod", "merModLmerTest"))) {

    # Get variance of fixed effects by multiplying coefficients by design matrix
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
          lme4::ranef(model), nrow))[!unlist(lapply(lme4::ranef(model), nrow)) ==
                                       nrow(model@pp$X)]
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
    varResid = attr(lme4::VarCorr(model), "sc")^2

    # Calculate R2 values
    ret$Marginal = varF / (varF + varRand + varResid)

    ret$Conditional = (varF + varRand) / (varF + varRand + varResid)

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

        X = model.matrix(model)

        Z = X[, rownames(Sigma), drop = FALSE]

        Z.m = Z %*% Sigma

        sum(diag(crossprod(Z.m, Z))) / nrow(X)

      } )

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

        } )

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

    } else if (ret$Family == "poisson" | grepl("Negative Binomial", ret$Family)) {

      # Generate null model (intercept and random effects only,
      # no fixed effects)
      null.model <-
        update(model, formula = paste(". ~ ", get.random.formula(model, "~1")))

      # Get the fixed effects of the null model
      null.fixef <- as.numeric(lme4::fixef(null.model))

      if (ret$Link == "log") {varDist = log(1 + 1/exp(null.fixef))}

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

  # Return results
  return(ret)

}

get.random.formula = function(model, rhs) {

  if (class(rhs) == "formula") {rhs <- Reduce(paste, deparse(rhs))}

  # Get random formula from model
  random.formula <- if (any(class(model) %in% c("lmerMod", "merModLmerTest",
                                                "glmerMod", "glmmTMB"))) {

        paste("(", lme4::findbars(formula(model)), ")", collapse = " + ")

  }

  # Get random structure(s)
  random.structure <- if (any(class(model) %in% c("lmerMod", "merModLmerTest",
                                                  "glmerMod", "glmmTMB"))) {

      ran.ef.splt = strsplit(random.formula, "\\+.")[[1]]

      sapply(ran.ef.splt[sapply(ran.ef.splt, function(x) grepl("\\|", x))],

             function(x)

               gsub(" ", "", gsub(".*\\|(.*)\\)", "\\1", x))

      )

  }

  random.structure <- unname(random.structure[!duplicated(random.structure)])

  # Get random slopes in the model list, otherwise return vector of terms to drop
  random.slopes <- if (any(class(model) %in% c("glmerMod", "merModLmerTest",
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
    new.random.slopes = 1
  } else {
    new.random.slopes = paste0(new.random.slopes, collapse = " + ")
  }

  # Replace random slopes if any variables in model formula appear in
  # random slopes
  if (length(random.slopes) != 0) {

    if (any(class(model) %in% c("glmerMod", "merModLmerTest", "glmmTMB"))) {

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

### Lifted from lme4 ########################################################

noReForm <- function(re.form) {
  (!is.null(re.form) && !is(re.form, "formula") && is.na(re.form)) ||
  (is(re.form, "formula") && length(re.form) == 2 && identical(re.form[[2]], 0))
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
      stop("NAs are not allowed in prediction data", " for grouping variables unless allow.new.levels is TRUE")
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
