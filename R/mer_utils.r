
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

  if (inherits(rhs, "formula")) {rhs <- Reduce(paste, deparse(rhs))}

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
