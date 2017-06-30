#' Regression summaries with options
#'
#' \code{j_summ()} prints output for a regression model in a fashion similar to
#' \code{summary()}, but formatted differently with more options.
#'
#' @param model A \code{lm}, \code{glm}, or \code{\link[survey]{svyglm}} object.
#' @param standardize If \code{TRUE}, adds a column to output with standardized regression
#'   coefficients. Default is \code{FALSE}.
#' @param vifs If \code{TRUE}, adds a column to output with variance inflation factors
#'   (VIF). Default is \code{FALSE}.
#' @param robust If \code{TRUE}, reports heteroskedasticity-robust standard errors
#'   instead of conventional SEs. These are also known as Huber-White standard
#'   errors.
#'
#'   Default is \code{FALSE}.
#'
#'   This requires the \code{sandwich} and \code{lmtest} packages to compute the
#'    standard errors.
#' @param robust.type Only used if \code{robust=TRUE}. Specifies the type of
#'   robust standard errors to be used by \code{sandwich}. By default, set to \code{"HC3"}
#'   . See details for more on options.
#' @param digits An integer specifying the number of digits past the decimal to report in
#'   the output. Default is 5.
#' @param model.info Toggles printing of basic information on sample size, name of
#'   DV, and number of predictors.
#' @param model.fit Toggles printing of R-squared, Adjusted R-squared, Pseudo-R-squared,
#'  and AIC (when applicable).
#' @param model.check Toggles whether to perform Breusch-Pagan test for heteroskedasticity
#'  and print number of high-leverage observations. See details for more info.
#' @param n.sd If \code{standardize = TRUE}, how many standard deviations should
#'  predictors be divided by? Default is 1, though some suggest 2.
#' @param center If you want coefficients for mean-centered variables but don't
#'  want to standardize, set this to \code{TRUE}.
#' @param standardize.response Should standardization apply to response variable?
#'  Default is \code{FALSE}.
#'
#' @details By default, this function will print the following items to the console:
#' \itemize{
#'   \item The sample size
#'   \item The name of the outcome variable
#'   \item The number of predictors used
#'   \item The (Pseudo-)R-squared value (plus adjusted R-squared if OLS regression).
#'   \item A table with regression coefficients, standard errors, t-values, and
#'    p-values.
#' }
#'
#'  There are several options available for \code{robust.type}. The heavy lifting
#'  is done by \code{\link[sandwich]{vcovHC}}, where those are better described.
#'  Put simply, you may choose from \code{"HC0"} to \code{"HC5"}. Based on the
#'  recommendation of the developers of \pkg{sandwich}, the default is set to
#'  \code{"HC3"}. Stata's default is \code{"HC1"}, so that choice may be better
#'  if the goal is to replicate Stata's output. Any option that is understood by
#'  \code{vcovHC} will be accepted.
#'
#'  The \code{standardize} and \code{center} options are performed via refitting
#'  the model with \code{\link{scale_lm}} and \code{\link{center_lm}},
#'  respectively. Each of those in turn uses \code{\link{gscale}} for the
#'  mean-centering and scaling. These functions can handle \code{svyglm} objects
#'  correctly by calling \code{svymean} and \code{svyvar} to compute means and
#'  standard deviations. Weights are not altered. The fact that the model is
#'  refit means the runtime will be similar to the original time it took to fit
#'  the model.
#'
#'  There are two pieces of information given for \code{model.check}, provided that
#'  the model is an \code{lm} object. First, a Breusch-Pagan test is performed with
#'  \code{\link[car]{ncvTest}}, which requires the \code{car} package. This is a
#'  hypothesis test for which the alternative hypothesis is heteroskedastic errors.
#'  The test becomes much more likely to be statistically significant as the sample
#'  size increases; however, the homoskedasticity assumption becomes less important
#'  to inference as sample size increases (Lumley, Diehr, Emerson, & Lu, 2002).
#'  Take the result of the test as a cue to check graphical checks rather than a
#'  definitive decision. Note that the use of robust standard errors can account
#'  for heteroskedasticity, though some oppose this approach (see King & Roberts,
#'  2015).
#'
#'  The second piece of information provided by setting \code{model.check} to
#'  \code{TRUE} is the number of high leverage observations. There are no hard
#'  and fast rules for determining high leverage either, but in this case it is
#'  based on Cook's Distance. All Cook's Distance values greater than (4/N) are
#'  included in the count. Again, this is not a recommendation to locate and
#'  remove such observations, but rather to look more closely with graphical and
#'  other methods.
#'
#' @return If saved, users can access most of the items that are returned in the
#'   output (and without rounding).
#'
#'  \item{coeftable}{The outputted table of variables and coefficients}
#'  \item{model}{The model for which statistics are displayed. This would be
#'    most useful in cases in which \code{standardize = TRUE}.}
#'
#'  Much other information can be accessed as attributes.
#'
#' @seealso \code{\link{scale_lm}} can simply perform the standardization if
#'  preferred.
#'
#'  \code{\link{gscale}} does the heavy lifting for mean-centering and scaling
#'  behind the scenes.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @examples
#' # Create lm object
#' fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))
#'
#' # Print the output with standardized coefficients and 2 digits past the decimal
#' j_summ(fit, standardize = TRUE, digits = 2)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata =~ stype, weights =~ pw, data = apistrat,
#'  fpc =~ fpc)
#' regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#' j_summ(regmodel)
#'
#' @references
#'
#' King, G., & Roberts, M. E. (2015). How robust standard errors expose methodological
#'  problems they do not fix, and what to do about it. \emph{Political Analysis},
#'   \emph{23}(2), 159–179. \url{https://doi.org/10.1093/pan/mpu015}
#'
#' Lumley, T., Diehr, P., Emerson, S., & Chen, L. (2002). The Importance of the
#' Normality Assumption in Large Public Health Data Sets. \emph{Annual Review of
#'  Public Health}, \emph{23}, 151–169.
#'  \url{https://doi.org/10.1146/annurev.publhealth.23.100901.140546}
#'
#'
#'
#' @importFrom stats coef coefficients lm predict sd cooks.distance pf logLik
#'  extractAIC family fitted pt residuals terms
#' @export
#'
#'

j_summ <- function(model, standardize = FALSE, vifs = FALSE, robust = FALSE,
                   robust.type = "HC3", digits = 3, model.info = TRUE,
                   model.fit = TRUE, model.check = FALSE, n.sd = 1,
                   center = FALSE, standardize.response = FALSE) {
  UseMethod("j_summ")
}


#' @rdname j_summ
#' @export

j_summ.lm <- function(model, standardize = FALSE, vifs = FALSE, robust = FALSE,
                      robust.type = "HC3", digits = 3, model.info = TRUE,
                      model.fit = TRUE, model.check = FALSE, n.sd = 1,
                      center = FALSE, standardize.response = FALSE) {

  j <- list()

  # Standardized betas
  if (standardize == TRUE) {
    model <- scale_lm(model, n.sd = n.sd, center = TRUE, scale.response = standardize.response)
  } else if (center == TRUE && standardize == FALSE) {
    model <- center_lm(model)
  }

  j <- structure(j, standardize = standardize, vifs = vifs, robust = robust,
                        robust.type = robust.type, digits = digits,
                        model.info = model.info, model.fit = model.fit,
                        model.check = model.check, n.sd = n.sd, center = center)

  # Using information from summary()
  sum <- summary(model)

  if (!all(attributes(model$terms)$order > 1)) {
    interaction <- TRUE
  } else {
    interaction <- FALSE
  }

  # Intercept?
  if (model$rank != attr(model$terms, "intercept")) {
    df.int <- if (attr(model$terms, "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(model$residuals)
  j <- structure(j, n = n)

  # Get R-squared and adjusted R-squared
  rsq <- unname(sum$r.squared)
  arsq <- unname(sum$adj.r.squared)

  # List of names of predictors
  ivs <- names(coefficients(model))

  # Unstandardized betas
  ucoefs <- unname(coef(model))

  # Model statistics
  fstat <- unname(sum$fstatistic[1])
  fnum <- unname(sum$fstatistic[2])
  fden <- unname(sum$fstatistic[3])
  j <- structure(j, fstat = fstat, fnum = fnum, fden = fden)

  # VIFs
  if (vifs==T) {
    if (model$rank==2 | (model$rank==1 & df.int==0L)) {
      tvifs <- rep(NA, 1)
    } else {
      tvifs <- rep(NA, length(ivs))
      tvifs[-1] <- unname(car::vif(model))
    }
  }

  # Standard errors and t-statistics
  if (robust == TRUE) {

    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("When robust is set to TRUE, you need to have the \'sandwich\' package
           for robust standard errors. Please install it or set robust to FALSE.",
           call. = FALSE)
    }

    if (!requireNamespace("lmtest", quietly = TRUE)) {
      stop("When robust is set to TRUE, you need to have the \'lmtest\' package
           for robust standard errors. Please install it or set robust to FALSE.",
           call. = FALSE)
    }

    coefs <- sandwich::vcovHC(model, type=robust.type)
    coefs <- lmtest::coeftest(model,coefs)
    ses <- coefs[,2]
    ts <- coefs[,3]
    pvals <- coefs[,4]
  } else {
    ses <- coef(sum)[,2]
    ts <- coef(sum)[,3]
    pvals <- coef(sum)[,4]
  }

  params <- list(ucoefs, ses, ts, pvals)
  namevec <- c("Est.", "S.E.", "t val.", "p")

  if (vifs == TRUE) {
    params[length(params)+1] <- list(tvifs)
    namevec <- c(namevec, "VIF")
  }

  mat <- matrix(nrow=length(ivs), ncol=length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in 1:length(params)) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  # Implement model checking features
  if (model.check == TRUE) {
    if (!requireNamespace("car", quietly = TRUE)) {
      stop("When model.check is set to TRUE, you need to have the \'car\' package
           for model checking functionality. Please install it or set model.check
           to FALSE.", call. = FALSE)
    }

    homoskedp <- car::ncvTest(model)$p
    j <- structure(j, homoskedp = homoskedp)

    cd <- table(cooks.distance(model) > 4/n)
    j <- structure(j, cooksdf = cd[2])

  }

  j <- structure(j, rsq = rsq, arsq = arsq, dv = names(model$model[1]),
                        npreds = model$rank-df.int, lmClass = class(model))

  modpval <- pf(fstat, fnum, fden, lower.tail = FALSE)
  j <- structure(j, modpval = modpval)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("j_summ.lm", "j_summ")
  return(j)

}

#' @rdname j_summ
#' @export

j_summ.glm <- function(model, standardize = FALSE, vifs = FALSE, robust = FALSE,
                       robust.type = "HC3", digits = 3, model.info = TRUE,
                       model.fit = TRUE, model.check = FALSE, n.sd = 1,
                       center = FALSE, standardize.response = FALSE) {

  j <- list()

  if (robust == TRUE) {
    warning("Robust standard errors are not supported for glm models.")
    robust <- FALSE
  }

  # Standardized betas
  if (standardize == TRUE) {
    # Save data --- using the call to access the data to avoid problems w/
    # transformed data
    call <- getCall(model)
    if (!is.null(call$data)) {
      d <- eval(call$data)
      mframe <- FALSE # telling myself whether I use model.frame
    } else {
      d <- model.frame(model)
      mframe <- TRUE
    }
    form <- update(formula(model),
                   as.formula(formula(model)))

    # Save response variable
    resp <- as.character(formula(model)[2])

    # Save list of terms
    vars <- attributes(model$terms)$variables
    vars <- as.character(vars)[2:length(vars)] # Use 2:length bc 1 is "list"

    # calling gscale(), incorporating the weights
    if (standardize.response == FALSE) {
      # Now we need to know the variables of interest
      vars <- vars[!(vars %in% resp)]
      d <- gscale(x = vars, data = d, n.sd = n.sd)
    } else {
      d <- gscale(x = vars, data = d, n.sd = n.sd)
    }
    model <- update(model, formula = form, data = d)
  } else if (center == TRUE && standardize == FALSE) {
    call <- getCall(model)
    if (!is.null(call$data)) {
      d <- eval(call$data)
      mframe <- FALSE # telling myself whether I use model.frame
    } else {
      d <- model.frame(model)
      mframe <- TRUE
    }
    form <- update(formula(model),
                   as.formula(formula(model)))

    # Save response variable
    resp <- as.character(formula(model)[2])

    # Save list of terms
    vars <- attributes(model$terms)$variables
    vars <- as.character(vars)[2:length(vars)] # Use 2:length bc 1 is "list"

    # calling gscale(), incorporating the weights
    if (standardize.response == FALSE) {
      # Now we need to know the variables of interest
      vars <- vars[!(vars %in% resp)]
      d <- gscale(x = vars, data = d, n.sd = n.sd, center.only = TRUE)
    } else {
      d <- gscale(x = vars, data = d, n.sd = n.sd, center.only = TRUE)
    }
    model <- update(model, formula = form, data = d)
  }


  j <- structure(j, standardize = standardize, vifs = vifs, robust = robust,
                 robust.type = robust.type, digits = digits,
                 model.info = model.info, model.fit = model.fit, n.sd = n.sd,
                 center = center)

  # Using information from summary()
  sum <- summary(model)

  if (!all(attributes(model$terms)$order > 1)) {
    interaction <- TRUE
  } else {
    interaction <- FALSE
  }

  # Intercept?
  if (model$rank != attr(model$terms, "intercept")) {
    df.int <- if (attr(model$terms, "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(model$residuals)
  j <- structure(j, n = n)

  # Calculate R-squared
  ### Below taken from summary.lm
  r <- model$residuals
  f <- model$fitted.values
  w <- model$weights
  ## Dealing with no-intercept models, getting the df.int
  if (is.null(w)) {
    mss <- if (df.int == 1L)
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  } else {
    mss <- if (df.int == 1L) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    } else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }

  ## Namespace issues require me to define pR2 here
  pR2 <- function(object) {
    llh <- suppressWarnings(logLik(object))
    objectNull <- suppressWarnings(update(object, ~ 1))
    llhNull <- logLik(objectNull)
    n <- dim(object$model)[1]
    pR2Work(llh,llhNull,n)
  }

  # Final calculations (linear pseudo-rsq)
  ## Cragg-Uhler
  rsq <- pR2(model)$r2CU
  ## McFadden
  rsqmc <- pR2(model)$McFadden

  # AIC for GLMs
  j <- structure(j, aic = model$aic)

  # List of names of predictors
  ivs <- names(coefficients(model))

  # Unstandardized betas
  ucoefs <- unname(coef(model))

  # VIFs
  if (vifs==T) {
    if (model$rank==2 | (model$rank==1 & df.int==0L)) {
      tvifs <- rep(NA, 1)
    } else {
      tvifs <- rep(NA, length(ivs))
      tvifs[-1] <- unname(car::vif(model))
    }
  }

  # Standard errors and t-statistics
  ses <- coef(sum)[,2]
  ts <- coef(sum)[,3]
  pvals <- coef(sum)[,4]

  # Need proper name for test statistic
  tcol <- colnames(coef(sum))[3]
  tcol <- gsub("value", "val.", tcol)

  # Put things together
  params <- list(ucoefs, ses, ts, pvals)
  namevec <- c("Est.", "S.E.", tcol, "p")

  # Calculate vifs
  if (vifs==T) {
    params[length(params)+1] <- list(tvifs)
    namevec <- c(namevec, "VIF")
  }

  mat <- matrix(nrow=length(ivs), ncol=length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in 1:length(params)) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  j <- structure(j, rsq = rsq, rsqmc = rsqmc, dv = names(model$model[1]),
                 npreds = model$rank-df.int)

  j <- structure(j, lmFamily = model$family)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("j_summ.glm", "j_summ")
  return(j)

}

#' @rdname j_summ
#' @export

j_summ.svyglm <- function(model, standardize = FALSE, vifs = FALSE, robust = FALSE,
                          robust.type = "HC3", digits = 3, model.info = TRUE,
                          model.fit = TRUE, model.check = FALSE, n.sd = 1,
                          center = FALSE, standardize.response = FALSE) {

  j <- list()

  if (robust == TRUE) {
    warning("The survey package always reports robust standard errors by default.")
    robust <- FALSE
  }

  # Standardized betas
  if (standardize == TRUE || center == TRUE) {
    # Save data --- using the call to access the data to avoid problems w/
    # transformed data
    call <- getCall(model)
    if (!is.null(call$data)) {
      d <- eval(call$data)
      mframe <- FALSE # telling myself whether I use model.frame
    } else {
      d <- model.frame(model)
      mframe <- TRUE
    }

    form <- update(formula(model), as.formula(formula(model)))
    call$formula <- as.call(as.formula(call$formula))

    # Save response variable
    resp <- as.character(formula(model)[2])

    # Save list of terms
    vars <- attributes(model$terms)$variables
    vars <- as.character(vars)[2:length(vars)] # Use 2:length bc 1 is "list"

    # Get the survey design object
    design <- model$survey.design

    # Now we need to know the variables of interest
    vars <- attributes(model$terms)$variables
    vars <- as.character(vars)[2:length(vars)]

    if (standardize.response == FALSE) {
      vars <- vars[!(vars %in% resp)]
    }

    # Call gscale()
    if (standardize == TRUE) {
      design <- gscale(x = vars, data = design, n.sd = n.sd)
    } else if (standardize == FALSE && center == TRUE) {
      design <- gscale(x = vars, data = design, n.sd = n.sd,
                       center.only = TRUE)
    }

    # this weirdness is all to deal with namespace issues with update
    call$design <- design
    call[[1]] <- survey::svyglm
    model <- eval(call)
  } else {
    design <- model$survey.design
  }

  j <- structure(j, standardize = standardize, vifs = vifs, robust = robust,
                 robust.type = robust.type, digits = digits,
                 model.info = model.info, model.fit = model.fit,
                 model.check = model.check, n.sd = n.sd, center = center)

  # Using information from summary()
  sum <- summary(model)

  # Check if linear model
  if (model$family[1] == "gaussian" && model$family[2] == "identity") {
    linear <- TRUE
  } else {
    linear <- FALSE
  }

  if (!all(attributes(model$terms)$order > 1)) {
    interaction <- TRUE
  } else {
    interaction <- FALSE
  }

  j <- structure(j, linear = linear)

  # Intercept?
  if (model$rank != attr(model$terms, "intercept")) {
    df.int <- if (attr(model$terms, "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(model$residuals)
  j <- structure(j, n = n)

  if (linear == TRUE) { # If it's linear, treat it like lm
    # Calculate R-squared and adjusted R-squared
    ### Below taken from summary.lm
    r <- model$residuals
    f <- model$fitted.values
    w <- model$weights
    ## Dealing with no-intercept models, getting the df.int
    if (is.null(w)) {
      mss <- if (df.int == 1L)
        sum((f - mean(f))^2)
      else sum(f^2)
      rss <- sum(r^2)
    } else {
      mss <- if (df.int == 1L) {
        m <- sum(w * f/sum(w))
        sum(w * (f - m)^2)
      } else sum(w * f^2)
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }

    ## Final calculations
    rsq <- mss/(mss+rss)
    arsq <- 1 - (1 - rsq) * ((n-df.int)/model$df.residual)

    j <- structure(j, rsq = rsq, arsq = arsq)
  } else { # If not linear, calculate pseudo-rsq

    ## Have to specify pR2 here to fix namespace issues
    pR2 <- function(object) {
      llh <- suppressWarnings(logLik(object))
      objectNull <- suppressWarnings(update(object, ~ 1,
                                            design = object$survey.design))

      llhNull <- logLik(objectNull)
      n <- dim(object$model)[1]
      pR2Work(llh,llhNull,n)
    }

    # Final calculations (linear pseudo-rsq)
    ## Cragg-Uhler
    rsq <- suppressWarnings(pR2(model)$r2CU)
    ## McFadden
    rsqmc <- suppressWarnings(pR2(model)$McFadden)

    j <- structure(j, rsq = rsq, rsqmc = rsqmc)

    # AIC for GLMs
    j <- structure(j, aic = model$aic)
  }

  # List of names of predictors
  ivs <- names(coefficients(model))

  # Unstandardized betas
  ucoefs <- unname(coef(model))

  # VIFs
  if (vifs==T) {
    if (model$rank==2 | (model$rank==1 & df.int==0L)) {
      tvifs <- rep(NA, 1)
    } else {
      tvifs <- rep(NA, length(ivs))
      tvifs[-1] <- unname(car::vif(model))
    }
  }

  # Standard errors and t-statistics
  ses <- coef(sum)[,2]
  ts <- coef(sum)[,3]
  pvals <- coef(sum)[,4]

  # Need proper name for test statistic
  tcol <- colnames(coef(sum))[3]
  tcol <- gsub("value", "val.", tcol)

  # Put things together
  params <- list(ucoefs, ses, ts, pvals)
  namevec <- c("Est.", "S.E.", tcol, "p")

  if (vifs == TRUE) {
    params[length(params)+1] <- list(tvifs)
    namevec <- c(namevec, "VIF")
  }

  mat <- matrix(nrow=length(ivs), ncol=length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in 1:length(params)) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  if (model.check == TRUE && linear == TRUE) {
    cd <- table(cooks.distance(model) > 4/n)
    j <- structure(j, cooksdf = cd[2])

    if (model.check == TRUE && linear == FALSE) {
      warning("Model checking for non-linear models is not yet implemented.")
    }
  }

  j <- structure(j, dv = names(model$model[1]), npreds = model$rank-df.int)

  j <- structure(j, lmFamily = model$family)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("j_summ.svyglm", "j_summ")
  return(j)

}

#' @rdname j_summ
#' @export

j_summ.merMod <- function(model, standardize = FALSE, vifs = FALSE,
                          robust = FALSE, robust.type = "HC3", digits = 3,
                          model.info = TRUE, model.fit = FALSE,
                          model.check = FALSE, n.sd = 1, center = FALSE,
                          standardize.response = FALSE) {

  j <- list()

  if (robust == TRUE) {
    warning("Robust standard errors are not supported for merMod models.")
    robust <- FALSE
  }

  # Standardized betas
  if (standardize == TRUE) {
    # Save data --- using the call to access the data to avoid problems w/
    # transformed data
    call <- getCall(model)
    if (!is.null(call$data)) {
      d <- eval(call$data)
      mframe <- FALSE # telling myself whether I use model.frame
    } else {
      d <- model.frame(model)
      mframe <- TRUE
    }
    form <- update(formula(model),
                   as.formula(formula(model)))

    # Save response variable
    resp <- as.character(formula(model)[2])

    # Save list of terms
    vars <- attributes(terms(model))$variables
    vars <- as.character(vars)[2:length(vars)] # Use 2:length bc 1 is "list"

    # calling gscale(), incorporating the weights
    if (standardize.response == FALSE) {
      # Now we need to know the variables of interest
      vars <- vars[!(vars %in% resp)]
      d <- gscale(x = vars, data = d, n.sd = n.sd)
    } else {
      d <- gscale(x = vars, data = d, n.sd = n.sd)
    }
    model <- update(model, formula = form, data = d)
  } else if (center == TRUE && standardize == FALSE) {
    call <- getCall(model)
    if (!is.null(call$data)) {
      d <- eval(call$data)
      mframe <- FALSE # telling myself whether I use model.frame
    } else {
      d <- model.frame(model)
      mframe <- TRUE
    }
    form <- update(formula(model),
                   as.formula(formula(model)))

    # Save response variable
    resp <- as.character(formula(model)[2])

    # Save list of terms
    vars <- attributes(terms(model))$variables
    vars <- as.character(vars)[2:length(vars)] # Use 2:length bc 1 is "list"

    # calling gscale(), incorporating the weights
    if (standardize.response == FALSE) {
      # Now we need to know the variables of interest
      vars <- vars[!(vars %in% resp)]
      d <- gscale(x = vars, data = d, n.sd = n.sd, center.only = TRUE)
    } else {
      d <- gscale(x = vars, data = d, n.sd = n.sd, center.only = TRUE)
    }
    model <- update(model, formula = form, data = d)
  }


  j <- structure(j, standardize = standardize, vifs = vifs, robust = robust,
                 robust.type = robust.type, digits = digits,
                 model.info = model.info, model.fit = model.fit, n.sd = n.sd,
                 center = center)

  # Using information from summary()
  sum <- summary(model)

  if (!all(attributes(terms(model))$order > 1)) {
    interaction <- TRUE
  } else {
    interaction <- FALSE
  }

  # Intercept?
  if (length(terms(model)) != attr(terms(model), "intercept")) {
    df.int <- if (attr(terms(model), "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(residuals(model))
  j <- structure(j, n = n)

  # Calculate R-squared
  ### Below taken from summary.lm
  r <- residuals(model)
  f <- fitted(model)
  w <- weights(model)
  ## Dealing with no-intercept models, getting the df.int
  if (is.null(w)) {
    mss <- if (df.int == 1L)
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  } else {
    mss <- if (df.int == 1L) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    } else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }

  # TODO: Figure out model fit indices for MLMs
  # Final calculations (linear pseudo-rsq)
  ## Need to define this here because of glm's weird namespace behavior
  # pR2 <- function(object){
  #   llh <- suppressWarnings(logLik(object))
  #   objectNull <- suppressWarnings(update(object, ~ 1))
  #   llhNull <- logLik(objectNull)
  #   n <- dim(model.frame(model))[1]
  #   pR2Work(llh,llhNull,n)
  # }
  # ## Cragg-Uhler
  # rsq <- pR2(model)$r2CU
  # ## McFadden
  # rsqmc <- pR2(model)$McFadden

  # AIC for GLMs
  j <- structure(j, aic = extractAIC(model))

  # List of names of predictors
  ivs <- rownames(sum$coefficients)

  # Unstandardized betas
  ucoefs <- unname(sum$coefficients[,1])

  # VIFs
  if (vifs==T) {
    warning("VIFs not supported for merMod objects.")
  }

  # Standard errors and t-statistics
  ses <- sum$coefficients[,2]
  ts <- sum$coefficients[,3]

  # lmerMod doesn't have p-values, so
  if (!sum$isLmer) {
    pvals <- sum$coefficients[,4]
  } else {
    df <- n - length(ivs) - 1
    vec <- rep(NA, times = length(ts))
    for (i in 1:length(ts)) {
      p <- pt(abs(ts[i]), lower.tail = F, df)
      p <- p*2
      vec[i] <- p
    }
    pvals <- vec
  }

  # Need proper name for test statistic
  tcol <- colnames(sum$coefficients)[3]
  tcol <- gsub("value", "val.", tcol)

  # Put things together
  params <- list(ucoefs, ses, ts, pvals)
  namevec <- c("Est.", "S.E.", tcol, "p")

  mat <- matrix(nrow=length(ivs), ncol=length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in 1:length(params)) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  # Dealing with random effects
  ## Names of grouping vars
  groups <- names(sum$ngrps)
  ## Number of groups
  ngroups <- sum$ngrps
  ## Calculate ICCs w/ internal function from sjstats
  iccs <- icc(model)

  ## Make a table summarizing grouping vars
  gvmat <- matrix(ncol = 3, nrow = length(ngroups))
  colnames(gvmat) <- c("Group","# groups","ICC")
  for (i in 1:length(ngroups)) {
    gvmat[i,1] <- groups[i]
    gvmat[i,2] <- ngroups[i]
    gvmat[i,3] <- iccs[i]
  }

  ## Make table explaining random coefs
  rcmat <- as.data.frame(lme4::VarCorr(model))
  rcmat <- rcmat[is.na(rcmat$var2),]
  rcmat <- rcmat[,names(rcmat) %in% c("grp","var1","sdcor")]
  rcmat <- as.matrix(rcmat)
  colnames(rcmat) <- c("Group","Parameter","Std.Dev.")


  j <- structure(j, groups = groups, ngroups = ngroups, iccs = iccs,
                 vcnames = names(iccs), dv = names(model.frame(model)[1]),
                 npreds = nrow(mat))

  j <- structure(j, lmFamily = family(model))

  j$coeftable <- mat
  j$rcoeftable <- rcmat # Random effects table
  j$gvars <- gvmat # Grouping variables table
  j$model <- model
  class(j) <- c("j_summ.merMod", "j_summ")
  return(j)

}

#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.j_summ.lm <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Saving number of columns in output table
  width <- dim(j$coeftable)[2]
  # Saving number of coefficients in output table
  height <- dim(j$coeftable)[1]
  # Saving table to separate object
  ctable <- round(j$coeftable, x$digits)

  # Need to squeeze sigstars between p-vals and VIFs (if VIFs present)
  if (x$vifs) {
    vifvec <- ctable[,width]
    ctable <- ctable[,1:width-1]
    width <- width - 1
  }

  # Making a vector of p-value significance indicators
  sigstars <- c()
  for (y in 1:height) {
    if (ctable[y,width] > 0.1) {
      sigstars[y] <- ""
    } else if (ctable[y,width] <= 0.1 & ctable[y,width] > 0.05) {
      sigstars[y] <- "."
    } else if (ctable[y,width] > 0.01 & ctable[y,width] <= 0.05) {
      sigstars[y] <- "*"
    } else if (ctable[y,width] > 0.001 & ctable[y,width] <= 0.01) {
      sigstars[y] <- "**"
    } else if (ctable[y,width] <= 0.001) {
      sigstars[y] <- "***"
    }
  }

  onames <- colnames(ctable)
  if (x$vifs) {
    ctable <- cbind(ctable, sigstars, vifvec)
    colnames(ctable) <- c(onames, "", "VIF")
  } else {
    ctable <- cbind(ctable, sigstars)
    colnames(ctable) <- c(onames, "")
  }

  if (x$model.info == TRUE) {
    cat("MODEL INFO:", "\n", "Sample Size: ", x$n, "\n",
        "Dependent Variable: ",
        x$dv, "\n", "Number of Terms: ", (x$npreds), "\n", sep="")
    cat("\n")
    }

  if (x$model.fit==T) {
    cat("MODEL FIT: ", "\n", "F(", x$fnum, ",", x$fden, ") = ",
        round(x$fstat, digits=x$digits), ", p = ",
        round(x$modpval, digits=x$digits),
        "\n", "R-squared = ", round(x$rsq, digits=x$digits), "\n",
        "Adj. R-squared = ",
        round(x$arsq, digits=x$digits), "\n", "\n", sep="")
  }

  if (x$model.check == TRUE) {
    # Since it's lm, we can do Breusch-Pagan test
    if (x$homoskedp < .05) {
      homoskedtf <- paste("Assumption violated (p = ",
                          round(x$homoskedp,digits=x$digits), ")", sep="")
    } else {
      homoskedtf <- paste("Assumption not violated (p = ",
                          round(x$homoskedp, digits=x$digits), ")", sep="")
    }
    cat("MODEL CHECKING:", "\n", "Homoskedasticity (Breusch-Pagan) = ",
        homoskedtf,
        "\n", "Number of high-leverage observations = ", x$cooksdf,
        "\n\n", sep="")
  }

  if (x$robust == FALSE) {
    cat("Standard errors: OLS", "\n")
  } else if (x$robust == TRUE) {
    cat("Standard errors: Robust, type = ", x$robust.type, "\n", sep="")
   }

  print(as.table(ctable))

  # Notifying user if variables altered from original fit
  if (x$standardize == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered and scaled by",
        x$n.sd, "s.d.", "\n")
  } else if (x$center == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered.")
  }
  cat("\n")

}

#' @export

print.j_summ.glm <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Saving number of columns in output table
  width <- dim(j$coeftable)[2]
  # Saving number of coefficients in output table
  height <- dim(j$coeftable)[1]
  # Saving table to separate object
  ctable <- round(j$coeftable, x$digits)

  # Need to squeeze sigstars between p-vals and VIFs (if VIFs present)
  if (x$vifs) {
    vifvec <- ctable[,width]
    ctable <- ctable[,1:width-1]
    width <- width - 1
  }

  # Making a vector of p-value significance indicators
  sigstars <- c()
  for (y in 1:height) {
    if (ctable[y,width] > 0.1) {
      sigstars[y] <- ""
    } else if (ctable[y,width] <= 0.1 & ctable[y,width] > 0.05) {
      sigstars[y] <- "."
    } else if (ctable[y,width] > 0.01 & ctable[y,width] <= 0.05) {
      sigstars[y] <- "*"
    } else if (ctable[y,width] > 0.001 & ctable[y,width] <= 0.01) {
      sigstars[y] <- "**"
    } else if (ctable[y,width] <= 0.001) {
      sigstars[y] <- "***"
    }
  }

  onames <- colnames(ctable)
  if (x$vifs) {
    ctable <- cbind(ctable, sigstars, vifvec)
    colnames(ctable) <- c(onames, "", "VIF")
  } else {
    ctable <- cbind(ctable, sigstars)
    colnames(ctable) <- c(onames, "")
  }

  if (x$model.info == TRUE) {
    cat("MODEL INFO:", "\n", "Sample Size: ", x$n, "\n",
        "Dependent Variable: ",
        x$dv, "\n", "Number of Terms: ", (x$npreds), "\n", sep="")
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      cat("Type: Linear regression", "\n\n")
    } else {
      cat("Error Distribution: ", as.character(x$lmFamily[1]), "\n",
          "Link function: ",
          as.character(x$lmFamily[2]), "\n", "\n", sep="")
    }
  }

  if (x$model.fit==T) {
    cat("MODEL FIT: ", "\n", "Pseudo R-squared (Cragg-Uhler) = ",
        round(x$rsq, digits=x$digits), "\n",
        "Pseudo R-squared (McFadden) = ",
        round(x$rsqmc, digits=x$digits),
        "\n", "AIC = ", x$aic, "\n\n", sep="")
  }

  print(as.table(ctable))

  # Notifying user if variables altered from original fit
  if (x$standardize == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered and scaled by",
        x$n.sd, "s.d.", "\n")
  } else if (x$center == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered.")
  }
  cat("\n")

}

#' @export

print.j_summ.svyglm <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Saving number of columns in output table
  width <- dim(j$coeftable)[2]
  # Saving number of coefficients in output table
  height <- dim(j$coeftable)[1]
  # Saving table to separate object
  ctable <- round(j$coeftable, x$digits)

  # Need to squeeze sigstars between p-vals and VIFs (if VIFs present)
  if (x$vifs) {
    vifvec <- ctable[,width]
    ctable <- ctable[,1:width-1]
    width <- width - 1
  }

  # Making a vector of p-value significance indicators
  sigstars <- c()
  for (y in 1:height) {
    if (ctable[y,width] > 0.1) {
      sigstars[y] <- ""
    } else if (ctable[y,width] <= 0.1 & ctable[y,width] > 0.05) {
      sigstars[y] <- "."
    } else if (ctable[y,width] > 0.01 & ctable[y,width] <= 0.05) {
      sigstars[y] <- "*"
    } else if (ctable[y,width] > 0.001 & ctable[y,width] <= 0.01) {
      sigstars[y] <- "**"
    } else if (ctable[y,width] <= 0.001) {
      sigstars[y] <- "***"
    }
  }

  onames <- colnames(ctable)
  if (x$vifs) {
    ctable <- cbind(ctable, sigstars, vifvec)
    colnames(ctable) <- c(onames, "", "VIF")
  } else {
    ctable <- cbind(ctable, sigstars)
    colnames(ctable) <- c(onames, "")
  }

  if (x$model.info == TRUE) {
    # Always showing this
    cat("MODEL INFO:", "\n", "Sample Size: ", x$n, "\n", "Dependent Variable: ",
        x$dv, "\n", "Number of Terms: ", (x$npreds), "\n", sep = "")
    cat("\n", "Analysis of complex survey design", "\n", sep = "")
    # If it's linear...
    if (as.character(x$lmFamily[1]) == "gaussian" &&
        as.character(x$lmFamily[2]) == "identity") {
      # Just call it linear
      cat("Survey-weighted linear regression", "\n", "\n", sep = "")
    } else {
      # Otherwise just treat it like glm
      cat("Error Distribution: ", as.character(x$lmFamily[1]), "\n",
          "Link function: ", as.character(x$lmFamily[2]), "\n", "\n", sep = "")
    }
  }

  if (x$model.fit == TRUE) { # Show fit statistics
    if (as.character(x$lmFamily[1]) == "gaussian" &&
        as.character(x$lmFamily[2]) == "identity") {
      # If it's a linear model, show regular lm fit stats
      cat("MODEL FIT: ", "\n", "R-squared = ", round(x$rsq, digits = x$digits),
          "\n", "Adj. R-squared = ", round(x$arsq, digits = x$digits), "\n",
          "\n", sep = "")
    } else {
      # If it isn't linear, show GLM fit stats
      cat("MODEL FIT: ", "\n", "Pseudo R-squared (Cragg-Uhler) = ",
          round(x$rsq, digits = x$digits), "\n",
          "Pseudo R-squared (McFadden) = ",
          round(x$rsqmc, digits = x$digits),
          "\n", "AIC = ", x$aic, "\n\n", sep = "")
    }
  }

  if (x$model.check == TRUE && x$linear == TRUE) {
    # Just check outliers
    cat("MODEL CHECKING:", "\n", "Number of high-leverage observations = ",
        x$cooksdf, "\n\n", sep = "")
  }

  if (x$linear == TRUE) {
    cat("Standard errors: Robust\n")
  }

  print(as.table(ctable))

  # Notifying user if variables altered from original fit
  if (x$standardize == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered and scaled by",
        x$n.sd, "s.d.", "\n")
  } else if (x$center == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered.")
  }
  cat("\n")

}

#' @export

print.j_summ.merMod <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Saving number of columns in output table
  width <- dim(j$coeftable)[2]
  # Saving number of coefficients in output table
  height <- dim(j$coeftable)[1]
  # Saving table to separate object
  ctable <- round(j$coeftable, x$digits)

  # Making a vector of p-value significance indicators
  sigstars <- c()
  for (y in 1:height) {
    if (ctable[y,width] > 0.1) {
      sigstars[y] <- ""
    } else if (ctable[y,width] <= 0.1 & ctable[y,width] > 0.05) {
      sigstars[y] <- "."
    } else if (ctable[y,width] > 0.01 & ctable[y,width] <= 0.05) {
      sigstars[y] <- "*"
    } else if (ctable[y,width] > 0.001 & ctable[y,width] <= 0.01) {
      sigstars[y] <- "**"
    } else if (ctable[y,width] <= 0.001) {
      sigstars[y] <- "***"
    }
  }

  onames <- colnames(ctable)
  ctable <- cbind(ctable, sigstars)
  colnames(ctable) <- c(onames, "")

  if (x$model.info == TRUE) {
    cat("MODEL INFO:", "\n", "Sample Size: ", x$n, "\n",
        "Dependent Variable: ",
        x$dv, "\n", "Number of Terms: ", (x$npreds), "\n", sep="")
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      cat("Type: Mixed effects linear regression", "\n\n")
    } else {
      cat("\nType: Mixed effects generalized linear regression", "\n",
          "Error Distribution: ", as.character(x$lmFamily[1]), "\n",
          "Link function: ",
          as.character(x$lmFamily[2]), "\n", "\n", sep="")
    }
  }

  if (x$model.fit==T) {
    cat("MODEL FIT: ",
        "\n", "AIC = ", x$aic, "\n\n", sep="")
  }

  cat("FIXED EFFECTS:\n")
  print(as.table(ctable))

  cat("\nRANDOM EFFECTS:\n")
  rtable <- j$rcoeftable
  rtable[,3] <- round(as.numeric(rtable[,3]), digits = x$digits)
  rtable <- as.table(rtable)
  rownames(rtable) <- rep("", times = nrow(rtable))
  print(rtable)

  cat("\nGrouping variables:\n")
  gtable <- j$gvars
  gtable[,2:3] <- round(as.numeric(gtable[,2:3]), digits = x$digits)
  gtable <- as.table(gtable)
  rownames(gtable) <- rep("", times = nrow(gtable))
  print(gtable)

  # Notifying user if variables altered from original fit
  if (x$standardize == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered and scaled by",
        x$n.sd, "s.d.", "\n")
  } else if (x$center == TRUE) {
    cat("\n")
    cat("All continuous variables are mean-centered.")
  }
  cat("\n")

}
