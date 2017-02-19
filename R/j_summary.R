#' Regression summaries with options
#'
#' \code{j_summ()} prints output for a regression model in a fashion similar to
#' \code{summary()}, but formatted differently with more options.
#'
#' @param lm A \code{lm}, \code{glm}, or \code{\link[survey]{svyglm}} object.
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
#' @param robust.type Only used if \code{robust=TRUE}. Specificies the type of
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
#' fit <- lm(Income ~ `HS Grad` + Illiteracy + Murder, data=as.data.frame(state.x77))
#'
#' # Print the output with standardized coefficients and 2 digits past the decimal
#' j_summ(fit, standardize=TRUE, digits=2)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell*meals,design=dstrat)
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
#' @importFrom stats coef coefficients lm predict sd cooks.distance
#' @export
#'
#'

j_summ <- jsumm <- function(lm, standardize = FALSE, vifs = FALSE, robust = FALSE,
                   robust.type = "HC3", digits = 3, model.info = TRUE,
                   model.fit = TRUE, model.check = FALSE, n.sd = 1,
                   center = FALSE, standardize.response = FALSE) {

  j <- NULL

  # Standardized betas
  if (standardize == TRUE) {
    lm <- scale_lm(lm, n.sd = n.sd, center = TRUE, scale.response = standardize.response)
  } else if (center == TRUE && standardize == FALSE) {
    lm <- center_lm(lm)
  }

  j <- structure(j, standardize = standardize, vifs = vifs, robust = robust,
                        robust.type = robust.type, digits = digits,
                        model.info = model.info, model.fit = model.fit,
                        model.check = model.check, n.sd = n.sd, center = center)

  # Using information from summary()
  sum <- summary(lm)
  if (class(lm)[1] == "lm") {
    bsum <- broom::glance(lm)
  }

  # Check if linear model
  if (class(lm)[1] == "lm") {
    linear <- T
  } else if (lm$family[1] == "gaussian" && lm$family[2] == "identity") {
    linear <- T
  } else {
    linear <- F
  }

  if (!all(attributes(lm$terms)$order > 1)) {
    interaction <- TRUE
  } else {
    interaction <- FALSE
  }

  j <- structure(j, linear = linear)

  # Intercept?
  if (lm$rank != attr(lm$terms, "intercept")) {
    df.int <- if (attr(lm$terms, "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(lm$residuals)
  j <- structure(j, n = n)

  # Calculate R-squared and adjusted R-squared
  ### Below taken from summary.lm
  r <- lm$residuals
  f <- lm$fitted.values
  w <- lm$weights
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
  arsq <- 1 - (1 - rsq) * ((n-df.int)/lm$df.residual)

  # AIC for GLMs
  if (class(lm)[1] != "lm") {
    j <- structure(j, aic = lm$aic)
  }

  # List of names of predictors
  ivs <- names(coefficients(lm))

  # Unstandardized betas
  ucoefs <- unname(coef(lm))

  # Model statistics
  fstat <- unname(sum$fstatistic[1])
  fnum <- unname(sum$fstatistic[2])
  fden <- unname(sum$fstatistic[3])
  j <- structure(j, fstat = fstat, fnum = fnum, fden = fden)

  # VIFs
  if (vifs==T) {
    if (lm$rank==2 | (lm$rank==1 & df.int==0L)) {
      tvifs <- rep(NA, 1)
    } else {
      tvifs <- rep(NA, length(ivs))
      tvifs[-1] <- unname(car::vif(lm))
    }
  }

  # Standard errors and t-statistics
  if (robust == TRUE && linear == TRUE) {

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

    coefs <- sandwich::vcovHC(lm, type=robust.type)
    coefs <- lmtest::coeftest(lm,coefs)
    ses <- coefs[,2]
    ts <- coefs[,3]
    pvals <- coefs[,4]

  } else if (robust == TRUE && linear == FALSE) {
    warning("Heteroskedasticity-robust standard errors should not be used for
            non-linear models. Using the glm object's standard errors instead.")
    ses <- coef(sum)[,2]
    ts <- coef(sum)[,3]
    pvals <- coef(sum)[,4]
  } else {
    ses <- coef(sum)[,2]
    ts <- coef(sum)[,3]
    pvals <- coef(sum)[,4]
  }

  params <- list(ucoefs, ses, ts, pvals)
  namevec <- c("Est.", "S.E.", "t val.", "p")

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

  if (model.check == TRUE && linear == TRUE && class(lm)[1] == "lm") {
    if (!requireNamespace("car", quietly = TRUE)) {
      stop("When model.check is set to TRUE, you need to have the \'car\' package
           for model checking functionality. Please install it or set model.check
           to FALSE.", call. = FALSE)
    }

    homoskedp <- car::ncvTest(lm)$p
    j <- structure(j, homoskedp = homoskedp)

    cd <- table(cooks.distance(lm) > 4/n)
    j <- structure(j, cooksdf = cd[2])

  } else if (model.check == TRUE && linear == FALSE) {
    warning("Model checking for non-linear models is not yet implemented.")
  }

  j <- structure(j, rsq = rsq, arsq = arsq, dv = names(lm$model[1]),
                        npreds = lm$rank-1, lmClass = class(lm))

  if (class(lm)[1]=="lm") {
    j <- structure(j, modpval = bsum$p.value)
  }

  if (class(lm)[1] == "glm" | class(lm)[1] == "svyglm" | class(lm)[1] == "svrepglm") {
    j <- structure(j, lmFamily = lm$family)
  }

  j$coeftable <- mat
  class(j) <- "j_summ"
  return(j)

}


#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.j_summ <- function(x, ...) {

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
    cat("MODEL INFO:", "\n", "Sample Size: ", x$n, "\n", "Dependent Variable: ",
        x$dv, "\n", "Number of Terms: ", (x$npreds), "\n", sep="")
    if (x$lmClass[1]=="svyglm" || x$lmClass[1]=="svrepglm") {
      cat("\n", "Analysis of complex survey design", "\n", sep="")
      if (as.character(x$lmFamily[1])=="gaussian" &&
          as.character(x$lmFamily[2])=="identity") {
        cat("Survey-weighted linear regression", "\n", "\n", sep="")
      } else {
        cat("Error Distribution: ", as.character(x$lmFamily[1]), "\n", "Link function: ",
            as.character(x$lmFamily[2]), "\n", "\n", sep="")
      }
    } else if (x$lmClass[1]=="glm" && (x$lmFamily[1] == "gaussian" &&
                                       x$lmFamily[2] == "identity")) {
      cat("Type: Linear regression", "\n\n")
    } else if (x$lmClass[1]=="glm") {
      cat("Error Distribution: ", as.character(x$lmFamily[1]), "\n", "Link function: ",
          as.character(x$lmFamily[2]), "\n", "\n", sep="")
    } else {
      cat("\n")
    }
  }

  if (x$model.fit==T) {
    if (x$lmClass[1]=="lm") {
      cat("MODEL FIT: ", "\n", "F(", x$fnum, ",", x$fden, ") = ",
          round(x$fstat, digits=x$digits), ", p = ", round(x$modpval, digits=x$digits),
          "\n", "R-squared = ", round(x$rsq, digits=x$digits), "\n",
          "Adj. R-squared = ",
          round(x$arsq, digits=x$digits), "\n", "\n", sep="")
    }

    if (x$lmClass[1] != "lm") {
      cat("MODEL FIT: ", "\n", "Pseudo R-squared = ", round(x$rsq, digits=x$digits),
          "\n", "AIC = ", x$aic, "\n\n", sep="")
    }
  }

  if (x$model.check == TRUE && x$lmClass[1] == "lm") {
    if (x$homoskedp < .05) {
      homoskedtf <- paste("Assumption violated (p = ",
                          round(x$homoskedp,digits=x$digits), ")", sep="")
    } else {
      homoskedtf <- paste("Assumption not violated (p = ",
                          round(x$homoskedp, digits=x$digits), ")", sep="")
    }

    cat("MODEL CHECKING:", "\n", "Homoskedasticity (Breusch-Pagan) = ", homoskedtf,
        "\n", "Number of high-leverage observations = ", x$cooksdf, "\n\n", sep="")
  }

  if (x$linear==T) {
    if (x$lmClass[1] == "svyglm" || x$lmClass[1] == "svrepglm") {
      cat("Robust\n")
    } else if (x$robust==F) {
      cat("Standard errors: OLS", "\n")
    } else if (x$robust==T) {
      cat("Standard errors: Robust, type = ", x$robust.type, "\n", sep="")
    }
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

