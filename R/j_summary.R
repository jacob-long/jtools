#' Regression summaries with options
#'
#' \code{j_summ()} prints output for a regression model in a fashion similar to
#' \code{summary()}, but formatted differently with more options.
#'
#' @param lm A \code{lm}, \code{glm}, or \code{\link[survey]{svyglm}} object.
#' @param stdbeta If \code{TRUE}, adds a column to output with standardized regression
#'   coefficients. Default is \code{FALSE}.
#' @param vifs If \code{TRUE}, adds a column to output with variance inflation factors
#'   (VIF). Default is \code{FALSE}.
#' @param robust If \code{TRUE}, reports heteroskedasticity-robust standard errors
#'   instead of conventional SEs. These are also known as Huber-White standard errors.
#'   Default is \code{FALSE}.
#'   This requires the \code{sandwich} and \code{lmtest} packages to compute the
#'    standard errors.
#' @param robust.type Only used if \code{robust=TRUE}. Specificies the type of
#'   robust standard errors to be used by \code{sandwich}. By default, set to \code{"HC3"}
#'   . See details for more on options.
#' @param digits An integer specifying the number of digits past the decimal to report in
#'   the output. Default is 5.
#'
#' @details Regardless of options selected, this function will print the following items to the console:
#' \itemize{
#'   \item The sample size
#'   \item The name of the outcome variable
#'   \item The number of predictors used
#'   \item The (Pseudo-)R-squared value (plus adjusted R-squared if OLS regression).
#'   \item A table with regression coefficients, standard errors, t-values, and p-values.
#' }
#'
#'  There are several options available for \code{robust.type}. The heavy lifting is done by
#'  \code{\link[sandwich]{vcovHC}}, where those are better described. Put simply, you may choose
#'  from \code{"HC0"} to \code{"HC5"}. Based on the recommendation of the developers of
#'  \code{sandwich}, the default is set to \code{"HC3"}. Stata's default is \code{"HC1"}, so
#'  that choice may be better if the goal is to replicate Stata's output. Any option that
#'  is understood by \code{vcovHC} will be accepted.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @examples
#' # Create lm object
#' fit <- lm(Income ~ `HS Grad` + Illiteracy + Murder, data=as.data.frame(state.x77))
#'
#' # Print the output with standardized coefficients and 2 digits past the decimal
#' j_summ(fit, stdbeta=TRUE, digits=2)
#'
#' @importFrom stats coef coefficients lm predict sd
#' @export j_summ

j_summ <- function(lm, stdbeta=FALSE, vifs=FALSE, robust=FALSE, robust.type="HC3", digits=5) {

  sum <- summary(lm)
  if (class(lm)[1] == "lm") {
    bsum <- broom::glance(lm)
  }

  # Intercept?
  if (lm$rank != attr(lm$terms, "intercept")) {
    df.int <- if (attr(lm$terms, "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(lm$residuals)

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

  # List of names of predictors
  ivs <- names(coefficients(lm))

  # Unstandardized betas
  ucoefs <- unname(coef(lm))

  ## Taken and modified from QuantPsyc package
  # Standardized betas
  if (stdbeta==T) {
    b <- rep(NA, length(ivs))
    b[2:as.numeric(length(ivs))] <- coef(lm)[-1]
    sx <- rep(NA, length(ivs))
    sx[2:as.numeric(length(ivs))] <- sapply(lm$model[-1], sd)
    sy <- sapply(lm$model[1], sd)
    betas <- rep(NA, length(ivs))
    betas <- (b * sx) / sy
  }

  # Model statistics
  fstat <- unname(sum$fstatistic[1])
  fnum <- unname(sum$fstatistic[2])
  fden <- unname(sum$fstatistic[3])

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
  if (robust == TRUE & typeof(lm)[1] != "svyglm") {

    if (!requireNamespace("sandwich", quietly = TRUE)) {
      stop("When robust is set to TRUE, you need to have the \'sandwich\' package for robust standard errors.
           Please install it or set robust to FALSE.",
           call. = FALSE)
    }

    if (!requireNamespace("lmtest", quietly = TRUE)) {
      stop("When robust is set to TRUE, you need to have the \'lmtest\' package for robust standard errors.
           Please install it or set robust to FALSE.",
           call. = FALSE)
    }

    coefs <- sandwich::vcovHC(lm, type=robust.type)
    coefs <- lmtest::coeftest(lm,coefs)
    ses <- coefs[,2]
    ts <- coefs[,3]
    pvals <- coefs[,4]
    # stdses <- rep(NA, length(ivs))
    # stdses[-1] <- ses * sx / sy
  } else {
    ses <- coef(sum)[,2]
    ts <- coef(sum)[,3]
    pvals <- coef(sum)[,4]
    # stdses <- rep(NA, length(ivs))
    # stdses[-1] <- ses * sx / sy
  }

  digitstars <- c()
  for (x in 1:length(ivs)) {
    if (pvals[x] > 0.1) {
      digitstars[x] <- ""
    } else if (pvals[x] <= 0.1 & pvals[x] > 0.05) {
      digitstars[x] <- "."
    } else if (pvals[x] > 0.01 & pvals[x] <= 0.05) {
      digitstars[x] <- "*"
    } else if (pvals[x] > 0.001 & pvals[x] <= 0.01) {
      digitstars[x] <- "**"
    } else if (pvals[x] <= 0.001) {
      digitstars[x] <- "***"
    }
  }

  if (stdbeta==F && vifs==F) {
    mat <- matrix(nrow=length(ivs), ncol=5)
    rownames(mat) <- ivs
    colnames(mat) <- c("Est.", "S.E.", "t val.", "p", "")
    mat[,1] <- round(ucoefs, digits=digits)
    mat[,2] <- round(ses, digits=digits)
    mat[,3] <- round(ts, digits=digits)
    mat[,4] <- round(pvals, digits=digits)
    mat[,5] <- digitstars
    t <- as.table(mat)
  }

  if (stdbeta==T && vifs==T) {
    mat <- matrix(nrow=length(ivs), ncol=7)
    rownames(mat) <- ivs
    colnames(mat) <- c("Est.", "S.E.", "Std. Beta.", "t val.", "p", "", "VIF")
    mat[,1] <- round(ucoefs, digits=digits)
    mat[,2] <- round(ses, digits=digits)
    mat[,3] <- round(betas, digits=digits)
    mat[,4] <- round(ts, digits=digits)
    mat[,5] <- round(pvals, digits=digits)
    mat[,6] <- digitstars
    mat[,7] <- round(tvifs, digits=digits)
    t <- as.table(mat)
  }

  if (stdbeta==F && vifs==T) {
    mat <- matrix(nrow=length(ivs), ncol=6)
    rownames(mat) <- ivs
    colnames(mat) <- c("Est.", "S.E.", "t val.", "p", "", "VIF")
    mat[,1] <- round(ucoefs, digits=digits)
    mat[,2] <- round(ses, digits=digits)
    mat[,3] <- round(ts, digits=digits)
    mat[,4] <- round(pvals, digits=digits)
    mat[,5] <- digitstars
    mat[,6] <- round(tvifs, digits=digits)
    t <- as.table(mat)
  }

  if (stdbeta==T && vifs==F) {
    mat <- matrix(nrow=length(ivs), ncol=6)
    rownames(mat) <- ivs
    colnames(mat) <- c("Est.", "S.E.", "Std. Beta", "t val.", "p", "")
    mat[,1] <- round(ucoefs, digits=digits)
    mat[,2] <- round(ses, digits=digits)
    mat[,3] <- round(betas, digits=digits)
    mat[,4] <- round(ts, digits=digits)
    mat[,5] <- round(pvals, digits=digits)
    mat[,6] <- digitstars
    t <- as.table(mat)
  }

  cat("Model Info", "\n", "Sample Size: ", n, "\n", "Dependent Variable: ", names(lm$model[1]), "\n", "Number of Predictors: ", (lm$rank-1), "\n", sep="")
  if (class(lm)[1]=="svyglm") {
    cat("\n", "Analysis of complex survey design", "\n", sep="")
    if (as.character(lm$family[1])=="gaussian" && as.character(lm$family[2])=="identity") {
      cat("Survey-weighted linear regression", "\n", "\n", sep="")
    } else {
      cat("Error Distribution: ", as.character(lm$family[1]), "\n", "Link function: ", as.character(lm$family[2]), "\n", "\n", sep="")
    }
  } else {
    cat("\n")
  }

  if (class(lm)[1]=="lm") {
    cat("Model Fit: ", "\n", "F(", fnum, ",", fden, ") = ", round(fstat, digits=digits), ", p = ", round(bsum$p.value, digits=digits), "\n", "R-squared = ", round(rsq, digits=digits), "\n", "Adj. R-squared = ", round(arsq, digits=digits), "\n", "\n", sep="")
    if (robust==F) {
      cat("Standard errors: OLS", "\n")
    }
  }

  if (class(lm)[1]=="glm" || (class(lm)[2]=="glm" && !is.na(class(lm)[2]))) {
    cat("Model Fit: ", "\n", "Pseudo R-squared = ", round(rsq, digits=digits), "\n", "\n", sep="")
  }

  if (robust==T) {
    cat("Standard errors: Robust, type = ", robust.type, "\n", sep="")
  }

  print(t)

    }
