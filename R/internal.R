## Making a "opposite of %in%" or "not %in%" function to simplify code
`%nin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))

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

### Have to specify data differently than pscl to fix namespace issues
# pR2 <- function(object) {
#   llh <- suppressWarnings(logLik(object))
#   if (class(object)[1] %in% c("svyglm","svrepglm")) {
#     objectNull <- suppressWarnings(update(object, ~ 1,
#                                           design = object$survey.design))
#   } else {
#   objectNull <- suppressWarnings(update(object, ~ 1,
#                                         data = model.frame(object)))
#   }
#   llhNull <- logLik(objectNull)
#   n <- dim(object$model)[1]
#   pR2Work(llh,llhNull,n)
# }

# Weighted std. dev. used in gscale
wtd.sd <- function(x, w) {
  # Get the mean
  xm <- weighted.mean(x, w, na.rm = TRUE)
  # Squaring the weighted deviations and dividing by weighted N - 1
  variance <- sum((w * (x - xm)^2)/(sum(w)-1), na.rm = TRUE)
  # Standard deviation is sqrt(variance)
  sd <- sqrt(variance)
  # Return the SD
  return(sd)
}

# Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}

## Taken from car package with modifications so it doesn't break j_summ
ncvTest <- function(model, ...){
  UseMethod("ncvTest")
}

ncvTest.lm <- function(model, var.formula, ...) {
  # data <- getCall(model)$data
  # model <- if (!is.null(data)){
  #   data <- eval(data, envir=environment(formula(model)))
  #   update(model, formula(model), na.action="na.exclude", data=data)
  # }
  # else update(model, formula(model), na.action="na.exclude")
  sumry <- summary(model)
  residuals <- residuals(model, type="pearson")
  S.sq <- stats::df.residual(model)*(sumry$sigma)^2/sum(!is.na(residuals))
  .U <- (residuals^2)/S.sq
  if (missing(var.formula)) {
    mod <- lm(.U ~ fitted.values(model))
    varnames <- "fitted.values"
    var.formula <- ~ fitted.values
    df <- 1
  }
  else {
    form <- as.formula(paste(".U ~ ", as.character(var.formula)[[2]], sep=""))
    mod <- if(!is.null(data)){
      data$.U <- .U
      lm(form, data=data)
    }
    else lm(form)
    df <- sum(!is.na(coefficients(mod))) - 1
  }
  SS <- anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS/2
  result <- list(formula=var.formula, formula.name="Variance", ChiSquare=Chisq, Df=df,
                 p=stats::pchisq(Chisq, df, lower.tail=FALSE), test="Non-constant Variance Score Test")
  class(result) <- "chisqTest"
  result
}

print.chisqTest <- function(x, ...){
  title <- if (!is.null(x$test)) x$test else "Chisquare Test"
  cat(title,"\n")
  if (!is.null(x$formula)) cat(x$formula.name,
                               "formula:", as.character(x$formula), "\n")
  cat("Chisquare =", x$ChiSquare,"   Df =", x$Df,
      "    p =", x$p, "\n")
  invisible(x)
}

### Hadley update #############################################################
# from https://stackoverflow.com/questions/13690184/update-inside-a-function-
# only-searches-the-global-environment

j_update <- function(mod, formula = NULL, data = NULL, offset = NULL,
                     weights = NULL) {
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }

  if (!is.null(data)) call$data <- data
  if (!is.null(formula)) call$formula <- update.formula(call$formula, formula)
  env <- attr(term, ".Environment")
  # Jacob add
  # if (!is.null(offset))
    call$offset <- offset
  # if (!is.null(weights))
    call$weights <- weights

  eval(call, env, parent.frame())
}

### coeftest ##################################################################

## Taken from lmtest package with changes

coeftest <- function(x, vcov. = NULL, df = NULL, ...) {
  UseMethod("coeftest")
}

#' @importFrom stats pnorm

coeftest.default <- function(x, vcov. = NULL, df = NULL, ...) {
  ## use S4 methods if loaded
  if (requireNamespace("stats4", quietly = TRUE)) {
    coef0 <- stats4::coef
    vcov0 <- stats4::vcov
  } else {
    coef0 <- coef
    vcov0 <- vcov
  }

  ## extract coefficients and standard errors
  est <- coef0(x)
  if (is.null(vcov.)) {
    se <- vcov0(x)
  } else {
    if (is.function(vcov.)) {
      se <- vcov.(x, ...)
    } else {
      se <- vcov.
    }
  }
  se <- sqrt(diag(se))

  ## match using names and compute t/z statistics
  if (!is.null(names(est)) && !is.null(names(se))) {

    if (length(unique(names(est))) == length(names(est)) &&
       length(unique(names(se))) == length(names(se))) {

      anames <- names(est)[names(est) %in% names(se)]
      est <- est[anames]
      se <- se[anames]

    }
  }
  tval <- as.vector(est)/se

  ## apply central limit theorem
  if (is.null(df)) {

    df <- try(df.residual(x), silent = TRUE)
    if(inherits(df, "try-error")) df <- NULL

  }

  if (is.null(df)) df <- 0

  if (is.finite(df) && df > 0) {

    pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
    cnames <- c("Est.", "S.E.", "t value", "p")
    mthd <- "t"

  } else {

    pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
    cnames <- c("Est.", "S.E.", "z value", "p")
    mthd <- "z"

  }

  rval <- cbind(est, se, tval, pval)
  colnames(rval) <- cnames
  class(rval) <- "coeftest"
  attr(rval, "method") <- paste(mthd, "test of coefficients")
  ##  dQuote(class(x)[1]), "object", sQuote(deparse(substitute(x))))
  return(rval)

}

coeftest.glm <- function(x, vcov. = NULL, df = Inf, ...) {
  coeftest.default(x, vcov. = vcov., df = df, ...)
}

# coeftest.mlm <- function(x, vcov. = NULL, df = NULL, ...) {
#   ## obtain vcov
#   v <- if (is.null(vcov.)) {
#     vcov(x)
#   } else if (is.function(vcov.)) {
#     vcov.(x)
#   } else {
#     vcov.
#   }
#
#   ## nasty hack: replace coefficients so that their names match the vcov() method
#   x$coefficients <- structure(as.vector(x$coefficients),
#                               .Names = colnames(vcov(x)))
#
#   ## call default method
#   coeftest.default(x, vcov. = v, df = df, ...)
# }
#
# coeftest.survreg <- function(x, vcov. = NULL, df = Inf, ...) {
#
#   if (is.null(vcov.)) {
#     v <- vcov(x)
#   } else {
#     if (is.function(vcov.)) {
#       v <- vcov.(x)
#     } else {
#       v <- vcov.
#     }
#   }
#
#   if (length(x$coefficients) < NROW(x$var)) {
#     x$coefficients <- c(x$coefficients, "Log(scale)" = log(x$scale))
#   }
#
#   coeftest.default(x, vcov. = v, df = df, ...)
#
# }
