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
