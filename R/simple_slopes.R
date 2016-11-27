#' Perform a simple slopes analysis.
#'
#' \code{sim_slopes()} conducts a simple slopes analysis for the purposes of understanding
#' interaction effects in linear regression.
#'
#' @param formula A regression formula, like would be provided to \code{lm()}, in
#'   quotation marks. This should not include the interaction term. Alternately, give
#'   an \code{lm} object and the formula will be extracted. Interaction terms will be ignored.
#'
#' @param data A data frame.
#'
#' @param pred The predictor variable involved in the interaction in quotes.
#'
#' @param modx The moderator variable involved in the interaction in quotes.
#'
#' @param modxvals For which two values of the moderator should simple slopes analysis
#'   be performed? Default is \code{NULL}. If \code{NULL}, then the customary +/-
#'   1 standard deviation from the mean values are used. This would not be appropriate
#'   in the case of a binary moderator, however, as well as in some other use cases.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered.
#'
#' @param weights If weights are being used, provide the variable name where they are stored.
#'
#' @param robust Logical. If \code{TRUE}, computes heteroskedasticity-robust standard errors.
#'
#' @param robust.type Type of heteroskedasticity-robust standard errors to use if \code{robust=TRUE}.
#'   See details of \code{\link{j_summ}} for more on options.
#'
#' @param summary If \code{TRUE}, prints a model summary using \code{\link{j_summ}}
#'   for the non-mean-adjusted model.
#'
#' @examples
#' # Using a fitted model as formula input
#' fit <- lm(Income ~ `HS Grad` + Murder + Illiteracy,
#'   data=as.data.frame(state.x77))
#' sim_slopes(formula=fit, pred="Murder", modx="Illiteracy",
#'   centered=c("Murder","Illiteracy"), summary=TRUE)
#'

sim_slopes <- function(formula, data, pred, modx, modxvals = NULL, centered, weights = NULL, robust=FALSE, robust.type="HC3", summary=FALSE) {

  d <- data

  if (!is.null(centered)){
    for (var in centered) {
      d[,var] <- d[,var] - mean(d[,var])
    }
  }

  if (!is.null(weights)) {
    weight <- d[,weights]
  }

  # Get the formula from lm object if given
  if (class(formula)=="lm") {
    formula <- formula(formula)

    # Remove interactions if found
    if (grepl("\\*", as.character(formula)[3])) {
      # By swapping the asterisk for a plus sign, preserves all the predictors
      formula <- gsub("\\*", "\\+", as.character(formula))
    }
  }

  # Default to +/- 1 SD
  if (is.null(modxvals)) {
    modsd <- sd(d[,modx])
    d$modp <- d[,modx] - (mean(d[,modx])+modsd)
    d$modn <- d[,modx] - (mean(d[,modx])-modsd)
    modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx])-modsd)
    modxvalssd <- round(modxvalssd,3)
    names(modxvalssd) <- c("+1 SD", "-1 SD")

  } else { #otherwise use given values
    d$modp <- d[,modx] - modxvals[1]
    d$modn <- d[,modx] - modxvals[2]
  }

  formulap <- paste(formula, " + modp*", pred, sep="")
  formulan <- paste(formula, " + modn*", pred, sep="")
  formular <- paste(formula, " + ", modx, "*", pred, sep="")

  if (is.null(weights)) {
    modelp <- lm(formulap, data=d)
    modeln <- lm(formulan, data=d)

    if (summary==TRUE) {
      modelr <- lm(formular, data=d)
    }

  } else {
    modelp <- lm(formulap, data=d, weights=weight)
    modeln <- lm(formulan, data=d, weights=weight)

    if (summary==TRUE) {
      modelr <- lm(formular, data=d, weights=weight)
    }

  }

  if (robust==TRUE) {

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

    slopep <- lmtest::coeftest(modelp, vcov=sandwich::vcovHC(modelp, type=robust.type))[pred,]
    slopen <- lmtest::coeftest(modeln, vcov=sandwich::vcovHC(modeln, type=robust.type))[pred,]

  } else {

    slopep <- coef(modelp)[pred,]
    slopen <- coef(modeln)[pred,]

  }

  if (is.null(modxvals)) {

    cat("Slope of ", pred, " when ", modx, " = ", modxvalssd[1], " (", names(modxvalssd)[1], ")", ": \n", sep="")
    print(round(slopep,3))

    cat("\n", "Slope of ", pred, " when ", modx, " = ", modxvalssd[2], " (", names(modxvalssd)[2], ")", ": \n", sep="")
    print(round(slopen,3))

  } else {
    cat("Slope of ", pred, " when ", modx, " = ", modxvals[1], ": \n", sep="")
    print(round(slopep,3))

    cat("\n", "Slope of ", pred, " when ", modx, " = ", modxvals[2], ": \n", sep="")
    print(round(slopen,3))
  }

  if (summary==TRUE) {
    j_summ(modelr, robust=robust, robust.type=robust.type, digits=3)
  }
}
