#' Test whether sampling weights are needed
#'
#' Use the DuMouchel-Duncan (1983) test to assess the need for sampling weights
#' in your linear regression analysis.
#'
#' @param model The unweighted linear model (must be \code{lm},
#' \code{glm}, see details for other types) you want to check.
#'
#' @param data The data frame with the data fed to the fitted model and the weights
#'
#' @param weights The name of the weights column in \code{model}'s data frame
#'   or a vector of weights equal in length to the number of observations
#'   included in \code{model}.
#'
#' @param model_output Should a summary of the model with weights as predictor
#'   be printed? Default is TRUE, but you may not want it if you are trying to
#'   declutter a document.
#'
#' @param test Which type of test should be used in the ANOVA? The default,
#'   \code{NULL}, chooses based on the model type ("F" for linear models).
#'   This argument is passed to \code{anova}.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 3. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired number.
#'
#' @details
#'
#' This is designed to be similar to the \code{wgttest} macro for Stata
#' (\url{http://fmwww.bc.edu/repec/bocode/w/wgttest.html}). This method,
#' advocated for by DuMouchel and Duncan (1983), is fairly straightforward. To
#' decide whether weights are needed, the weights are added to the linear model
#' as a predictor and interaction with each other predictor. Then, an omnibus
#' test of significance is performed to compare the weights-added model to the
#' original; if insignificant, weights are not significantly related to the
#' result and you can use the more efficient estimation from unweighted OLS.
#'
#' It can be helpful to look at the created model using \code{model_output = TRUE}
#' to see which variables might be the ones affected by inclusion of weights.
#'
#' This test can support most GLMs in addition to LMs, a use validated by
#' Nordberg (1989). This, to my knowledge, is different from the Stata macro.
#' It does not work for mixed models (e.g., \code{lmer} or \code{lme}) though
#' it could plausibly be
#' implemented. However, there is no scholarly consensus how to properly
#' incorporate weights into mixed models. There are other types of models that
#' may work, but have not been tested. The function is designed to be
#' compatible with as many model types as possible, but the user should be
#' careful to make sure s/he understands whether this type of test is
#' appropriate for the model being considered. DuMouchel and Duncan (1983) were
#' only thinking about linear regression when the test was conceived.
#' Nordberg (1989) validated its use with generalized linear models, but to
#' this author's knowledge it has not been tested with other model types.
#'
#' @references
#'
#' DuMouchel, W. H. & Duncan, D.J. (1983). Using sample survey weights in
#'   multiple regression analyses of stratified samples. \emph{Journal of the
#'   American Statistical Association}, \emph{78}. 535-543.
#'
#' Nordberg, L. (1989). Generalized linear modeling of sample survey data.
#'   \emph{Journal of Official Statistics; Stockholm}, \emph{5}, 223–239.
#'
#' Winship, C. & Radbill, L. (1994). Sampling weights and regression
#'   analysis. \emph{Sociological Methods and Research}, \emph{23}, 230-257.
#'
#' @family survey tools
#'
#' @examples
#' # First, let's create some fake sampling weights
#' wts <- runif(50, 0, 5)
#' # Create model
#' fit <- lm(Income ~ Frost + Illiteracy + Murder,
#'           data = as.data.frame(state.x77))
#' # See if the weights change the model
#' wgttest(fit, wts)
#'
#' # With a GLM
#' wts <- runif(100, 0, 2)
#' x <- rnorm(100)
#' y <- rbinom(100, 1, .5)
#' fit <- glm(y ~ x, family = binomial)
#' wgttest(fit, wts)
#' ## Can specify test manually
#' wgttest(fit, wts, test = "Rao")
#'
#' # Quasi family is treated differently than likelihood-based
#' ## Dobson (1990) Page 93: Randomized Controlled Trial (plus some extra values):
#' counts <- c(18,17,15,20,10,20,25,13,12,18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,18)
#' treatment <- gl(3,6)
#' glm.D93 <- glm(counts ~ outcome + treatment, family = quasipoisson)
#' wts <- runif(18, 0, 3)
#' wgttest(glm.D93, wts)
#'
#' @importFrom stats anova reformulate
#' @export

wgttest <- function(model, data = NULL, weights, model_output = TRUE,
                    test = NULL,
                    digits = getOption("jtools-digits", default = 3)) {

  # Need to parse the arguments
  # Need to parse the arguments
  if (length(as.character(substitute(weights))) == 1 &&
      as.character(substitute(weights)) %in% names(data)) {
    wname <- as.character(substitute(weights))
    numeric.input <- FALSE
  } else {
    if (is.numeric(weights)) {
      numeric.input <- TRUE
    } else {
      stop("weights argument must be either the name of a column in the data frame or a numeric vector.")
    }
  }

  if (!is.null(data)) {

    call <- getCall(model)
    if (!is.null(call$data)) {

      d <- eval(call$data)

    } else {

      d <- model.frame(model)

    }

  } else {

    d <- data

  }

  # Save to d.f. and handle weights entered as numeric vector
  d <- as.data.frame(d)
  if (numeric.input == TRUE) {
    d$weight <- weights
    wname <- "weight"
  }

  # Save response variable
  resp <- as.character(formula(model)[2])

  terms <- attributes(terms(model))$term.labels
  terms <- unname(as.character(terms))
  nterms <- sapply(terms, c, paste(" *", wname), USE.NAMES = F, simplify = F)
  nterms <- sapply(nterms, paste, sep = "", collapse = "")

  newf <- reformulate(nterms, response = resp)

  newmod <- update(model, formula. = newf, data = d)

  # Getting model family, but trying to avoid breaking the function when no
  # family() method exists
  tryCatch({
    family <- family(model)[1]
  }, error = {family <- NULL})

  #
  if (class(model)[1] == "lm") {

    if (is.null(test)) {
      test <- "F"
    }

    linear <- TRUE
    lm <- TRUE

    aout <- anova(model, newmod, test = test)

  } else if (family == "gaussian") { # OLS from GLM returns different ANOVA obj

    if (is.null(test)) {
      test <- "F"
    }

    linear <- TRUE
    lm <- FALSE

    aout <- anova(model, newmod, test = test)

  } else if (family %in% c("quasi","quasibinomial","quasipoisson")) {
    # Quasilikelihood should use F test

    if (is.null(test)) {
      test <- "F"
    }

    linear <- FALSE
    lm <- FALSE

    aout <- anova(model, newmod, test = test)

  } else if (!is.null(family)) { # Some other family

    if (is.null(test)) {
      test <- "LRT"
    }

    linear <- FALSE
    lm <- FALSE

    aout <- anova(model, newmod, test = test)

  } else { # Contigency plan for model types that may not use test argument

    aout <- anova(model, newmod)

  }

  # Return object
  out <- list(aout = aout, newmod = newmod, model_output = model_output)
  # Attributes for use in pretty printing
  attr(out, "family") <- family
  attr(out, "linear") <- linear
  attr(out, "test") <- test
  attr(out, "lm") <- lm

  class(out) <- "wgttest"
  return(out)

}

#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.wgttest <- function(x, ...) {

  cat("DuMouchel-Duncan test of model change with weights\n\n")

  if (attr(x, "linear") == TRUE && attr(x, "lm") == TRUE &&
      attr(x, "test") == "F") {

    cat("F(", x$aout$Df[2], ",",
        x$aout$Res.Df[2], ") = ", round(x$aout$F[2],3),
        "\np = ", round(x$aout$`Pr(>F)`[2],3), "\n", sep = "")

  } else if (attr(x, "test") == "F" && attr(x, "lm") == F) {

    cat("F(", x$aout$Df[2], ",",
        x$aout$`Resid. Df`[2], ") = ", round(x$aout$F[2],3),
        "\np = ", round(x$aout$`Pr(>F)`[2],3), "\n", sep = "")

  } else if (attr(x, "test") %in% c("LRT","Chisq")) {

    cat("Deviance (", x$aout$Df[2], ") = ", round(x$aout$Deviance[2],3),
        "\np = ", round(x$aout$`Pr(>Chi)`[2],3), "\n", sep = "")

  } else if (attr(x, "test") == "Rao") {

    cat("Rao (", x$aout$Df[2], ",",
        x$aout$`Resid. Df`[2], ") = ", round(x$aout$Rao[2],3),
        "\np = ", round(x$aout$`Pr(>Chi)`[2],3), "\n", sep = "")

  } else {
    # In case the anova method returns something unexpected
    print(x$aout)

  }

  cat("\nLower p values indicate greater influence of the weights.")

  if (x$model_output == TRUE) {
    j_summ(x$newmod, model.info = F, model.fit = F)
    cat("\n")
  } else {
    cat("\n")
  }

}
#'
  }

}
#'
