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
#' wgttest(fit, weights = wts)
#'
#' # With a GLM
#' wts <- runif(100, 0, 2)
#' x <- rnorm(100)
#' y <- rbinom(100, 1, .5)
#' fit <- glm(y ~ x, family = binomial)
#' wgttest(fit, wts)
#' ## Can specify test manually
#' wgttest(fit, weights = wts, test = "Rao")
#'
#' # Quasi family is treated differently than likelihood-based
#' ## Dobson (1990) Page 93: Randomized Controlled Trial (plus some extra values):
#' counts <- c(18,17,15,20,10,20,25,13,12,18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,18)
#' treatment <- gl(3,6)
#' glm.D93 <- glm(counts ~ outcome + treatment, family = quasipoisson)
#' wts <- runif(18, 0, 3)
#' wgttest(glm.D93, weights = wts)
#'
#' @importFrom stats anova reformulate
#' @export

wgttest <- function(model, weights, data = NULL, model_output = TRUE,
                    test = NULL,
                    digits = getOption("jtools-digits", default = 3)) {

  if (is.null(data)) {

    call <- getCall(model)
    if (!is.null(call$data)) {

      d <- eval(call$data)

    } else {

      d <- model.frame(model)

    }

  } else {

    d <- data

  }

  # Need to parse the arguments
  if (length(as.character(substitute(weights))) == 1 &&
      as.character(substitute(weights)) %in% names(d)) {
    wname <- as.character(substitute(weights))
    numeric.input <- FALSE
  } else {
    if (is.numeric(weights)) {
      numeric.input <- TRUE
    } else {
      stop("weights argument must be either the name of a column in the data frame or a numeric vector.")
    }
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

#' Test whether sampling weights are needed
#'
#' Use the test proposed in Pfeffermann and Sverchkov (1999) to check whether
#' a regression model is specified correctly without weights.
#'
#' @param model The fitted model, without weights
#'
#' @param data The data frame with the data fed to the fitted model and the weights
#'
#' @param weights The name of the weights column in \code{model}'s data frame
#'   or a vector of weights equal in length to the number of observations
#'   included in \code{model}.
#'
#' @param sims The number of bootstrap simulations to use in estimating the
#'   variance of the residual correlation. Default is 1000, but for publications
#'   or when computing power/time is sufficient, a higher number is better.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 3. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired number.
#'
#' @details
#'
#' This is a test described by Pfeffermann and Sverchkov (1999) that is
#' designed to help analysts decide whether they need to use sample weights
#' in their regressions to avoid biased parameter estimation.
#'
#' It first checks the correlation of the residuals of the model with the
#' weights. It then uses bootstrapping to estimate the variance of the
#' correlation, ending with a t-test of whether the correlation differs from
#' zero. This is done for the squared residuals and cubed residuals as well.
#' If anyone of them are statistically significant (at whatever level you
#' feel appropriate), it is best to do a weighted regression. Note that in
#' large samples, a very small correlation may have a low p-value without a
#' large bias in the unweighted regression.
#'
#' @references
#'
#' Pfeffermann, D., & Sverchkov, M. (1999). Parametric and semi-parametric
#'  estimation of regression models fitted to survey data.
#'  \emph{Sankhyā: The Indian Journal of Statistics}, \emph{61}. 166–186.
#'
#' @family survey tools
#'
#' @examples
#'
#' # Note: This is a contrived example to show how the function works,
#' # not a case with actual sammpling weights from a survey vendor
#' states <- as.data.frame(state.x77)
#' set.seed(100)
#' states$wts <- runif(50, 0, 3)
#' fit <- lm(Murder ~ Illiteracy + Frost, data = states)
#' pf_sv_test(model = fit, data = states, weights = wts, sims = 100)
#'
#' @export
#' @importFrom stats resid cor pt sd
#' @importFrom boot boot

pf_sv_test <- function (model, data, weights, sims = 1000,
                        digits = getOption("jtools-digits", default = 3)) {

  if (!requireNamespace("boot", quietly = TRUE)) {
    stop("This function relies on the boot package. Please install it and try again.",
         call. = FALSE)
  }

  # Need to parse the arguments
  if (length(as.character(substitute(weights))) == 1 &&
      as.character(substitute(weights)) %in% names(data)) {
    weights <- as.character(substitute(weights))
    numeric.input <- FALSE
  } else {
    if (is.numeric(weights)) {
      numeric.input <- TRUE
    } else {
      stop("weights argument must be either the name of a column in the data frame or a numeric vector.")
    }
  }

  data <- as.data.frame(data)
  if (numeric.input == TRUE) {
    data$weight <- weights
    weights <- "weight"
  }

  corfun <- function(data, wts, model, numeric.input, indices) {

    data <- as.data.frame(data)[indices,]
    newmod <- update(model, data = data)
    wts <- data[[wts]]

    c1 <- cor(resid(newmod), wts)
    c2 <- cor(resid(newmod)^2, wts)
    c3 <- cor(resid(newmod)^3, wts)

    return(c(atanh(c1), atanh(c2), atanh(c3)))

  }

  bo <- boot::boot(data = data, statistic = corfun, R = sims, model = model,
                   wts = weights, numeric.input = numeric.input)

  # Getting the in-sample correlations
  c1 <- cor(resid(model), data[[weights]])
  c2 <- cor(resid(model)^2, data[[weights]])
  c3 <- cor(resid(model)^3, data[[weights]])

  # Now saving the Fisher's z-transformed versions of them
  z1 <- atanh(c1)
  z2 <- atanh(c2)
  z3 <- atanh(c3)

  # Getting the bootstrapped SD of the correlation, but transforming to
  # Fisher's Z first
  sd1 <- sd(atanh(bo$t[,1]))
  sd2 <- sd(atanh(bo$t[,2]))
  sd3 <- sd(atanh(bo$t[,3]))

  # Calculating the test statistics
  t1 <- z1/sd1
  t2 <- z2/sd2
  t3 <- z3/sd3

  # Calculating the p-values using N - 1 DF
  p1 <- pt(abs(t1), df = length(resid(model)), lower.tail = FALSE) * 2
  p2 <- pt(abs(t2), df = length(resid(model)), lower.tail = FALSE) * 2
  p3 <- pt(abs(t3), df = length(resid(model)), lower.tail = FALSE) * 2

  out <- list(tval1 = t1, tval2 = t2, tval3 = t3,
              p1 = p1, p2 = p2, p3 = p3,
              r1 = c1, r2 = c2, r3 = c3, digits = digits)

  class(out) <- "pf_sv_test"
  return(out)


}

#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.pf_sv_test <- function (x, ...) {

  cat("\nPfeffermann-Sverchkov test of sample weight ignorability \n\n")
  cat("Residual correlation = ", round(x$r1, x$digits), ", p = ",
      round(x$p1, x$digits), "\n", sep = "")
  cat("Squared residual correlation = ", round(x$r2, x$digits), ", p = ",
      round(x$p2, x$digits), "\n", sep = "")
  cat("Cubed residual correlation = ", round(x$r3, x$digits), ", p = ",
      round(x$p3, x$digits), "\n", sep = "")

  cat("\nA significant correlation may indicate biased estimates in",
      "the unweighted model.\n")

}

#' Test whether sampling weights are needed
#'
#' Use the tests proposed in Pfeffermann and Sverchkov (1999)
#' and DuMouchel and Duncan (1983) to check whether
#' a regression model is specified correctly without weights.
#'
#' @param model The fitted model, without weights
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
#' @param sims The number of bootstrap simulations to use in estimating the
#'   variance of the residual correlation. Default is 1000, but for publications
#'   or when computing power/time is sufficient, a higher number is better.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 3. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired number.
#'
#' @details
#'
#' This function is a wrapper for the two tests implemented in this package that
#' test whether your regression model is correctly specified. The first is
#' \code{\link{wgttest}}, an R adaptation of the Stata macro of the same name.
#' This test can otherwise be referred to as the DuMouchel-Duncan test. The
#' other test is the Pfeffermann-Sverchkov test, which can be accessed directly
#' with \code{\link{pf_sv_test}}.
#'
#' For more details on each, visit the documentation on the respective functions.
#' This function just runs each of them for you.
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
#' Pfeffermann, D., & Sverchkov, M. (1999). Parametric and semi-parametric
#'  estimation of regression models fitted to survey data.
#'  \emph{Sankhyā: The Indian Journal of Statistics}, \emph{61}. 166–186.
#'
#' @family survey tools
#'
#' @examples
#'
#' # Note: This is a contrived example to show how the function works,
#' # not a case with actual sammpling weights from a survey vendor
#' states <- as.data.frame(state.x77)
#' set.seed(100)
#' states$wts <- runif(50, 0, 3)
#' fit <- lm(Murder ~ Illiteracy + Frost, data = states)
#' weights_tests(model = fit, data = states, weights = wts, sims = 100)
#'
#' @export

weights_tests <- function(model, weights, data, model_output = TRUE,
                          test = NULL, sims = 1000,
                          digits = getOption("jtools-digits", default = 3)) {

  # Create list of acceptable arguments to both functions
  wtnames <- names(formals(wgttest))
  pfnames <- names(formals(pf_sv_test))

  # Capture explicit args
  args <- list(model = model, weights = substitute(weights), data = data,
               model_output = model_output, test = test, sims = sims,
               digits = digits)

  # Create list of arguments for each
  wtargs <- args[names(args) %in% wtnames]
  pfargs <- args[names(args) %in% pfnames]

  # Call the functions
  wt <- do.call("wgttest", wtargs)
  pf <- do.call("pf_sv_test", pfargs)

  # Save both to output object
  out <- list(wt = wt, pf = pf)
  class(out) = "weights_tests"

  return(out)

}

#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.weights_tests <- function(x, ...) {

  print(x$wt)
  print(x$pf)

}





