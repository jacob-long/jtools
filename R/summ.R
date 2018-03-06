#' Regression summaries with options
#'
#' To get specific documentation, choose the appropriate link to the
#' type of model that you want to summarize from the details section.
#'
#' \itemize{
#'
#'   \item \code{\link{summ.lm}}
#'   \item \code{\link{summ.glm}}
#'   \item \code{\link{summ.svyglm}}
#'   \item \code{\link{summ.merMod}}
#'
#' }
#'
#' @param model A \code{lm}, \code{glm}, \code{\link[survey]{svyglm}}, or
#'   \code{\link[lme4]{merMod}} object.
#' @param ... Other arguments to be passed to the model.specific function.
#'
#'
#' @export
#' @importFrom stats nobs
#'


summ <- function(model, ...) {
  UseMethod("summ")
}

#' Regression summaries with options
#'
#' \code{j_summ} is an alias for \code{summ}.
#' To get specific documentation, choose the appropriate link to the
#' type of model that you want to summarize from the details section.
#'
#' \itemize{
#'
#'   \item \code{\link{summ.lm}}
#'   \item \code{\link{summ.glm}}
#'   \item \code{\link{summ.svyglm}}
#'   \item \code{\link{summ.merMod}}
#'
#' }
#'
#' @param model A \code{lm}, \code{glm}, \code{\link[survey]{svyglm}}, or
#'   \code{\link[lme4]{merMod}} object.
#' @param ... Other arguments to be passed to the model.specific function.
#'
#'
#' @export
#'

j_summ <- summ

#### lm #######################################################################

#' Linear regression summaries with options
#'
#' \code{summ} prints output for a regression model in a fashion similar to
#' \code{summary}, but formatted differently with more options.
#'
#' @param model A \code{lm} object.
#'
#' @param scale If \code{TRUE}, reports standardized regression
#'   coefficients. Default is \code{FALSE}.
#'
#' @param vifs If \code{TRUE}, adds a column to output with variance inflation
#'   factors (VIF). Default is \code{FALSE}.
#'
#' @param confint Show confidence intervals instead of standard errors? Default
#'   is \code{FALSE}.
#'
#' @param ci.width A number between 0 and 1 that signifies the width of the
#'   desired confidence interval. Default is \code{.95}, which corresponds
#'   to a 95\% confidence interval. Ignored if \code{confint = FALSE}.
#'
#' @param robust If not `FALSE`, reports heteroskedasticity-robust standard
#'   errors instead of conventional SEs. These are also known as Huber-White
#'   standard errors. There are several options provided by
#'   [sandwich::vcovHC()]: `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`,
#'   `"HC4m"`, `"HC5"`.
#'
#'   Default is \code{FALSE}.
#'
#'   This requires the \code{sandwich} package to compute the
#'    standard errors.
#'
#' @param cluster For clustered standard errors, provide the column name of
#'   the cluster variable in the input data frame (as a string). Alternately,
#'   provide a vector of clusters.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 2. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired
#'   number.
#'
#' @param pvals Show p values and significance stars? If \code{FALSE}, these
#'  are not printed. Default is \code{TRUE}, except for merMod objects (see
#'  details).
#'
#' @param n.sd If \code{scale = TRUE}, how many standard deviations should
#'  predictors be divided by? Default is 1, though some suggest 2.
#'
#' @param center If you want coefficients for mean-centered variables but don't
#'    want to standardize, set this to \code{TRUE}.
#'
#' @param transform.response Should scaling/centering apply to response
#'    variable? Default is \code{FALSE}.
#'
#' @param part.corr Print partial (labeled "partial.r") and
#'  semipartial (labeled "part.r") correlations with the table?
#'  Default is \code{FALSE}. See details about these quantities when robust
#'  standard errors are used.
#'
#' @param model.info Toggles printing of basic information on sample size,
#'   name of DV, and number of predictors.
#'
#' @param model.fit Toggles printing of R-squared and adjusted R-squared.
#'
#' @param model.check Toggles whether to perform Breusch-Pagan test for
#'  heteroskedasticity
#'  and print number of high-leverage observations. See details for more info.
#'
#' @param data If you provide the data used to fit the model here, that data
#'   frame is used to re-fit the model (if `scale` is `TRUE`)
#'   instead of the [stats::model.frame()]
#'   of the model. This is particularly useful if you have variable
#'   transformations or polynomial terms specified in the formula.
#'
#' @param ... This just captures extra arguments that may only work for other
#'  types of models.
#'
#' @details By default, this function will print the following items to the
#'  console:
#' \itemize{
#'   \item The sample size
#'   \item The name of the outcome variable
#'   \item The R-squared value plus adjusted R-squared
#'   \item A table with regression coefficients, standard errors, t-values, and
#'    p values.
#' }
#'
#'  There are several options available for \code{robust}. The heavy
#'  lifting is done by \code{\link[sandwich]{vcovHC}}, where those are better
#'  described.
#'  Put simply, you may choose from \code{"HC0"} to \code{"HC5"}. Based on the
#'  recommendation of the developers of \pkg{sandwich}, the default is set to
#'  \code{"HC3"}. Stata's default is \code{"HC1"}, so that choice may be better
#'  if the goal is to replicate Stata's output. Any option that is understood
#'  by \code{vcovHC} will be accepted. Cluster-robust standard errors are
#'  computed if \code{cluster} is set to the name of the input data's cluster
#'  variable or is a vector of clusters.
#'
#'  The \code{scale} and \code{center} options are performed via
#'  refitting
#'  the model with \code{\link{scale_mod}} and \code{\link{center_mod}},
#'  respectively. Each of those in turn uses \code{\link{gscale}} for the
#'  mean-centering and scaling.
#'
#'  If using \code{part.corr = TRUE}, then you will get these two common
#'  effect size metrics on the far right two columns of the output table.
#'  However, it should be noted that these do not go hand in hand with
#'  robust standard error estimators. The standard error of the coefficient
#'  doesn't change the point estimate, just the uncertainty. However,
#'  this function uses \emph{t}-statistics in its calculation of the
#'  partial and semipartial correlation. This provides what amounts to a
#'  heteroskedasticity-adjusted set of estimates, but I am unaware of any
#'  statistical publication that validates this type of use. Please
#'  use these as a heuristic when used alongside robust standard errors; do
#'  not report the "robust" partial and semipartial correlations in
#'  publications.
#'
#'  There are two pieces of information given for \code{model.check}, provided
#'  that the model is an \code{lm} object. First, a Breusch-Pagan test is
#'  performed with \code{\link[car]{ncvTest}}. This is a
#'  hypothesis test for which the alternative hypothesis is heteroskedastic
#'  errors. The test becomes much more likely to be statistically significant
#'  as the sample size increases; however, the homoskedasticity assumption
#'  becomes less important to inference as sample size increases (Lumley,
#'  Diehr, Emerson, & Lu, 2002). Take the result of the test as a cue to check
#'  graphical checks rather than a definitive decision. Note that the use of
#'  robust standard errors can account for heteroskedasticity, though some
#'  oppose this approach (see King & Roberts, 2015).
#'
#'  The second piece of information provided by setting \code{model.check} to
#'  \code{TRUE} is the number of high leverage observations. There are no hard
#'  and fast rules for determining high leverage either, but in this case it is
#'  based on Cook's Distance. All Cook's Distance values greater than (4/N) are
#'  included in the count. Again, this is not a recommendation to locate and
#'  remove such observations, but rather to look more closely with graphical
#'  and other methods.
#'
#'
#' @return If saved, users can access most of the items that are returned in
#'   the output (and without rounding).
#'
#'  \item{coeftable}{The outputted table of variables and coefficients}
#'  \item{model}{The model for which statistics are displayed. This would be
#'    most useful in cases in which \code{scale = TRUE}.}
#'
#'  Much other information can be accessed as attributes.
#'
#' @seealso \code{\link{scale_mod}} can simply perform the standardization if
#'  preferred.
#'
#'  \code{\link{gscale}} does the heavy lifting for mean-centering and scaling
#'  behind the scenes.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @examples
#' # Create lm object
#' fit <- lm(Income ~ Frost + Illiteracy + Murder,
#'           data = as.data.frame(state.x77))
#'
#' # Print the output with standardized coefficients and 3 digits
#' summ(fit, scale = TRUE, digits = 3)
#'
#' @references
#'
#' King, G., & Roberts, M. E. (2015). How robust standard errors expose
#'  methodological
#'  problems they do not fix, and what to do about it. \emph{Political
#'   Analysis},
#'   \emph{23}(2), 159–179. \url{https://doi.org/10.1093/pan/mpu015}
#'
#' Lumley, T., Diehr, P., Emerson, S., & Chen, L. (2002). The Importance of the
#' Normality Assumption in Large Public Health Data Sets.
#'  \emph{Annual Review of
#'  Public Health}, \emph{23}, 151–169.
#'  \url{https://doi.org/10.1146/annurev.publhealth.23.100901.140546}
#'
#'
#' @importFrom stats coef coefficients lm predict sd cooks.distance pf logLik
#'  extractAIC family fitted pt residuals terms model.weights
#' @export
#' @aliases j_summ.lm

summ.lm <- function(
  model, scale = FALSE, confint = FALSE, ci.width = .95,
  robust = FALSE, cluster = NULL, vifs = FALSE,
  digits = getOption("jtools-digits", default = 2), pvals = TRUE,
  n.sd = 1, center = FALSE, transform.response = FALSE, data = NULL,
  part.corr = FALSE, model.info = TRUE, model.fit = TRUE, model.check = FALSE,
  ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated arguments with helper function
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(any_deps)) {
    for (n in names(any_deps)[which(any_deps == FALSE)]) {
      # Reassign values as needed
      assign(n, deps[[n]])
    }
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

  # Checking for required package for VIFs to avoid problems
  if (vifs == TRUE) {
    if (!requireNamespace("car", quietly = TRUE)) {
      warning("When vifs is set to TRUE, you need to have the 'car' package",
              "installed. Proceeding without VIFs...")
      vifs <- FALSE
    }
  }

  # Using information from summary()
  sum <- summary(model)

  # Check missing obs
  missing <- length(sum$na.action)

  # Standardized betas
  if (scale == TRUE) {

    model <- scale_mod(model, n.sd = n.sd,
                      scale.response = transform.response,
                      data = data)
    # Using information from summary()
    sum <- summary(model)

  } else if (center == TRUE && scale == FALSE) {

    model <- center_mod(model, center.response = transform.response,
                        data = data)
    # Using information from summary()
    sum <- summary(model)

  }

  j <- structure(j, standardize = scale, vifs = vifs, robust = robust,
                 digits = digits, model.info = model.info,
                 model.fit = model.fit, model.check = model.check,
                 n.sd = n.sd, center = center, call = the_call,
                 env = the_env, scale = scale, data = data,
                 transform.response = transform.response)

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
  if (vifs == TRUE) {
    if (model$rank == 2 | (model$rank == 1 & df.int == 0L)) {
      tvifs <- rep(NA, 1)
    } else {
      tvifs <- rep(NA, length(ivs))
      tvifs[-1] <- unname(car::vif(model))
    }
  }

  # Standard errors and t-statistics
  if (identical(FALSE, robust)) {

    ses <- coef(sum)[,2]
    ts <- coef(sum)[,3]
    ps <- coef(sum)[,4]
    use_cluster <- FALSE

  } else {

    # Pass to robust helper function
    rob_info <- do_robust(model, robust, cluster, data)

    coefs <- rob_info$coefs
    ses <- rob_info$ses
    ts <- rob_info$ts
    ps <- rob_info$ps

    use_cluster <- rob_info$use_cluster
    robust <- rob_info$robust

  }

  if (confint == TRUE) {

    alpha <- (1 - ci.width) / 2
    tcrit <- abs(qnorm(alpha))

    lci_lab <- 0 + alpha
    lci_lab <- paste(round(lci_lab * 100, 1), "%", sep = "")

    uci_lab <- 1 - alpha
    uci_lab <- paste(round(uci_lab * 100, 1), "%", sep = "")

    lci <- ucoefs - (ses * tcrit)
    uci <- ucoefs + (ses * tcrit)
    params <- list(ucoefs, lci, uci, ts, ps)
    namevec <- c("Est.", lci_lab, uci_lab, "t val.", "p")

  } else {

    params <- list(ucoefs, ses, ts, ps)
    namevec <- c("Est.", "S.E.", "t val.", "p")

  }

  if (vifs == TRUE) {

    params[length(params) + 1] <- list(tvifs)
    namevec <- c(namevec, "VIF")

  }

  if (part.corr == TRUE) {

    pcs <- part_corr(ts, df.int, rsq, robust, n)

    namevec <- c(namevec, "partial.r", "part.r")
    pl <- length(params)
    params[(pl + 1)] <- list(pcs$partial_corrs)
    params[(pl + 2)] <- list(pcs$semipart_corrs)


  }

  mat <- matrix(nrow = length(ivs), ncol = length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in seq_len(length(params))) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  # Drop p-vals if user requests
  if (pvals == FALSE) {

    mat <- mat[,colnames(mat) %nin% "p"]

  }

  # Implement model checking features
  if (model.check == TRUE) {

    homoskedp <- ncvTest(model)$p
    j <- structure(j, homoskedp = homoskedp)

    cd <- table(cooks.distance(model) > 4 / n)
    j <- structure(j, cooksdf = cd[2])

  }

  j <- structure(j, rsq = rsq, arsq = arsq, dv = names(model$model[1]),
                 npreds = model$rank - df.int, lmClass = class(model),
                 missing = missing, use_cluster = use_cluster,
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 test.stat = "t val.",
                 standardize.response = transform.response,
                 scale.response = transform.response,
                 transform.response = transform.response,
                 odds.ratio = FALSE)

  modpval <- pf(fstat, fnum, fden, lower.tail = FALSE)
  j <- structure(j, modpval = modpval)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("summ.lm", "summ")
  return(j)

}

### PRINT METHOD

#' @export

print.summ.lm <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals)

  if (x$model.info == TRUE) {
    if (x$missing == 0) {
      cat("MODEL INFO:", "\n", "Observations: ", x$n, "\n",
          "Dependent Variable: ",
          x$dv, "\n", sep = "")
      cat("\n")
    } else {
      cat("MODEL INFO:", "\n", "Observations: ", x$n, " (", x$missing,
          " missing obs. deleted)", "\n",
          "Dependent Variable: ",
          x$dv, "\n", sep = "")
      cat("\n")
    }
  }

  if (x$model.fit == T) {
    cat("MODEL FIT: ", "\n", "F(", x$fnum, ",", x$fden, ") = ",
        round(x$fstat, digits = x$digits), ", p = ",
        round(x$modpval, digits = x$digits),
        "\n", "R-squared = ", round(x$rsq, digits = x$digits), "\n",
        "Adj. R-squared = ",
        round(x$arsq, digits = x$digits), "\n", "\n", sep = "")
  }

  if (x$model.check == TRUE) {
    # Since it's lm, we can do Breusch-Pagan test
    if (x$homoskedp < .05) {
      homoskedtf <- paste0("Assumption violated (p = ",
                          round(x$homoskedp,digits = x$digits), ")")
    } else {
      homoskedtf <- paste0("Assumption not violated (p = ",
                          round(x$homoskedp, digits = x$digits), ")")
    }
    cat("MODEL CHECKING:", "\n", "Homoskedasticity (Breusch-Pagan) = ",
        homoskedtf,
        "\n", "Number of high-leverage observations = ", x$cooksdf,
        "\n\n", sep = "")
  }

  if (identical(FALSE, x$robust)) {

    cat("Standard errors: OLS", "\n")

  } else {

    if (x$robust == TRUE) {x$robust <- "HC3"}

    cat("Standard errors:", sep = "")

    if (x$use_cluster == FALSE) {

      cat(" Robust, type = ", x$robust, "\n", sep = "")

    } else if (x$use_cluster == TRUE) {

      cat(" Cluster-robust, type = ", x$robust, "\n", sep = "")

    }

  }

  print(ctable)

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}

###### glm ####################################################################

#' Generalized linear regression summaries with options
#'
#' \code{summ} prints output for a regression model in a fashion similar to
#' \code{summary}, but formatted differently with more options.
#'
#' @param model A `glm` object.
#' @param odds.ratio If \code{TRUE}, reports exponentiated coefficients with
#'  confidence intervals for exponential models like logit and Poisson models.
#'  This quantity is known as an odds ratio for binary outcomes and incidence
#'  rate ratio for count models.
#' @param ... This just captures extra arguments that may only work for other
#'  types of models.
#'
#' @inheritParams summ.lm
#'
#' @details By default, this function will print the following items to the
#'  console:
#' \itemize{
#'   \item The sample size
#'   \item The name of the outcome variable
#'   \item The (Pseudo-)R-squared value and AIC/BIC.
#'   \item A table with regression coefficients, standard errors, t-values, and
#'    p values.
#' }
#'
#'  There are several options available for \code{robust}. The heavy
#'  lifting is done by \code{\link[sandwich]{vcovHC}}, where those are better
#'  described.
#'  Put simply, you may choose from \code{"HC0"} to \code{"HC5"}. Based on the
#'  recommendation of the developers of \pkg{sandwich}, the default is set to
#'  \code{"HC3"}. Stata's default is \code{"HC1"}, so that choice may be better
#'  if the goal is to replicate Stata's output. Any option that is understood by
#'  \code{vcovHC} will be accepted. Cluster-robust standard errors are computed
#'  if \code{cluster} is set to the name of the input data's cluster variable
#'  or is a vector of clusters.
#'
#'  The \code{scale} and \code{center} options are performed via
#'  refitting
#'  the model with \code{\link{scale_mod}} and \code{\link{center_mod}},
#'  respectively. Each of those in turn uses \code{\link{gscale}} for the
#'  mean-centering and scaling.
#'
#' @return If saved, users can access most of the items that are returned in
#'   the output (and without rounding).
#'
#'  \item{coeftable}{The outputted table of variables and coefficients}
#'  \item{model}{The model for which statistics are displayed. This would be
#'    most useful in cases in which \code{scale = TRUE}.}
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
#'  ## Dobson (1990) Page 93: Randomized Controlled Trial :
#'  counts <- c(18,17,15,20,10,20,25,13,12)
#'  outcome <- gl(3,1,9)
#'  treatment <- gl(3,3)
#'  print(d.AD <- data.frame(treatment, outcome, counts))
#'  glm.D93 <- glm(counts ~ outcome + treatment, family = poisson)
#'
#'  # Summarize with standardized coefficients
#'  summ(glm.D93, scale = TRUE)
#'
#' @references
#'
#' King, G., & Roberts, M. E. (2015). How robust standard errors expose
#'  methodological problems they do not fix, and what to do about it.
#'  \emph{Political Analysis}, \emph{23}(2), 159–179.
#'  \url{https://doi.org/10.1093/pan/mpu015}
#'
#' Lumley, T., Diehr, P., Emerson, S., & Chen, L. (2002). The Importance of the
#' Normality Assumption in Large Public Health Data Sets. \emph{Annual Review
#'  of
#'  Public Health}, \emph{23}, 151–169.
#'  \url{https://doi.org/10.1146/annurev.publhealth.23.100901.140546}
#'
#'
#'
#' @importFrom stats coef coefficients lm predict sd cooks.distance pf logLik
#'  AIC BIC family fitted pt residuals terms model.weights
#' @export
#' @aliases j_summ.glm
#'

summ.glm <- function(
  model, scale = FALSE, confint = FALSE, ci.width = .95,
  robust = FALSE, cluster = NULL, vifs = FALSE,
  digits = getOption("jtools-digits", default = 2),
  odds.ratio = FALSE, model.info = TRUE, model.fit = TRUE,
  pvals = TRUE, n.sd = 1, center = FALSE,
  transform.response = FALSE, data = NULL,
  ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated argument
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(any_deps)) {
    for (n in names(any_deps)[which(any_deps == FALSE)]) {
      # Reassign values as needed
      assign(n, deps[[n]])
    }
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

  # Checking for required package for VIFs to avoid problems
  if (vifs == TRUE) {
    if (!requireNamespace("car", quietly = TRUE)) {
      warning("When vifs is set to TRUE, you need to have the 'car' package",
              " installed.\n Proceeding without VIFs...")
      vifs <- FALSE
    }
  }

  # Using information from summary()
  sum <- summary(model)

  # Check missing obs
  missing <- length(sum$na.action)

  if ("model.check" %in% names(dots) && dots$model.check == TRUE) {
    warning("Model checking is not currently implemented for GLMs")
  }

  # Standardized betas
  if (scale == TRUE) {

    model <- scale_mod(model, n.sd = n.sd, scale.response = transform.response,
                       data = data)
    # Using information from summary()
    sum <- summary(model)

  } else if (center == TRUE && scale == FALSE) {

    model <- center_mod(model, center.response = transform.response,
                        data = data)
    # Using information from summary()
    sum <- summary(model)

  }


  j <- structure(j, standardize = scale, vifs = vifs, digits = digits,
                 model.info = model.info, model.fit = model.fit, n.sd = n.sd,
                 center = center, call = the_call, env = the_env,
                 scale = scale, data = data,
                 transform.response = transform.response)

  # Intercept?
  if (model$rank != attr(model$terms, "intercept")) {
    df.int <- if (attr(model$terms, "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(model$residuals)
  j <- structure(j, n = n)

  if (model.fit == TRUE) {
    # Final calculations (linear pseudo-rsq)
    pr <- pR2(model)
    ## Cragg-Uhler
    rsq <- pr$r2CU
    ## McFadden
    rsqmc <- pr$McFadden
  } else {
    rsq <- NULL
    rsqmc <- NULL
  }

  # AIC for GLMs
  j <- structure(j, aic = AIC(model), bic = BIC(model))

  # List of names of predictors
  ivs <- names(coefficients(model))

  # Unstandardized betas
  ucoefs <- unname(coef(model))

  # VIFs
  if (vifs == TRUE) {
    if (model$rank == 2 | (model$rank == 1 & df.int == 0L)) {
      tvifs <- rep(NA, 1)
    } else {
      tvifs <- rep(NA, length(ivs))
      tvifs[-1] <- unname(car::vif(model))
    }
  }

  # Standard errors and t-statistics
  if (identical(FALSE, robust)) {

    ses <- coef(sum)[,2]
    ts <- coef(sum)[,3]
    ps <- coef(sum)[,4]
    use_cluster <- FALSE

  } else {

    # Pass to robust helper function
    rob_info <- do_robust(model, robust, cluster, data)

    coefs <- rob_info$coefs
    ses <- rob_info$ses
    ts <- rob_info$ts
    ps <- rob_info$ps

    use_cluster <- rob_info$use_cluster
    robust <- rob_info$robust

  }

  # Need proper name for test statistic
  tcol <- colnames(coef(sum))[3]
  tcol <- gsub("value", "val.", tcol)

  if (confint == TRUE | odds.ratio == TRUE) {

    alpha <- (1 - ci.width) / 2
    tcrit <- abs(qnorm(alpha))

    lci_lab <- 0 + alpha
    lci_lab <- paste(round(lci_lab * 100,1), "%", sep = "")

    uci_lab <- 1 - alpha
    uci_lab <- paste(round(uci_lab * 100,1), "%", sep = "")

  }

  # Report odds ratios instead, with conf. intervals
  if (odds.ratio == TRUE) {

    ecoefs <- exp(ucoefs)
    lci <- exp(ucoefs - (ses*tcrit))
    uci <- exp(ucoefs + (ses*tcrit))
    params <- list(ecoefs, lci, uci, ts, ps)
    namevec <- c("Odds Ratio", lci_lab, uci_lab, tcol, "p")

  } else if (odds.ratio == FALSE & confint == TRUE) {

    lci <- ucoefs - (ses*tcrit)
    uci <- ucoefs + (ses*tcrit)
    params <- list(ucoefs, lci, uci, ts, ps)
    namevec <- c("Est.", lci_lab, uci_lab, tcol, "p")

  } else {

    params <- list(ucoefs, ses, ts, ps)
    namevec <- c("Est.", "S.E.", tcol, "p")

  }

  # Put things together

  # Calculate vifs
  if (vifs == TRUE) {
    params[length(params) + 1] <- list(tvifs)
    namevec <- c(namevec, "VIF")
  }

  mat <- matrix(nrow = length(ivs), ncol = length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in seq_len(length(params))) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  # Drop p-vals if user requests
  if (pvals == FALSE) {

    mat <- mat[, colnames(mat) %nin% "p"]

  }

  # Extract dispersion parameter
  dispersion <- sum$dispersion

  j <- structure(j, rsq = rsq, rsqmc = rsqmc, dv = names(model$model[1]),
                 npreds = model$rank - df.int, dispersion = dispersion,
                 missing = missing, pvals = pvals, robust = robust,
                 robust.type = robust, use_cluster = use_cluster,
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 test.stat = tcol,
                 standardize.response = transform.response,
                 odds.ratio = odds.ratio,
                 scale.response = transform.response,
                 lmFamily = model$family)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("summ.glm", "summ")
  return(j)

}

### PRINT METHOD ###

#' @export

print.summ.glm <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

    # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals)

  if (x$model.info == TRUE) {
    cat("MODEL INFO:", "\n", "Observations: ", x$n, sep = "")
    if (x$missing == 0) {
      cat("\n",
          "Dependent Variable: ",
          x$dv, "\n", sep = "")
    } else {
      cat(" (", x$missing, " missing obs. deleted)\n", sep = "")
    }
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      cat("Type: Linear regression", "\n\n")
    } else {
      cat("Error Distribution: ", as.character(x$lmFamily[1]), "\n",
          "Link function: ",
          as.character(x$lmFamily[2]), "\n", "\n", sep = "")
    }
  }

  if (x$model.fit == TRUE) {
    cat("MODEL FIT: ", "\n", "Pseudo R-squared (Cragg-Uhler) = ",
        round(x$rsq, digits = x$digits), "\n",
        "Pseudo R-squared (McFadden) = ",
        round(x$rsqmc, digits = x$digits),
        "\n", "AIC = ", round(x$aic, x$digits), ", BIC = ",
        round(x$bic, x$digits), "\n\n", sep = "")
  }

  if (identical(FALSE, x$robust)) {

    cat("Standard errors: MLE", "\n")

  } else {

    if (x$robust == TRUE) {x$robust <- "HC3"}

    cat("Standard errors:", sep = "")

    if (x$use_cluster == FALSE) {

      cat(" Robust, type = ", x$robust, "\n", sep = "")

    } else if (x$use_cluster == TRUE) {

      cat(" Cluster-robust, type = ", x$robust, "\n", sep = "")

    }

  }

  print(ctable)

  if (x$dispersion != 1) {
    cat("\n")
    cat("Estimated dispersion parameter =", round(x$dispersion, x$digits), "\n")
  }

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}

##### svyglm ##################################################################

#' Complex survey regression summaries with options
#'
#' \code{summ} prints output for a regression model in a fashion similar to
#' \code{summary}, but formatted differently with more options.
#'
#' @param model A `svyglm` object.
#' @param odds.ratio If \code{TRUE}, reports exponentiated coefficients with
#'  confidence intervals for exponential models like logit and Poisson models.
#'  This quantity is known as an odds ratio for binary outcomes and incidence
#'  rate ratio for count models.
#'
#' @inheritParams summ.lm
#'
#' @details By default, this function will print the following items to the
#' console:
#' \itemize{
#'   \item The sample size
#'   \item The name of the outcome variable
#'   \item The (Pseudo-)R-squared value and AIC.
#'   \item A table with regression coefficients, standard errors, t-values, and
#'    p values.
#' }
#'
#'  The \code{scale} and \code{center} options are performed via refitting
#'  the model with \code{\link{scale_lm}} and \code{\link{center_lm}},
#'  respectively. Each of those in turn uses \code{\link{gscale}} for the
#'  mean-centering and scaling. These functions can handle \code{svyglm} objects
#'  correctly by calling \code{svymean} and \code{svyvar} to compute means and
#'  standard deviations. Weights are not altered. The fact that the model is
#'  refit means the runtime will be similar to the original time it took to fit
#'  the model.
#'
#' @return If saved, users can access most of the items that are returned in the
#'   output (and without rounding).
#'
#'  \item{coeftable}{The outputted table of variables and coefficients}
#'  \item{model}{The model for which statistics are displayed. This would be
#'    most useful in cases in which \code{scale = TRUE}.}
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
#' if (requireNamespace("survey")) {
#'   library(survey)
#'   data(api)
#'   dstrat <- svydesign(id = ~1, strata =~ stype, weights =~ pw,
#'                       data = apistrat, fpc =~ fpc)
#'   regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#'
#'   summ(regmodel)
#' }
#
#' @importFrom stats coef coefficients lm predict sd cooks.distance pf logLik
#'  extractAIC family fitted pt residuals terms model.weights poisson binomial
#' @export
#' @aliases j_summ.svyglm

summ.svyglm <- function(
  model, scale = FALSE,
  confint = FALSE, ci.width = .95,
  digits = getOption("jtools-digits", default = 2), pvals = TRUE,
  n.sd = 1, center = FALSE, transform.response = FALSE,
  odds.ratio = FALSE, vifs = FALSE,
  model.info = TRUE, model.fit = TRUE, model.check = FALSE,
  ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated arguments with helper function
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(any_deps)) {
    for (n in names(any_deps)[which(any_deps == FALSE)]) {
      # Reassign values as needed
      assign(n, deps[[n]])
    }
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

  # Checking for required package for VIFs to avoid problems
  if (vifs == TRUE) {
    if (!requireNamespace("car", quietly = TRUE)) {
      warning("When vifs is set to TRUE, you need to have the 'car' ",
              "package installed.\n Proceeding without VIFs...")
      vifs <- FALSE
    }
  }

  # Using information from summary()
  sum <- summary(model)

  # Check missing obs
  missing <- length(sum$na.action)

  if ("robust" %in% names(dots)) {
    warning("Robust standard errors are reported by default\n in the ",
            "survey package.")
  }

  # Standardized betas
  if (scale == TRUE || center == TRUE) {
    # Standardized betas
    if (scale == TRUE) {

      model <- scale_mod(model, n.sd = n.sd,
                         scale.response = transform.response)
      # Using information from summary()
      sum <- summary(model)

    } else if (center == TRUE && scale == FALSE) {

      model <- center_mod(model, center.response = transform.response)
      # Using information from summary()
      sum <- summary(model)

    }
  } else {

    design <- model$survey.design

  }

  j <- structure(j, standardize = scale, vifs = vifs, digits = digits,
                 model.info = model.info, model.fit = model.fit,
                 n.sd = n.sd, center = center, call = the_call,
                 env = the_env, scale = scale,
                 transform.response = transform.response)



  # Check if linear model
  if (model$family[1] == "gaussian" && model$family[2] == "identity") {
    linear <- TRUE
  } else {
    linear <- FALSE
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
    rsq <- mss/(mss + rss)
    arsq <- 1 - (1 - rsq) * ((n - df.int)/model$df.residual)

    j <- structure(j, rsq = rsq, arsq = arsq)
  } else { # If not linear, calculate pseudo-rsq

    ## Have to specify pR2 here to fix namespace issues
    svypR2 <- function(object) {

      llh <- suppressWarnings(logLik(object))
      fam <- family(object)$family
      link <- family(object)$link

      if (fam == "quasibinomial") {
        fam <- binomial(link = link)
      } else if (fam == "quasipoisson") {
        fam <- poisson(link = link)
      } else {
        fam <- family(object)
      }
      if (is.null(attr(terms(formula(object)),"offset"))) {
        objectNull <- suppressWarnings(update(object, ~ 1,
                                              design = object$survey.design,
                                              family = fam))
      } else {
        offs <- model.offset(model.frame(object))
        frame <- object$survey.design
        frame$variables$jtools_offs <- offs
        objectNull <- suppressWarnings(update(object, ~ 1 + offset(jtools_offs),
                                              design = frame, family = fam))
      }
      llhNull <- logLik(objectNull)
      n <- dim(object$model)[1]
      pR2Work(llh,llhNull,n)
    }

    # Final calculations (linear pseudo-rsq)
    pr2 <- suppressWarnings(svypR2(model))
    ## Cragg-Uhler
    rsq <-  pr2$r2CU
    ## McFadden
    rsqmc <- pr2$McFadden

    j <- structure(j, rsq = rsq, rsqmc = rsqmc)

    # AIC for GLMs
    j <- structure(j, aic = model$aic)
  }

  # List of names of predictors
  ivs <- names(coefficients(model))

  # Unstandardized betas
  ucoefs <- unname(coef(model))

  # VIFs
  if (vifs == TRUE) {
    if (model$rank == 2 | (model$rank == 1 & df.int == 0L)) {
      tvifs <- rep(NA, 1)
    } else {
      tvifs <- rep(NA, length(ivs))
      tvifs[-1] <- unname(car::vif(model))
    }
  }

  # Standard errors and t-statistics
  ses <- coef(sum)[,2]
  ts <- coef(sum)[,3]
  ps <- coef(sum)[,4]

  # Need proper name for test statistic
  tcol <- colnames(coef(sum))[3]
  tcol <- gsub("value", "val.", tcol)

  # Groundwork for CIs and ORs
  if (confint == TRUE | odds.ratio == TRUE) {

    alpha <- (1 - ci.width)/2
    tcrit <- abs(qnorm(alpha))

    lci_lab <- 0 + alpha
    lci_lab <- paste(round(lci_lab * 100, 1), "%", sep = "")

    uci_lab <- 1 - alpha
    uci_lab <- paste(round(uci_lab * 100, 1), "%", sep = "")

  }

  # Report odds ratios instead, with conf. intervals
  if (odds.ratio == TRUE) {

    ecoefs <- exp(ucoefs)
    lci <- exp(ucoefs - (ses * tcrit))
    uci <- exp(ucoefs + (ses * tcrit))
    params <- list(ecoefs, lci, uci, ts, ps)
    namevec <- c("Odds Ratio", lci_lab, uci_lab, tcol, "p")

  } else if (odds.ratio == FALSE & confint == TRUE) { # regular CIs

    lci <- ucoefs - (ses*tcrit)
    uci <- ucoefs + (ses*tcrit)
    params <- list(ucoefs, lci, uci, ts, ps)
    namevec <- c("Est.", lci_lab, uci_lab, tcol, "p")

  } else {

    params <- list(ucoefs, ses, ts, ps)
    namevec <- c("Est.", "S.E.", tcol, "p")

  }

  # Put things together

  if (vifs == TRUE) {
    params[length(params)+1] <- list(tvifs)
    namevec <- c(namevec, "VIF")
  }

  mat <- matrix(nrow=length(ivs), ncol=length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in seq_len(length(params))) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  # Dropping p-vals if requested by user
  if (pvals == FALSE) {

    mat <- mat[,!(colnames(mat) == "p")]

  }

  if (model.check == TRUE && linear == TRUE) {
    cd <- table(cooks.distance(model) > 4 / n)
    j <- structure(j, cooksdf = cd[2])

    if (model.check == TRUE && linear == FALSE) {
      warning("Model checking for non-linear models is not ",
              "yet implemented.")
    }
  }

  # Extract dispersion parameter
  dispersion <- sum$dispersion

  j <- structure(j, dv = names(model$model[1]),
                 npreds = model$rank-df.int,
                 dispersion = dispersion, missing = missing,
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 test.stat = tcol,
                 standardize.response = transform.response,
                 odds.ratio = odds.ratio,
                 scale.response = transform.response)

  j <- structure(j, lmFamily = model$family, model.check = model.check)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("summ.svyglm", "summ")
  return(j)

}

### PRINT METHOD ###

#' @export

print.summ.svyglm <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

    # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals)

  if (x$model.info == TRUE) {
    # Always showing this
    cat("MODEL INFO:", "\n", "Observations: ", x$n, sep = "")
    if (x$missing == 0) {
      cat("\n",
          "Dependent Variable: ",
          x$dv, "\n", sep = "")
    } else {
      cat(" (", x$missing, " missing obs. deleted)\n", sep = "")
    }
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
          "\n", "AIC = ", round(x$aic, x$digits), "\n\n", sep = "")
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

  print(ctable)

  if (x$dispersion != 1) {
    cat("\n")
    cat("Estimated dispersion parameter =", round(x$dispersion, x$digits),"\n")
  }

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}

##### merMod ##################################################################

#' Mixed effects regression summaries with options
#'
#' \code{summ} prints output for a regression model in a fashion similar to
#' \code{summary}, but formatted differently with more options.
#'
#' @param model A \code{\link[lme4]{merMod}} object.
#' @param r.squared Calculate an r-squared model fit statistic? Default is
#'  \code{TRUE}, but if it has errors or takes a long time to calculate you
#'  may want to consider setting to FALSE.
#' @param pvals Show p values and significance stars? If \code{FALSE}, these
#'  are not printed. Default is \code{TRUE}, except for merMod objects (see
#'  details).
#' @param odds.ratio If \code{TRUE}, reports exponentiated coefficients with
#'  confidence intervals for exponential models like logit and Poisson models.
#'  This quantity is known as an odds ratio for binary outcomes and incidence
#'  rate ratio for count models.
#' @param t.df For \code{lmerMod} models only. User may set the degrees of
#'  freedom used in conducting t-tests. See details for options.
#'
#' @inheritParams summ.lm
#'
#' @details By default, this function will print the following items to the
#' console:
#' \itemize{
#'   \item The sample size
#'   \item The name of the outcome variable
#'   \item The (Pseudo-)R-squared value and AIC/BIC.
#'   \item A table with regression coefficients, standard errors, and t-values.
#' }
#'
#'  The \code{scale} and \code{center} options are performed via refitting
#'  the model with \code{\link{scale_lm}} and \code{\link{center_lm}},
#'  respectively. Each of those in turn uses \code{\link{gscale}} for the
#'  mean-centering and scaling.
#'
#'  \code{merMod} models are a bit different than the others. The \code{lme4}
#'  package developers have, for instance, made a decision not to report or
#'  compute p values for \code{lmer} models. There are good reasons for this,
#'  most notably that the t-values produced are not "accurate" in the sense of
#'  the Type I error rate. For certain large, balanced samples with many
#'  groups, this is no big deal. What's
#'  a "big" or "small" sample? How much balance is necessary? What type of
#'  random effects structure is okay? Good luck getting a statistician to
#'  give you any clear guidelines on this.
#'  Some simulation studies have been done on fewer than 100 observations, so
#'  for sure if your sample is around 100 or fewer you should not interpret
#'  the t-values. A large number of groups is also crucial for avoiding bias
#'  using t-values. If groups are nested or crossed in a linear model,
#'  it is best to just get the \pkg{pbkrtest} package.
#'
#'  By default, this function follows \code{lme4}'s lead and does not report
#'  the p values for \code{lmer} models. If the user has \pkg{pbkrtest}
#'  installed, however, p values are reported using the Kenward-Roger
#'  d.f. approximation unless \code{pvals = FALSE} or \code{t.df} is
#'  set to something other than \code{NULL}. In publications,
#'  you should cite the
#'  Kenward & Roger (1997) piece as well as either this package or
#'  \pkg{pbkrtest} package to explain how the p values were calculated.
#'
#'  See \code{\link[lme4]{pvalues}} from the \pkg{lme4} for more details.
#'  If you're looking for a simple test with no extra packages installed,
#'  it is better to use the confidence
#'  intervals and check to see if they exclude zero than use the t-test.
#'  For users of \code{glmer}, see some of the advice there as well. While
#'  \code{lme4} and by association \code{summ} does as well, they are
#'  still imperfect.
#'
#'  You have some options to customize the output in this regard with the
#'  \code{t.df} argument. If \code{NULL}, the default, the
#'  degrees of freedom used depends on whether the user has \pkg{pbkrtest}
#'  installed. If installed, the Kenward-Roger approximation is used. If not,
#'  and user sets \code{pvals = TRUE}, then the residual degrees of freedom
#'  is used. If \code{t.df = "residual"}, then the residual d.f. is used
#'  without a message. If the user prefers to use some other method to
#'  determine the d.f., then any number provided as the argument will be
#'  used.
#'
#'  **About pseudo-R^2**
#'
#'  There is no one way to calculate R^2 for mixed models or nonlinear
#'  models. Many caution against interpreting or even using such
#'  approximations outside of OLS regression. With that said, this package
#'  reports one version for your benefit, though you should of course
#'  understand that it is not an unambiguous measure of model fit.
#'
#'  This package calculates R^2 for mixed models using an adapted version
#'  of \code{\link[piecewiseSEM]{sem.model.fits}} from the \pkg{piecewiseSEM}
#'  package. This is an implementation of the Nakagawa & Schielzeth (2013)
#'  procedure with refinements by Johnson (2014). If you choose to report
#'  the pseudo-R^2 in a publication, you should cite Nakagawa & Schielzeth
#'  to explain how the calculation was done.
#'
#' @return If saved, users can access most of the items that are returned in
#'   the output (and without rounding).
#'
#'  \item{coeftable}{The outputted table of variables and coefficients}
#'  \item{model}{The model for which statistics are displayed. This would be
#'    most useful in cases in which \code{scale = TRUE}.}
#'
#'  Much other information can be accessed as attributes.
#'
#' @seealso \code{\link{scale_lm}} can simply perform the standardization if
#'  preferred.
#'
#'  \code{\link{gscale}} does the heavy lifting for mean-centering and scaling
#'  behind the scenes.
#'
#'  \code{\link[pbkrtest]{get_ddf_Lb}} gets the Kenward-Roger degrees of
#'  freedom if you have \pkg{pbkrtest} installed.
#'
#'  A tweaked version of \code{\link[piecewiseSEM]{sem.model.fits}} is used to
#'  generate the pseudo-R-squared estimates for linear models.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @examples
#' if (requireNamespace("lme4")) {
#'   library(lme4, quietly = TRUE)
#'   data(sleepstudy)
#'   mv <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#'   summ(mv) # Note lack of p values if you don't have pbkrtest
#'
#'   # Without pbkrtest, you'll get message about Type 1 errors
#'   summ(mv, pvals = TRUE)
#'
#'   # To suppress message, manually specify t.df argument
#'   summ(mv, t.df = "residual")
#' }
#'
#' \dontrun{
#'  # Confidence intervals may be better alternative in absence of pbkrtest
#'  summ(mv, confint = TRUE)
#' }
#'
#' @references
#'
#' Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth’s
#'  $R^{2}_{GLMM}$ to random slopes models. \emph{Methods in Ecology and
#'  Evolution}, \emph{5}, 944–946. https://doi.org/10.1111/2041-210X.12225
#'
#' Kenward, M. G., & Roger, J. H. (1997). Small sample inference for fixed
#'  effects from restricted maximum likelihood. \emph{Biometrics},
#'  \emph{53}, 983.
#'  https://doi.org/10.2307/2533558
#'
#' Luke, S. G. (2017). Evaluating significance in linear mixed-effects models
#'  in R. \emph{Behavior Research Methods}, \emph{49}, 1494–1502.
#'  https://doi.org/10.3758/s13428-016-0809-y
#'
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
#'  obtaining $R^2$ from generalized linear mixed-effects models.
#'  \emph{Methods in Ecology and Evolution}, \emph{4}, 133–142.
#'  https://doi.org/10.1111/j.2041-210x.2012.00261.x
#'
#'
#'
#'
#' @importFrom stats coef coefficients lm predict sd cooks.distance pf logLik
#'  AIC BIC family fitted pt residuals terms model.weights
#' @export
#' @aliases j_summ.merMod
#'

summ.merMod <- function(
  model, scale = FALSE, confint = FALSE, ci.width = .95,
  digits = getOption("jtools-digits", default = 2), r.squared = TRUE,
  pvals = NULL, n.sd = 1, center = FALSE, transform.response = FALSE,
  data = NULL, odds.ratio = FALSE, t.df = NULL,
  model.info = TRUE, model.fit = TRUE,
  ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated arguments with helper function
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(any_deps)) {
    for (n in names(any_deps)[which(any_deps == FALSE)]) {
      # Reassign values as needed
      assign(n, deps[[n]])
    }
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

  # Accept arguments meant for other types of models and print warnings.

  if ("robust" %in% names(dots) && dots$robust == TRUE) {
    warning("Robust standard errors are not supported for mixed models.")
  }

  if ("model.check" %in% names(dots) && dots$model.check == TRUE) {
    warning("Model checking is not currently implemented for mixed ",
            "models.")
  }

  if ("vifs" %in% names(dots) && dots$vifs == TRUE) {
    warning("VIFs are not supported for mixed models.")
  }

  # If pbkrtest is installed, using the Kenward-Roger approximation
  if (requireNamespace("pbkrtest", quietly = TRUE)) {

    if (is.null(pvals)) {

      pvals <- TRUE
      pbkr <- TRUE

    } else if (pvals == TRUE) {

      pbkr <- TRUE

    } else if (pvals == FALSE) {

      pbkr <- FALSE

    }

  } else {

    pbkr <- FALSE

  }

  if (lme4::isGLMM(model)) {

    if (is.null(pvals)) {

      pvals <- TRUE

    }

    # Using pbkr as a stand-in for whether to calculate t-vals myself
    pbkr <- FALSE

  }

  # Standardized betas
  if (scale == TRUE) {

    model <- scale_mod(model, n.sd = n.sd, scale.response = transform.response,
                       data = data)

  } else if (center == TRUE && scale == FALSE) {

    model <- center_mod(model, center.response = transform.response,
                        data = data)

  }

  j <- structure(j, standardize = scale, digits = digits,
                 model.info = model.info, model.fit = model.fit,
                 n.sd = n.sd,
                 center = center, t.df = t.df, call = the_call, env = the_env,
                 scale = scale, transform.response = transform.response)

  # Using information from summary()
  sum <- summary(model)

  # Intercept?
  if (length(terms(model)) != attr(terms(model), "intercept")) {
    df.int <- if (attr(terms(model), "intercept"))
      1L
    else 0L
  }

  # Sample size used
  n <- length(residuals(model))
  j <- structure(j, n = n)

  # TODO: Figure out model fit indices for MLMs
  ## This is a start
  failed.rsq <- FALSE
  if (r.squared == TRUE) {

    t0 <- Sys.time() # Calculating time elapsed
    tryo <- try({rsqs <- suppressWarnings(pR2_merMod(model))}, silent = TRUE)
    t1 <- Sys.time()

    if (class(tryo) == "try-error") {

      rsqs <- NA
      failed.rsq <- TRUE
      r.squared <- FALSE
      warning("Could not calculate r-squared. Try removing missing data\n",
              "before fitting the model.\n")

    }

    if (failed.rsq == FALSE & (t1 - t0) > 5 &
        !getOption("pr2_warned", FALSE)) {

      message("If summ is taking too long to run, try setting ",
              "r.squared = FALSE.")
      options("pr2_warned" = TRUE)

    }
  } else {

    rsqs <- NA

  }

  # AIC for GLMs
  j <- structure(j, aic = AIC(model), bic = BIC(model), rsqs = rsqs)

  # List of names of predictors
  ivs <- rownames(sum$coefficients)

  # Unstandardized betas
  ucoefs <- unname(sum$coefficients[,1])

  # Standard errors and t-statistics
  ses <- sum$coefficients[,2]
  ts <- sum$coefficients[,3]

  # lmerMod doesn't have p values, so
  if (!sum$isLmer) {
    ps <- sum$coefficients[,4]
  } else {
    # Use Kenward-Roger if available
    if (pbkr == FALSE & is.null(t.df)) { # If not, do it like any lm

      df <- n - length(ivs) - 1

    } else if (pbkr == TRUE & is.null(t.df)) {

      t0 <- Sys.time()
      df <- pbkrtest::get_ddf_Lb(model, lme4::fixef(model))
      t1 <- Sys.time()

      if ((t1 - t0) > 5 & !getOption("pbkr_warned", FALSE)) {
        message("If summ is taking too long to run, try setting\n",
                "pvals = FALSE or t.df = 'residual' (or some number).")
        options("pbkr_warned" = TRUE)
      }

    } else if (!is.null(t.df)) {

      if (is.numeric(t.df)) {
        df <- t.df
      } else if (t.df %in% c("residual","resid")) {
        df <- n - length(ivs) - 1
      }

    }

    vec <- rep(NA, times = length(ts))
    for (i in seq_len(length(ts))) {
      p <- pt(abs(ts[i]), lower.tail = F, df)
      p <- p*2
      vec[i] <- p
    }

    ps <- vec

  }

  # Need proper name for test statistic
  tcol <- colnames(sum$coefficients)[3]
  tcol <- gsub("value", "val.", tcol)

  if (confint == TRUE | odds.ratio == TRUE) {

    alpha <- (1 - ci.width) / 2

    lci_lab <- 0 + alpha
    lci_lab <- paste(round(lci_lab * 100,1), "%", sep = "")

    uci_lab <- 1 - alpha
    uci_lab <- paste(round(uci_lab * 100,1), "%", sep = "")

  }

  # Report odds ratios instead, with conf. intervals
  if (odds.ratio == TRUE) {
  # TODO: revisit after lme4 bug fixed
    the_cis <-
      suppressWarnings(confint(model, parm = "beta_", method = "profile",
                       level = ci.width))
    the_cis <- the_cis[rownames(sum$coefficients),]
    ecoefs <- exp(ucoefs)
    lci <- exp(the_cis[,1])
    uci <- exp(the_cis[,2])
    params <- list(ecoefs, lci, uci, ts, ps)
    namevec <- c("Odds Ratio", lci_lab, uci_lab, tcol, "p")

  } else if (odds.ratio == FALSE & confint == TRUE) {

    the_cis <-
      suppressWarnings(confint(model, parm = "beta_", method = "profile",
                       level = ci.width))
    the_cis <- the_cis[rownames(sum$coefficients),]
    lci <- the_cis[,1]
    uci <- the_cis[,2]
    params <- list(ucoefs, lci, uci, ts, ps)
    namevec <- c("Est.", lci_lab, uci_lab, tcol, "p")

  } else {

    params <- list(ucoefs, ses, ts, ps)
    namevec <- c("Est.", "S.E.", tcol, "p")

  }

  # Put things together

  mat <- matrix(nrow = length(ivs), ncol = length(params))
  rownames(mat) <- ivs
  colnames(mat) <- namevec

  for (i in seq_len(length(params))) {
    if (is.numeric(params[[i]])) {
      mat[,i] <- params[[i]]
    } else {
      mat[,i] <- params[[i]]
    }
  }

  if (pvals == FALSE) {

    mat <- mat[, seq_len(ncol(mat) - 1)]

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
  for (i in seq_len(length(ngroups))) {
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
                 npreds = nrow(mat),
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 df = df, pbkr = pbkr, r.squared = r.squared,
                 failed.rsq = failed.rsq, test.stat = tcol,
                 standardize.response = transform.response,
                 odds.ratio = odds.ratio,
                 scale.response = transform.response)

  j <- structure(j, lmFamily = family(model))

  j$coeftable <- mat
  j$rcoeftable <- rcmat # Random effects table
  j$gvars <- gvmat # Grouping variables table
  j$model <- model
  class(j) <- c("summ.merMod", "summ")
  return(j)

}

### PRINT METHOD ###

#' @export

print.summ.merMod <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals)

  if (x$model.info == TRUE) {
    cat("MODEL INFO:", "\n", "Observations: ", x$n, "\n",
        "Dependent Variable: ",
        x$dv, "\n", sep = "")
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      cat("Type: Mixed effects linear regression", "\n\n")
    } else {
      cat("\nType: Mixed effects generalized linear regression", "\n",
          "Error Distribution: ", as.character(x$lmFamily[1]), "\n",
          "Link function: ",
          as.character(x$lmFamily[2]), "\n", "\n", sep = "")
    }
  }

  if (x$model.fit == T) {
    cat("MODEL FIT: ",
        "\n", "AIC = ", round(x$aic, x$digits),
        ", BIC = ", round(x$bic, x$digits), "\n", sep = "")
    if (x$r.squared == TRUE) {
      cat("Pseudo R-squared (fixed effects) = ", round(x$rsq$Marginal,
                                                       x$digits),
          "\n", sep = "")
      cat("Pseudo R-squared (total) = ", round(x$rsq$Conditional, x$digits),
          "\n\n", sep = "")
    } else {
      cat("\n")
    }
  }

  cat("FIXED EFFECTS:\n")
  print(ctable)
  ## Explaining the origin of the p values if they were used
  if (x$pvals == TRUE & lme4::isLMM(j$model)) {

    if (x$pbkr == FALSE & is.null(x$t.df)) {

      cat("\nNote: p values calculated based on residual d.f. =", x$df, "\n")

      message("Using p values with lmer based on residual d.f. may inflate\n",
              "the Type I error rate in many common study designs. \n",
              "Install package \"pbkrtest\" to get more accurate p values.")

    } else if (x$pbkr == TRUE & is.null(x$t.df)) {

      cat("\np values calculated using Kenward-Roger d.f. =",
          round(x$df, x$digits), "\n")

    } else if (!is.null(x$t.df)) {

      if (x$t.df %in% c("residual","resid")) {

        cat("\nNote: p values calculated based on residual d.f. =", x$df, "\n")

      } else {

        cat("\nNote: p values calculated based on user-defined d.f. =",
            x$df, "\n")

      }

    }

  }

  cat("\nRANDOM EFFECTS:\n")
  rtable <- round_df_char(j$rcoeftable, digits = x$digits)
  #rownames(rtable) <- rep("", times = nrow(rtable))
  print(rtable, row.names = FALSE)

  cat("\nGrouping variables:\n")
  gtable <- round_df_char(j$gvars, digits = x$digits)
  gtable[, "# groups"] <- as.integer(gtable[, "# groups"])
  #rownames(gtable) <- rep("", times = nrow(gtable))
  print(gtable, row.names = FALSE)

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}

#### utilities ###############################################################

#' @export

getCall.summ <- function(x, ...) {

  return(attr(x, "call"))

}

#' @export

update.summ <- function(object, ...) {

  call <- getCall(object)

  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  s_env <- attr(object, "env")
  eval(call, envir = s_env)

}

update_summ <- function(summ, call.env, ...) {

  call <- getCall(summ)

  # Assuming all models are the same type as first
  mod_type <- which(!is.null(sapply(class(summ$model),
                    utils::getS3method, f = "summ",
                    optional = T)))
  mod_type <- class(summ$model)[mod_type[1]]
  mod_type <- paste0("summ.", mod_type)

  # Now get the argument names for that version of summ
  summ_formals <- formals(getFromNamespace(mod_type, "jtools"))

  extras <- as.list(match.call())
  indices <- match(names(extras), names(summ_formals))
  extras <- extras[indices]

  for (i in 1:length(extras)) {
    if (is.name(extras[[i]])) {
      extras[[i]] <- eval(extras[[i]], envir = call.env)
    }
  }
  existing <- !is.na(match(names(extras), names(call)))
  for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
  if (any(!existing)) {
    call <- c(as.list(call), extras[!existing])
    call <- as.call(call)
  }

  env <- attr(summ, "env")

  call$model <- summ$model

  eval(call, env, parent.frame())

}

