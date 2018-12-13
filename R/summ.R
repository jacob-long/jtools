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
#'   \item \code{\link{summ.rq}}
#'
#' }
#'
#' @param model A \code{lm}, \code{glm}, \code{\link[survey]{svyglm}},
#'   \code{\link[lme4]{merMod}}, \code{\link[quantreg]{rq}} object.
#' @param ... Other arguments to be passed to the model-specific function.
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
#'  are not printed. Default is \code{TRUE}.
#'  
#' @param stars Show significance stars with p values? Default is FALSE. 
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
#' @param model.fit Toggles printing of model fit statistics.
#'
#' @param data If you provide the data used to fit the model here, that data
#'   frame is used to re-fit the model (if `scale` is `TRUE`)
#'   instead of the [stats::model.frame()]
#'   of the model. This is particularly useful if you have variable
#'   transformations or polynomial terms specified in the formula.
#'
#' @param which.cols Developmental feature. By providing columns by name,
#'   you can add/remove/reorder requested columns in the output. Not fully
#'   supported, for now.
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
  model, scale = FALSE, confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", .95),
  robust = getOption("summ-robust", FALSE), cluster = NULL,
  vifs = getOption("summ-vifs", FALSE),
  digits = getOption("jtools-digits", 2), pvals = getOption("summ-pvals", TRUE),
  stars = getOption("summ-stars", FALSE),
  n.sd = 1, center = FALSE, transform.response = FALSE, data = NULL,
  part.corr = FALSE, model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE), which.cols = NULL,  ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated arguments with helper function
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(!any_deps)) {
    for (n in names(any_deps)[which(any_deps == FALSE)]) {
      # Reassign values as needed
      assign(n, deps[[n]])
    }
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

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
                 model.fit = model.fit, n.sd = n.sd, center = center,
                 call = the_call, env = the_env, scale = scale, data = data,
                 transform.response = transform.response)

  # Intercept?
  if (model$rank != attr(model$terms, "intercept")) {
    df.int <- if (attr(model$terms, "intercept"))
      1L else 0L
  } else { # intercept only
    df.int <- 1
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
  coefs <- unname(coef(model))
  params <- list("Est." = coefs)

  # Model statistics
  fstat <- unname(sum$fstatistic[1])
  fnum <- unname(sum$fstatistic[2])
  fden <- unname(sum$fstatistic[3])
  if (!is.null(fstat)) {
    modpval <- pf(fstat, fnum, fden, lower.tail = FALSE)
  } else {modpval <- NULL}
  j <- structure(j, fstat = fstat, fnum = fnum, fden = fden, modpval = modpval)

  # VIFs
  if (vifs == TRUE) {
    tvifs <- rep(NA, length(ivs))
    the_vifs <- unname(vif(model))
    if (is.matrix(the_vifs)) {the_vifs <- the_vifs[,1]}
    tvifs[-1] <- the_vifs
    params[["VIF"]] <- tvifs
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

    ses <- rob_info$ses
    ts <- rob_info$ts
    ps <- rob_info$ps

    use_cluster <- rob_info$use_cluster
    robust <- rob_info$robust

  }

  # Handle rank-deficient models
  if (length(coefs) > length(ses)) {
    # Creating a vector the length of ucoefs (which has the NAs)
    temp_vec <- rep(NA, times = length(coefs))
    # Now I replace only at indices where ucoefs is non-missing
    temp_vec[which(!is.na(coefs))] <- ses
    # Now replace params[[i]] with the vector that includes the missings
    ses <- temp_vec
  }
  params[c("S.E.", "t val.", "p")] <- list(ses, ts, ps)

  # Alpha level to find critical t-statistic for confidence intervals
  alpha <- (1 - ci.width) / 2
  # Get the critical t
  tcrit <- abs(qt(alpha, df = df.residual(model)))
  # Make confidence interval labels
  labs <- make_ci_labs(ci.width)
  # Get the lower and upper CI bounds
  lci <- coefs - (ses * tcrit)
  uci <- coefs + (ses * tcrit)
  # Store cis in list
  cis <- list(lci, uci)
  names(cis) <- labs

  params[names(cis)] <- cis

  if (part.corr == TRUE) {

    pcs <- part_corr(ts, df.int, rsq, robust, n)

    partial_corr <- pcs$partial_corrs
    part_corr <- pcs$semipart_corrs
    params[c("partial.r", "part.r")] <- list(partial_corr, part_corr)

  }

  part.corr.arg <- if (part.corr) {c("partial.r", "part.r")} else {NULL}
  which.cols <- which_columns(which.cols = which.cols, confint = confint,
                              ci.labs = make_ci_labs(ci.width), vifs = vifs,
                              pvals = pvals, t.col = "t val.",
                              others = part.corr.arg)
  mat <- create_table(params = params, which.cols = which.cols, ivs = ivs)

  j <- structure(j, rsq = rsq, arsq = arsq, dv = names(model$model[1]),
                 npreds = model$rank - df.int, lmClass = class(model),
                 missing = missing, use_cluster = use_cluster,
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 test.stat = "t val.", stars = stars,
                 standardize.response = transform.response,
                 scale.response = transform.response,
                 transform.response = transform.response,
                 exp = FALSE)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("summ.lm", "summ")
  return(j)

}

### PRINT METHOD

#' @export
#' @importFrom crayon underline inverse italic

print.summ.lm <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals,
                      stars = x$stars)

  if (x$model.info == TRUE) {
    type <- paste("OLS linear regression")
    print_mod_info(missing = x$missing, n = x$n, dv = x$dv, type = type)
  }

  if (x$model.fit == T && !is.null(x$modpval)) {
    stats <- paste(italic("F"), "(", x$fnum, ",", x$fden, ") = ",
        num_print(x$fstat, digits = x$digits), ", ", italic("p"), " = ",
        num_print(x$modpval, digits = x$digits), "\n",
        italic("R\u00B2 = "), num_print(x$rsq, digits = x$digits), "\n",
        italic("Adj. R\u00B2 = "), num_print(x$arsq, digits = x$digits),
        sep = "")
    print_mod_fit(stats)
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
    cat(underline("MODEL CHECKING:"), "\n",
        "Homoskedasticity (Breusch-Pagan) = ",
        homoskedtf,
        "\n", "Number of high-leverage observations = ", x$cooksdf,
        "\n\n", sep = "")
  }

  print_se_info(x$robust, x$use_cluster, manual = "OLS")

  print(ctable)

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}

#' @title knitr methods for summ
#' @description There's no reason for end users to utilize these functions,
#' but CRAN requires it to be documented.
#' @param x The `summ` object
#' @param options Chunk options.
#' @param ... Ignored.
#' @rdname knit_print.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, summ.lm)
#' } else {
#'   export(knit_print.summ.lm)
#' }

knit_print.summ.lm <- function(x, options = NULL, ...) {

  if (!nzchar(system.file(package = "kableExtra")) |
      getOption("summ-normal-print", FALSE)) {
    return(knitr::normal_print(x))
  }

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals,
                      add_col = TRUE, stars = x$stars)

  format <- ifelse(knitr::is_latex_output(), yes = "latex", no = "html")
  o_opt <- getOption("kableExtra.auto_format", NULL)
  options(kableExtra.auto_format = FALSE)

  if (x$model.info == TRUE) {
    type <- paste("OLS linear regression")
    mod_info <-
      mod_info_list(missing = x$missing, n = x$n, dv = x$dv,
                    type = "OLS linear regression")
    obs <- mod_info$n
    if ("missing" %in% names(mod_info)) {
      obs <- paste0(obs, " (", mod_info$missing, " missing obs. deleted)")
    }
    mod_meta <- data.frame(
      datum = c("Observations", "Dependent variable", "Type"),
      value = c(obs, mod_info$dv, mod_info$type)
    )
    
    mod_meta %<>% to_kable(format = format, row.names = FALSE, col.names = NULL)

  } else {
    mod_meta <- NULL
  }

  if (x$model.fit == T && !is.null(x$modpval)) {
    stats <- data.frame(datum = c(paste0("F(", x$fnum, ",", x$fden, ")"),
                       "R\u00B2", "Adj. R\u00B2"),
                  value = c(num_print(x$fstat, digits = x$digits),
                       num_print(x$rsq, digits = x$digits),
                       num_print(x$arsq, digits = x$digits)),
                       stringsAsFactors = FALSE
                  )
    stats %<>% to_kable(format = format, row.names = FALSE, col.names = NULL)
  } else {stats <- NULL}

  se_info <- get_se_info(x$robust, x$use_cluster, manual = "OLS")
  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  ss <- if (!is.null(ss)) {paste(";", ss)} else {ss}
  cap <- paste0("Standard errors: ", se_info, ss)

  # pandoc turns single asterisks into a big dot
  if (format == "html") {ctable %<>% escape_stars()}
  ctable %<>% to_kable(format = format, row.names = TRUE, footnote = cap)

  out <- paste(mod_meta, stats, ctable, collapse = "\n\n")
  options(kableExtra.auto_format = o_opt)
  if (format == "latex") {
    return(knitr::asis_output(out, meta = kableExtra_latex_deps))
  }
  knitr::asis_output(out)

}

###### glm ####################################################################

#' Generalized linear regression summaries with options
#'
#' \code{summ} prints output for a regression model in a fashion similar to
#' \code{summary}, but formatted differently with more options.
#'
#' @param model A `glm` object.
#' @param exp If \code{TRUE}, reports exponentiated coefficients with
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
#'   \item The chi-squared test, (Pseudo-)R-squared value and AIC/BIC.
#'   \item A table with regression coefficients, standard errors, z values, and
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
  model, scale = FALSE, confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", .95),
  robust = getOption("summ-robust", FALSE), cluster = NULL,
  vifs = getOption("summ-vifs", FALSE),
  digits = getOption("jtools-digits", default = 2),
  exp = FALSE, pvals = getOption("summ-pvals", TRUE),
  stars = getOption("summ-stars", FALSE), n.sd = 1,
  center = FALSE, transform.response = FALSE, data = NULL,
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE), which.cols = NULL, ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated argument
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(!any_deps)) {
    for (n in names(any_deps)[which(any_deps == FALSE)]) {
      # Reassign values as needed
      assign(n, deps[[n]])
    }
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

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
      1L else 0L
  } else { # intercept only
    df.int <- 1
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
    chisq <- list(chi = pr$chisq, df = pr$chisq_df, p = pr$chisq_p)
  } else {
    rsq <- NULL
    rsqmc <- NULL
    chisq <- NULL
  }

  # AIC for GLMs
  j <- structure(j, aic = AIC(model), bic = BIC(model))

  # List of names of predictors
  ivs <- names(coefficients(model))

  # Unstandardized betas
  coefs <- unname(coef(model))
  params <- list("Est." = coefs)

  # VIFs
  if (vifs == TRUE) {
    tvifs <- rep(NA, length(ivs))
    the_vifs <- unname(vif(model))
    if (is.matrix(the_vifs)) {the_vifs <- the_vifs[,1]}
    tvifs[-1] <- the_vifs
    params[["VIF"]] <- tvifs
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

    ses <- rob_info$ses
    ts <- rob_info$ts
    ps <- rob_info$ps

    use_cluster <- rob_info$use_cluster
    robust <- rob_info$robust

  }

  # Handle rank-deficient models
  if (length(coefs) > length(ses)) {
    # Creating a vector the length of ucoefs (which has the NAs)
    temp_vec <- rep(NA, times = length(coefs))
    # Now I replace only at indices where ucoefs is non-missing
    temp_vec[which(!is.na(coefs))] <- ses
    # Now replace params[[i]] with the vector that includes the missings
    ses <- temp_vec
  }
  # Need proper name for test statistic
  tcol <- colnames(coef(sum))[3]
  tcol <- gsub("value", "val.", tcol)
  # Add SE, test statistic, p to params
  params[c("S.E.", tcol, "p")] <- list(ses, ts, ps)

  # Alpha level to find critical t-statistic for confidence intervals
  alpha <- (1 - ci.width) / 2
  # Get the critical t
  tcrit <- abs(qnorm(alpha))
  # Make confidence interval labels
  labs <- make_ci_labs(ci.width)

  # Report odds ratios instead, with conf. intervals
  if (exp == TRUE) {

    ecoefs <- exp(coefs)
    lci <- exp(coefs - (ses * tcrit))
    uci <- exp(coefs + (ses * tcrit))
    cis <- list(lci, uci)
    names(cis) <- labs
    params[["exp(Est.)"]] <- ecoefs
    params[names(cis)] <- cis
    if ("confint" %nin% names(the_call)) {confint <- TRUE}

  } else {

    lci <- coefs - (ses * tcrit)
    uci <- coefs + (ses * tcrit)
    cis <- list(lci, uci)
    names(cis) <- labs
    params[names(cis)] <- cis

  }

  ## TODO: finish margins implementation
  # if (margins == TRUE) {
  #   margs <- rep(NA, times = length(ivs))
  #   names(margs) <- ivs
  #   the_margs <- summary(margins::margins(model))
  #   which_coefs <- which(ivs %in% the_margs$factor)
  #   margs[which_coefs] <- the_margs$AME
  #   params[["A.M.E."]] <- margs
  # }

  # Put things together
  which.cols <- which_columns(which.cols = which.cols, confint = confint,
                              ci.labs = make_ci_labs(ci.width), vifs = vifs,
                              pvals = pvals, t.col = tcol,
                              exp = exp)
  mat <- create_table(params = params, which.cols = which.cols, ivs = ivs)

  # Extract dispersion parameter
  dispersion <- sum$dispersion

  j <- structure(j, rsq = rsq, rsqmc = rsqmc, dv = names(model$model[1]),
                 npreds = model$rank - df.int, dispersion = dispersion,
                 missing = missing, pvals = pvals, robust = robust,
                 robust.type = robust, use_cluster = use_cluster,
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 test.stat = tcol, stars = stars, 
                 standardize.response = transform.response,
                 exp = exp,
                 scale.response = transform.response,
                 lmFamily = model$family, chisq = chisq)

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
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals,
                      stars = x$stars)

  if (x$model.info == TRUE) {
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      type <- "Linear regression"
    } else {
      type <- paste("Generalized linear model\n ",
                    italic("Family:"),
                    as.character(x$lmFamily[1]), "\n ",
                    italic("Link function:"),
                    as.character(x$lmFamily[2]), sep = " ")
    }
    print_mod_info(missing = x$missing, n = x$n, dv = x$dv, type = type)
  }

  if (x$model.fit == TRUE) {
    stats <- paste("\u03C7\u00B2(",
                  x$chisq$df,  ") = ", num_print(x$chisq$chi, x$digits), ", ",
                  italic("p"), " = ", num_print(x$chisq$p, x$digits), "\n",
                   italic("Pseudo-R\u00B2 (Cragg-Uhler)"), " = ",
                   num_print(x$rsq, digits = x$digits), "\n",
                   italic("Pseudo-R\u00B2 (McFadden)"), " = ",
                   num_print(x$rsqmc, digits = x$digits), "\n",
                   italic("AIC"), " = ", num_print(x$aic, x$digits),
                   ", ", italic("BIC"), " = ", num_print(x$bic, x$digits),
                   sep = "")
    print_mod_fit(stats)
  }

  print_se_info(x$robust, x$use_cluster)

  print(ctable)

  if (x$dispersion != 1) {
    cat("\n")
    cat("Estimated dispersion parameter =", round(x$dispersion, x$digits), "\n")
  }

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}


#' @rdname knit_print.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, summ.glm)
#' } else {
#'   export(knit_print.summ.glm)
#' }

knit_print.summ.glm <- function(x, options = NULL, ...) {

  if (!nzchar(system.file(package = "kableExtra")) |       getOption("summ-normal-print", FALSE)) {
    return(knitr::normal_print(x))
  }

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals,
                      add_col = TRUE, stars = x$stars)

  format <- ifelse(knitr::is_latex_output(), yes = "latex", no = "html")
  o_opt <- getOption("kableExtra.auto_format", NULL)
  options(kableExtra.auto_format = FALSE)

  if (x$model.info == TRUE) {
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      type <- "Linear regression"
    } else {
      type <- "Generalized linear model"
    }
    mod_info <- mod_info_list(missing = x$missing, n = x$n, dv = x$dv,
                              type = type)
    obs <- mod_info$n
    if ("missing" %in% names(mod_info)) {
      obs <- paste0(obs, " (", mod_info$missing, " missing obs. deleted)")
    }

    if (type != "Linear regression") {
      mod_meta <- data.frame(
        datum = c("Observations", "Dependent variable", "Type", "Family",
                  "Link"),
        value = c(obs, mod_info$dv, mod_info$type, x$lmFamily[[1]],
                  x$lmFamily[[2]])
      )
    } else {
      mod_meta <- data.frame(
        datum = c("Observations", "Dependent variable", "Type"),
        value = c(obs, mod_info$dv, mod_info$type)
      )
    }
    
    mod_meta %<>% to_kable(format = format, row.names = FALSE, col.names = NULL)

  } else {
    mod_meta <- NULL
  }

  if (x$model.fit == T) {
    if (format != "latex" && Sys.info()[['sysname']] != "Windows") {
      chi <- "\U1D6D8\u00B2("
      # alternately -> "\U0001D712\u00B2("
    } else if (format == "latex") {
      chi <- "$\\chi^2$("
    } else if (format == "html") {
      chi <- "&chi;\u00B2("
    } else {
      chi <- "chi\u00B2("
    }
    stats <- data.frame(stat = c(paste0(chi, x$chisq$df,  ")"),
                                 "Pseudo-R\u00B2 (Cragg-Uhler)",
                                 "Pseudo-R\u00B2 (McFadden)",
                                 "AIC", "BIC"),
                        value = c(num_print(x$chisq$chi, x$digits),
                                  num_print(x$rsq, digits = x$digits),
                                  num_print(x$rsqmc, digits = x$digits),
                                  num_print(x$aic, x$digits),
                                  num_print(x$bic, x$digits)),
                        stringsAsFactors = FALSE
    )

    stats %<>% to_kable(format = format, row.names = FALSE, col.names = NULL,
                        escape = FALSE)
  
  } else {stats <- NULL}

  se_info <- get_se_info(x$robust, x$use_cluster)
  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  ss <- if (!is.null(ss)) {paste(";", ss)} else {ss}
  cap <- paste0("Standard errors: ", se_info, ss)

  if (format == "html") {ctable %<>% escape_stars()}
  ctable %<>% to_kable(format = format, row.names = TRUE, footnote = cap)

  out <- paste(mod_meta, stats, ctable, collapse = "\n\n")
  options(kableExtra.auto_format = o_opt)
  if (format == "latex") {
    kableExtra_latex_deps[[length(kableExtra_latex_deps) + 1]] <-
      list(name = "babel", options = c("greek", "english"))
    return(knitr::asis_output(out, meta = kableExtra_latex_deps))
  }
  knitr::asis_output(out)

}

##### svyglm ##################################################################

#' Complex survey regression summaries with options
#'
#' \code{summ} prints output for a regression model in a fashion similar to
#' \code{summary}, but formatted differently with more options.
#'
#' @param model A `svyglm` object.
#' @param exp If \code{TRUE}, reports exponentiated coefficients with
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
#'   \item A table with regression coefficients, standard errors, t values, and
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
  model, scale = FALSE, confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", .95),
  digits = getOption("jtools-digits", default = 2),
  pvals = getOption("summ-pvals", TRUE),
  stars = getOption("summ-stars", FALSE), n.sd = 1, center = FALSE, 
  transform.response = FALSE,
  exp = FALSE, vifs = getOption("summ-vifs", FALSE),
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE), which.cols = NULL, ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated arguments with helper function
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(!any_deps)) {
    for (n in names(any_deps)[which(any_deps == FALSE)]) {
      # Reassign values as needed
      assign(n, deps[[n]])
    }
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

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
      1L else 0L
  } else { # intercept only
    df.int <- 1
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
    mss <- if (df.int == 1L) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    } else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
    ## Final calculations
    rsq <- mss/(mss + rss)
    arsq <- 1 - (1 - rsq) * ((n - df.int)/model$df.residual)
    j <- structure(j, rsq = rsq, arsq = arsq)

  } else { # If not linear, calculate pseudo-rsq

    ## Have to specify pR2 here to avoid namespace issues
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
      pR2Work(llh, llhNull, n)
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
  coefs <- unname(coef(model))
  params <- list("Est." = coefs)

  # VIFs
  if (vifs == TRUE) {
    tvifs <- rep(NA, length(ivs))
    the_vifs <- unname(vif(model))
    if (is.matrix(the_vifs)) {the_vifs <- the_vifs[,1]}
    tvifs[-1] <- the_vifs
    params[["VIF"]] <- tvifs
  }

  # Standard errors and t-statistics
  ses <- coef(sum)[,2]
  ts <- coef(sum)[,3]
  ps <- coef(sum)[,4]

  # Need proper name for test statistic
  tcol <- colnames(coef(sum))[3]
  tcol <- gsub("value", "val.", tcol)

  params[c("S.E.", tcol, "p")] <- list(ses, ts, ps)

  # Groundwork for CIs and ORs
  alpha <- (1 - ci.width)/2
  if (linear == TRUE) {
    tcrit <- abs(qt(alpha, df = df.residual(model)))
  } else {
    tcrit <- abs(qnorm(alpha))
  }

  labs <- make_ci_labs(ci.width)

  # Report odds ratios instead, with conf. intervals
  if (exp == TRUE) {

    ecoefs <- exp(coefs)
    lci <- exp(coefs - (ses * tcrit))
    uci <- exp(coefs + (ses * tcrit))
    cis <- list(lci, uci)
    names(cis) <- labs
    params[["exp(Est.)"]] <- ecoefs
    params[names(cis)] <- cis

  } else { # regular CIs

    lci <- coefs - (ses * tcrit)
    uci <- coefs + (ses * tcrit)
    cis <- list(lci, uci)
    names(cis) <- labs
    params[names(cis)] <- cis

  }

  # Put things together
  which.cols <- which_columns(which.cols = which.cols, confint = confint,
                              ci.labs = make_ci_labs(ci.width), vifs = vifs,
                              pvals = pvals, t.col = tcol,
                              exp = exp)
  mat <- create_table(params = params, which.cols = which.cols, ivs = ivs)


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
                 test.stat = tcol, stars = stars,
                 standardize.response = transform.response,
                 exp = exp,
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
    # If it's linear...
    if (as.character(x$lmFamily[1]) == "gaussian" &&
        as.character(x$lmFamily[2]) == "identity") {
      # Just call it linear
      type <- paste("Survey-weighted linear regression", sep = "")
    } else {
      # Otherwise just treat it like glm
      type <- paste("Analysis of complex survey design", "\n",
                  italic("Family:"), as.character(x$lmFamily[1]),
                  "\n", italic("Link function:"), as.character(x$lmFamily[2]),
                  sep = " ")
    }
    print_mod_info(missing = x$missing, n = x$n, dv = x$dv, type = type)
  }

  if (x$model.fit == TRUE) { # Show fit statistics
    if (as.character(x$lmFamily[1]) == "gaussian" &&
        as.character(x$lmFamily[2]) == "identity") {
      # If it's a linear model, show regular lm fit stats
      stats <- paste(italic("R\u00B2"), " = ",
                     num_print(x$rsq, digits = x$digits), "\n",
                     italic("Adj. R\u00B2"), " = ",
                     num_print(x$arsq, digits = x$digits), sep = "")
    } else {
      # If it isn't linear, show GLM fit stats
      stats <- paste(italic("Pseudo-R\u00B2 (Cragg-Uhler)"), " = ",
                     num_print(x$rsq, digits = x$digits), "\n",
                     italic("Pseudo-R\u00B2 (McFadden)"), " = ",
                     num_print(x$rsqmc, digits = x$digits), "\n",
                     italic("AIC"), " = ", num_print(x$aic, x$digits), sep = "")
    }
    print_mod_fit(stats)
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

#' @rdname knit_print.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, summ.svyglm)
#' } else {
#'   export(knit_print.summ.svyglm)
#' }

knit_print.summ.svyglm <- function(x, options = NULL, ...) {

  if (!nzchar(system.file(package = "kableExtra")) |       getOption("summ-normal-print", FALSE)) {
    return(knitr::normal_print(x))
  }

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  format <- ifelse(knitr::is_latex_output(), yes = "latex", no = "html")
  o_opt <- getOption("kableExtra.auto_format", NULL)
  options(kableExtra.auto_format = FALSE)

  # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals,
                      add_col = TRUE, stars = x$stars)

  if (x$model.info == TRUE) {
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      type <- "Survey-weighted linear regression"
    } else {
      type <- "Survey-weighted generalized linear model"
    }
    mod_info <- mod_info_list(missing = x$missing, n = x$n, dv = x$dv,
                              type = type)
    obs <- mod_info$n
    if ("missing" %in% names(mod_info)) {
      obs <- paste0(obs, " (", mod_info$missing, " missing obs. deleted)")
    }

    if (type != "Survey-weighted linear regression") {
      mod_meta <- data.frame(
        datum = c("Observations", "Dependent variable", "Type", "Family",
                  "Link"),
        value = c(obs, mod_info$dv, mod_info$type, x$lmFamily[[1]],
                  x$lmFamily[[2]])
      )
    } else {
      mod_meta <- data.frame(
        datum = c("Observations", "Dependent variable", "Type"),
        value = c(obs, mod_info$dv, mod_info$type)
      )
    }

    mod_meta %<>% to_kable(format = format, row.names = FALSE, col.names = NULL)

  } else {
    mod_meta <- NULL
  }

  if (x$model.fit == T) {

    if (type != "Survey-weighted linear regression") {
      stats <- data.frame(stat = c("Pseudo-R\u00B2 (Cragg-Uhler)",
                                   "Pseudo-R\u00B2 (McFadden)",
                                   "AIC"),
                          value = c(num_print(x$rsq, digits = x$digits),
                                    num_print(x$rsqmc, digits = x$digits),
                                    num_print(x$aic, x$digits))
                          )
    } else {
      stats <- data.frame(stat = c("R\u00B2", "Adj. R\u00B2"),
                          value = c(num_print(x$rsq, digits = x$digits),
                                    num_print(x$arsq, digits = x$digits))
      )
    }

    stats %<>% to_kable(format = format, row.names = FALSE, col.names = NULL) 

  }

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  ss <- if (!is.null(ss)) {paste(";", ss)} else {ss}
  cap <- paste0("Standard errors: Robust", ss)

  if (format == "html") {ctable %<>% escape_stars()}
  ctable %<>% to_kable(format = format, row.names = TRUE, footnote = cap)

  out <- paste(mod_meta, stats, ctable, collapse = "\n\n")
  options(kableExtra.auto_format = o_opt)
  if (format == "latex") {
    kableExtra_latex_deps[[length(kableExtra_latex_deps) + 1]] <-
      list(name = "babel", options = c("greek", "english"))
    return(knitr::asis_output(out, meta = kableExtra_latex_deps))
  }
  knitr::asis_output(out)

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
#' @param exp If \code{TRUE}, reports exponentiated coefficients with
#'  confidence intervals for exponential models like logit and Poisson models.
#'  This quantity is known as an odds ratio for binary outcomes and incidence
#'  rate ratio for count models.
#' @param t.df For \code{lmerMod} models only. User may set the degrees of
#'  freedom used in conducting t-tests. See details for options.
#' @param re.variance Should random effects variances be expressed in
#'  standard deviations or variances? Default, to be consistent with previous
#'  versions of `jtools`, is `"sd"`. Use `"var"` to get the variance instead.
#' @param re.table Show table summarizing variance of random effects? Default
#'  is TRUE.
#' @param groups.table Show table summarizing the grouping variables? Default
#'  is TRUE.
#' @param conf.method Argument passed to [lme4::confint.merMod()]. Default
#'  is `"Wald"`, but `"profile"` or `"boot"` are better when accuracy is a
#'  priority. Be aware that both of the alternate methods are sometimes very
#'  time-consuming.
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
#'  degrees of freedom used depends on whether the user has
#'  \pkg{lmerTest} or \pkg{pbkrtest} installed. If `lmerTest` is installed,
#'  the degrees of freedom for each coefficient are calculated using the
#'  Satterthwaite method and the p values calculated accordingly.
#'  If only `pbkrtest` is installed or `t.df` is `"k-r"`, the Kenward-Roger
#'  approximation of the standard errors and degrees of freedom for each
#'  coefficient is used. Note that Kenward-Roger standard errors can take
#'  longer to calculate and may cause R to crash with models fit to large
#'  (roughly greater than 5000 rows) datasets.
#'
#'  If neither is installed and the user sets
#'  \code{pvals = TRUE}, then the residual degrees of freedom
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
#'  \item{rcoeftable}{The secondary table with the grouping variables and
#'    random coefficients.}
#'  \item{gvars}{The tertiary table with the grouping variables, numbers of
#'    groups, and ICCs.}
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
#'  [pbkrtest::get_ddf_Lb()] gets the Kenward-Roger degrees of
#'  freedom if you have \pkg{pbkrtest} installed.
#'
#'  A tweaked version of [piecewiseSEM::sem.model.fits()] is used to
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
#'   summ(mv) # Note lack of p values if you don't have lmerTest/pbkrtest
#'
#'   # Without lmerTest/pbkrtest, you'll get message about Type 1 errors
#'   summ(mv, pvals = TRUE)
#'
#'   # To suppress message, manually specify t.df argument
#'   summ(mv, t.df = "residual")
#'
#'   # Confidence intervals may be better alternative to p values
#'   summ(mv, confint = TRUE)
#'   # Use conf.method to get profile intervals (may be slow to run)
#'   # summ(mv, confint = TRUE, conf.method = "profile")
#'
#' }
#'
#' @references
#'
#' Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth's
#'  $R^{2}_{GLMM}$ to random slopes models. \emph{Methods in Ecology and
#'  Evolution}, \emph{5}, 944–946. \url{https://doi.org/10.1111/2041-210X.12225}
#'
#' Kenward, M. G., & Roger, J. H. (1997). Small sample inference for fixed
#'  effects from restricted maximum likelihood. \emph{Biometrics},
#'  \emph{53}, 983.
#'  \url{https://doi.org/10.2307/2533558}
#'
#' Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). lmerTest
#'  package: Tests in linear mixed effects models.
#'  *Journal of Statistical Software*, *82*.
#'  \url{https://doi.org/10.18637/jss.v082.i13}
#'
#' Luke, S. G. (2017). Evaluating significance in linear mixed-effects models
#'  in R. \emph{Behavior Research Methods}, \emph{49}, 1494–1502.
#'  \url{https://doi.org/10.3758/s13428-016-0809-y}
#'
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
#'  obtaining $R^2$ from generalized linear mixed-effects models.
#'  \emph{Methods in Ecology and Evolution}, \emph{4}, 133–142.
#'  \url{https://doi.org/10.1111/j.2041-210x.2012.00261.x}
#'
#'
#' @importFrom stats coef coefficients lm predict sd cooks.distance pf logLik
#'  AIC BIC family fitted pt residuals terms model.weights
#' @export
#' @aliases j_summ.merMod
#'

summ.merMod <- function(
  model, scale = FALSE, confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", .95),
  conf.method = getOption("summ-conf.method", c("Wald", "profile", "boot")),
  digits = getOption("jtools-digits", default = 2), r.squared = TRUE,
  pvals = getOption("summ-pvals", NULL), 
  stars = getOption("summ-stars", FALSE), n.sd = 1, center = FALSE,
  transform.response = FALSE, data = NULL, exp = FALSE, t.df = NULL,
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE),
  re.variance = getOption("summ-re.variance", c("sd", "var")),
  which.cols = NULL, re.table = getOption("summ-re.table", TRUE),
  groups.table = getOption("summ-groups.table", TRUE), ...) {

  j <- list()

  dots <- list(...)

  # Check for deprecated arguments with helper function
  deps <- dep_checks(dots)
  any_deps <- sapply(deps, is.null)
  if (any(!any_deps)) {
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

  # Get random effects variances argument
  re.variance <- match.arg(re.variance, c("sd", "var"), several.ok = FALSE)

  # Setting defaults
  manual_df <- FALSE
  pbkr <- FALSE
  satt <- FALSE
  if (is.null(pvals)) {
    if (is.null(t.df)) {
      if (requireNamespace("lmerTest", quietly = TRUE) |
          requireNamespace("pbkrtest", quietly = TRUE)) {
        pvals <- TRUE
      } else {
        pvals <- FALSE
      }
    } else {
      pvals <- TRUE
    }
  }

  if (pvals == TRUE) {
    if (is.null(t.df)) {
      if (requireNamespace("lmerTest", quietly = TRUE)) {
        satt <- TRUE
      } else if (requireNamespace("pbkrtest", quietly = TRUE)) {
        pbkr <- TRUE
      }
    } else {
      if (t.df %in% c("s", "satterthwaite", "Satterthwaite")) {
        if (requireNamespace("lmerTest", quietly = TRUE)) {
          satt <- TRUE
        } else {
          stop_wrap("You have requested Satterthwaite p values but you do
                    not have the lmerTest package installed.")
        }
      } else if (t.df %in% c("k-r", "kenward-roger", "Kenward-Roger")) {
        if (requireNamespace("pbkrtest", quietly = TRUE)) {
          pbkr <- TRUE
        } else {
          stop_wrap("You have requested Kenward-Roger p values but you do
                    not have the pbkrtest package installed.")
        }
      } else if (is.numeric(t.df) | t.df %in% c("resid", "residual")) {
        manual_df <- TRUE
      } else {
        stop_wrap("t.df argument not understood.")
      }

    }
  }

  if (lme4::isGLMM(model)) {

    if (is.null(pvals)) {

      pvals <- TRUE

    }

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

  # Model fit
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
  coefs <- unname(sum$coefficients[,1])
  params <- list("Est." = coefs)

  # Standard errors and t-statistics
  ses <- sum$coefficients[,2]
  ts <- sum$coefficients[,3]
  # Need proper name for test statistic
  tcol <- colnames(sum$coefficients)[3]
  tcol <- gsub("value", "val.", tcol)

  dfs <- NULL
  p_calc <- NULL

  # lmerMod doesn't have p values, so
  if (!sum$isLmer) {
    ps <- sum$coefficients[,4]
    params[["p"]] <- ps
  } else {
    if (satt == TRUE) {

      all_coefs <- get_all_sat(model)
      ts <- all_coefs[,"t value"]
      ses <- all_coefs[,"Std. Error"]
      dfs <- all_coefs[, "df"]
      params[["d.f."]] <- dfs
      ps <- all_coefs[, "Pr(>|t|)"]
      p_calc <- "s"

    } else if (pbkr == TRUE) {

      t0 <- Sys.time()
      # df <- pbkrtest::get_ddf_Lb(model, lme4::fixef(model))
      ses <- get_se_kr(model)
      # New t values with adjusted covariance matrix
      ts <- coefs[!is.na(coefs)] / ses
      dfs <- get_df_kr(model)
      params[["d.f."]] <- dfs
      ps <- pt(abs(ts), lower.tail = F, dfs)
      t1 <- Sys.time()

      if ((t1 - t0) > 10 & !getOption("pbkr_warned", FALSE)) {
        msg_wrap("If summ is taking too long to run, try setting pvals = FALSE,
                 t.df = 's' if you have the lmerTest package, or
                 t.df = 'residual' (or some number).")
        options("pbkr_warned" = TRUE)
      }

      p_calc <- "k-r"

    } else if (manual_df == TRUE) {

      if (is.numeric(t.df)) {
        df <- t.df
        p_calc <- "manual"
      } else if (t.df %in% c("residual","resid")) {
        df <- n - length(ivs) - 1
        p_calc <- "residual"
      }

      ps <- pt(abs(ts), lower.tail = F, df)


    }

    if (exists("ps")) {params[["p"]] <- ps}

  }

  params[c("Est.", "S.E.", tcol)] <- list(coefs, ses, ts)

  if (confint == TRUE | exp == TRUE) {

    alpha <- (1 - ci.width) / 2
    labs <- make_ci_labs(ci.width)

  }

  # Report odds ratios instead, with conf. intervals
  if (exp == TRUE) {
  # TODO: revisit after lme4 bug fixed
    the_cis <-
      suppressWarnings(confint(model, parm = "beta_", method = conf.method[1],
                       level = ci.width))
    the_cis <- the_cis[rownames(sum$coefficients),]
    ecoefs <- exp(coefs)
    lci <- exp(the_cis[,1])
    uci <- exp(the_cis[,2])
    cis <- list(lci, uci)
    names(cis) <- labs
    params[["exp(Est.)"]] <- ecoefs
    params[names(cis)] <- cis

  } else if (exp == FALSE & confint == TRUE) {

    the_cis <-
      suppressWarnings(confint(model, parm = "beta_", method = conf.method[1],
                       level = ci.width))
    the_cis <- the_cis[rownames(sum$coefficients),]
    lci <- the_cis[,1]
    uci <- the_cis[,2]
    cis <- list(lci, uci)
    names(cis) <- labs
    params[names(cis)] <- cis

  }

  # Put things together
  which.cols <- which_columns(which.cols = which.cols, confint = confint,
                              ci.labs = make_ci_labs(ci.width), vifs = FALSE,
                              pvals = pvals, t.col = tcol,
                              exp = exp,
                              df = !is.null(dfs))
  mat <- create_table(params = params, which.cols = which.cols, ivs = ivs)

  # Dealing with random effects
  ## Names of grouping vars
  groups <- names(sum$ngrps)
  ## Number of groups
  ngroups <- sum$ngrps
  ## Calculate ICCs w/ internal function from sjstats
  iccs <- icc(model)

  tables <- get_re_tables_mer(model = model, re.variance = re.variance,
                              groups = groups, ngroups = ngroups,
                              iccs = iccs)

  j <- structure(j, groups = groups, ngroups = ngroups, iccs = iccs,
                 vcnames = names(iccs), dv = names(model.frame(model)[1]),
                 npreds = nrow(mat),
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 df = df, p_calc = p_calc, r.squared = r.squared,
                 failed.rsq = failed.rsq, test.stat = tcol, stars = stars,
                 standardize.response = transform.response,
                 exp = exp, scale.response = transform.response,
                 re.table = re.table, groups.table = groups.table)

  j <- structure(j, lmFamily = family(model))

  j$coeftable <- as.table(mat)
  j$rcoeftable <- tables$rcmat # Random effects table
  j$gvars <- tables$gvmat # Grouping variables table
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
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals,
                      stars = x$stars)

  if (x$model.info == TRUE) {
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      type <- "Mixed effects linear regression"
    } else {
      type <- paste("Mixed effects generalized linear regression", "\n",
                    italic("Error Distribution: "),
                    as.character(x$lmFamily[1]), "\n",
                    italic("Link function: "), as.character(x$lmFamily[2]),
                    sep = "")
    }
    print_mod_info(missing = x$missing, n = x$n, dv = x$dv, type = type)
  }

  if (x$model.fit == T) {
    stats <- paste(italic("AIC"), " = ", num_print(x$aic, x$digits),
                   ", ", italic("BIC"), " = ",
                   num_print(x$bic, x$digits), sep = "")
    if (x$r.squared == TRUE) {
      stats <- paste(stats, "\n", italic("Pseudo-R\u00B2 (fixed effects)"),
                     " = ", num_print(x$rsq$Marginal, x$digits), "\n",
                     italic("Pseudo-R\u00B2 (total)"), " = ",
                     num_print(x$rsq$Conditional, x$digits), sep = "")
    }
    print_mod_fit(stats)
  }

  cat(underline("FIXED EFFECTS:\n"))
  if ("d.f." %in% names(ctable)) {
    ctable[,"d.f."] <- as.integer(ctable[,"d.f."])
  }
  print(ctable)
  ## Explaining the origin of the p values if they were used
  if (x$pvals == TRUE & lme4::isLMM(j$model)) {

    if (x$p_calc == "residual") {

      cat(italic$cyan("\nNote: p values calculated based on residual d.f. =",
          x$df, "\n"))

      if (is.null(x$t.df)) {
        msg_wrap("Using p values with lmer based on residual d.f. may inflate
                the Type I error rate in many common study designs.
                Install package \"lmerTest\" and/or \"pbkrtest\" to get more
                accurate p values.", brk = "\n")
      }

    } else if (x$p_calc %in% c("k-r", "Kenward-Roger", "kenward-roger")) {

      cat("\n")
      cat_wrap(italic$cyan("p values calculated using Kenward-Roger standard
                           errors and d.f."), brk = "\n")

    } else if (x$p_calc %in% c("s", "Satterthwaite", "satterthwaite")) {

      cat("\n")
      cat_wrap(italic$cyan("p values calculated using Satterthwaite
                      d.f."), brk = "\n")

    } else if (x$p_calc == "manual") {

      cat(italic("\nNote: p values calculated based on user-defined d.f. ="),
          x$df, "\n")

    }

  }

  if (x$re.table == TRUE) {
    cat(underline("\nRANDOM EFFECTS:\n"))
    rtable <- round_df_char(j$rcoeftable, digits = x$digits, na_vals = "")
    #rownames(rtable) <- rep("", times = nrow(rtable))
    print(rtable, row.names = FALSE)
  }

  if (x$groups.table == TRUE) {
    cat(underline("\nGrouping variables:\n"))
    gtable <- round_df_char(j$gvars, digits = x$digits, na_vals = "")
    gtable[, "# groups"] <- as.integer(gtable[, "# groups"])
    #rownames(gtable) <- rep("", times = nrow(gtable))
    print(gtable, row.names = FALSE)
  }

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}

#' @rdname knit_print.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, summ.merMod)
#' } else {
#'   export(knit_print.summ.merMod)
#' }

knit_print.summ.merMod <- function(x, options = NULL, ...) {

  if (!nzchar(system.file(package = "kableExtra")) |       getOption("summ-normal-print", FALSE)) {
    return(knitr::normal_print(x))
  }

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  # Helper function to deal with table rounding, significance stars
  ctable <- add_stars(table = j$coeftable, digits = x$digits, p_vals = x$pvals,
                      add_col = TRUE, stars = x$stars)

  format <- ifelse(knitr::is_latex_output(), yes = "latex", no = "html")
  if (length(format) == 0) {format <- "html"}
  o_opt <- getOption("kableExtra.auto_format", NULL)
  options(kableExtra.auto_format = FALSE)

  if (x$model.info == TRUE) {
    if (x$lmFamily[1] == "gaussian" && x$lmFamily[2] == "identity") {
      type <- "Mixed effects linear regression"
    } else {
      type <- "Mixed effects generalized linear model"
    }
    mod_info <- mod_info_list(missing = x$missing, n = x$n, dv = x$dv,
                              type = type)
    obs <- mod_info$n
    if ("missing" %in% names(mod_info)) {
      obs <- paste0(obs, " (", mod_info$missing, " missing obs. deleted)")
    }

    if (type != "Mixed effects linear regression") {
      mod_meta <- data.frame(
        datum = c("Observations", "Dependent variable", "Type", "Family",
                  "Link"),
        value = c(obs, mod_info$dv, mod_info$type, x$lmFamily[[1]],
                  x$lmFamily[[2]])
      )
    } else {
      mod_meta <- data.frame(
        datum = c("Observations", "Dependent variable", "Type"),
        value = c(obs, mod_info$dv, mod_info$type)
      )
    }
    
    mod_meta %<>% to_kable(format = format, col.names = NULL)

  } else {mod_meta <- NULL}

  if (x$model.fit == T) {

    stats <- data.frame(stat = c("AIC", "BIC"),
                        value = c(num_print(x$aic, x$digits),
                                  num_print(x$bic, x$digits))
    )

    if (x$r.squared == TRUE) {
      stats <- data.frame(stat = c("AIC", "BIC",
                                   "Pseudo-R\u00B2 (fixed effects)",
                                   "Pseudo-R\u00B2 (total)"),
                          value = c(num_print(x$aic, x$digits),
                                    num_print(x$bic, x$digits),
                                    num_print(x$rsq$Marginal, x$digits),
                                    num_print(x$rsq$Conditional, x$digits))
      )
    }

    stats %<>% to_kable(format = format, col.names = NULL)

  } else {stats <- NULL}

  cap <- NULL

  # Handling p-value explanation
  if (x$pvals == TRUE & lme4::isLMM(j$model)) {

    if (x$p_calc == "residual") {

      p_stmt <- paste("Note: p values calculated based on residual d.f. =",
                      x$df)

      if (is.null(x$t.df)) {
        msg_wrap("Using p values with lmer based on residual d.f. may inflate
                the Type I error rate in many common study designs.
                Install package \"lmerTest\" and/or \"pbkrtest\" to get more
                accurate p values.", brk = "\n")
      }

    } else if (x$p_calc %in% c("k-r", "Kenward-Roger", "kenward-roger")) {

      p_stmt <- paste("p values calculated using Kenward-Roger standard",
                      "errors and d.f.")

    } else if (x$p_calc %in% c("s", "Satterthwaite", "satterthwaite")) {

      p_stmt <- paste("p values calculated using Satterthwaite d.f.")

    } else if (x$p_calc == "manual") {

      p_stmt <- paste("Note: p values calculated based on user-defined d.f. =",
                      x$df)

    }

    cap <- paste(cap, p_stmt)

  }

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  ss <- if (!is.null(ss)) {paste0("; ", ss)} else {ss}
  cap <- paste(cap, ss)

  num_cols <- ncol(ctable)
  if (format == "html") {ctable %<>% escape_stars()}
  ctable %<>% to_kable(format = format, cols = num_cols + 1,
                       caption = "Fixed Effects", footnote = cap,
                       row.names = TRUE)

  if (x$re.table == TRUE) {
    rtable <- round_df_char(j$rcoeftable, digits = x$digits, na_vals = "")
    rtable %<>% to_kable(format = format, row.names = FALSE, 
                         cols = ncol(j$rcoeftable), caption = "Random Effects",
                         html = FALSE)
  } else {rtable <- NULL}

  if (x$groups.table == TRUE) {
    gtable <- round_df_char(j$gvars, digits = x$digits, na_vals = "")
    gtable[, "# groups"] <- as.integer(gtable[, "# groups"])

    gtable %<>% to_kable(format = format, cols = ncol(j$gvars), 
                         caption = "Grouping Variables", html = FALSE)
  } else {gtable <- NULL}

  out <- paste(mod_meta, stats, ctable, rtable, gtable, collapse = "\n\n")
  options(kableExtra.auto_format = o_opt)
  if (format == "latex") {
    return(knitr::asis_output(out, meta = kableExtra_latex_deps))
  }
  knitr::asis_output(out)

}

#### utilities ###############################################################

#' @title Set defaults for `summ` function
#'
#' @description This function is convenience wrapper for manually setting
#'  options using [options()]. This gives a handy way to, for instance,
#'  set the arguments to be used in every call to `summ` in your script/session.
#'
#'  To make the settings persist across sessions, you can run this in your
#'  `.Rprofile` file.
#'
#'  Note that arguments that do not apply (e.g., `robust` for `merMod` models)
#'  are silently ignored when those types of models are used.
#'
#' @inheritParams summ.lm
#' @inheritParams summ.merMod
#'
#' @export
#'

set_summ_defaults <- function(digits = NULL, model.info = NULL,
                              model.fit = NULL, pvals = NULL, robust = NULL,
                              confint = NULL, ci.width = NULL, vifs = NULL,
                              conf.method = NULL) {

  if ("confint" %in% names(match.call())) {
    options("summ-confint" = confint)
  }
  if ("ci.width" %in% names(match.call())) {
    options("summ-ci.width" = ci.width)
  }
  if ("model.info" %in% names(match.call())) {
    options("summ-model.info" = model.info)
  }
  if ("model.fit" %in% names(match.call())) {
    options("summ-model.fit" = model.fit)
  }
  if ("robust" %in% names(match.call())) {
    options("summ-robust" = robust)
  }
  if ("vifs" %in% names(match.call())) {
    options("summ-vifs" = vifs)
  }
  if ("digits" %in% names(match.call())) {
    options("jtools-digits" = digits)
  }
  if ("pvals" %in% names(match.call())) {
    options("summ-pvals" = pvals)
  }
  if ("conf.method" %in% names(match.call())) {
    options("summ-conf.method" = pvals)
  }

}

#' @export

getCall.summ <- function(x, ...) {

  return(attr(x, "call"))

}

# #' @export

# update.summ <- function(object, ...) {
#
#   call <- getCall(object)
#
#   extras <- match.call(expand.dots = FALSE)$...
#   if (length(extras)) {
#     existing <- !is.na(match(names(extras), names(call)))
#     for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
#     if (any(!existing)) {
#       call <- c(as.list(call), extras[!existing])
#       call <- as.call(call)
#     }
#   }
#   s_env <- attr(object, "env")
#   eval(call, envir = s_env)
#
# }

update_summ <- function(summ, call.env, ...) {

  call <- getCall(summ)

  # Now get the argument names for that version of summ
  summ_formals <- formals(getFromNamespace(class(summ), "jtools"))

  extras <- as.list(match.call())
  indices <- which(names(extras) %in% names(summ_formals))
  extras <- extras[indices]

  for (i in seq_along(extras)) {
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

# Use summ over a list of models and return them.
# If model isn't supported, just return it.
summs <- function(models, ...) {

  # Create empty list to hold tidy frames
  the_summs <- as.list(rep(NA, times = length(models)))

  ex_args <- list(...)

  for (i in seq_along(models)) {

    method_stub <- find_S3_class("summ", models[[i]], package = "jtools")

    if (!is.na(method_stub)) {
      # Get the right summ function's arguments
      method_args <- formals(getS3method("summ", method_stub))
      # Because deprecated args aren't in formals, I add them here
      dep_names <- c("standardize", "scale.response", "standardize.response",
                     "center.response", "robust.type")
      # Match the args
      extra_args <- ex_args[names(ex_args) %in% c(names(method_args),
                                                  dep_names)]
      if (!is.null(extra_args)) {
        extra_args <- lapply(extra_args, function(x) {
          if (length(x) > 1) {return(x[[i]])} else {return(x)}
        })
      }

      all_args <- as.list(c(unname(models[i]), extra_args))

      the_summs[[i]] <- do.call(getS3method("summ", method_stub),
                                args = all_args)

    } else {

      the_summs[[i]] <- models[[i]]

    }

  }

  return(the_summs)

}
