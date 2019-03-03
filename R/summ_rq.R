#' @title Quantile regression summaries with options
#'
#' @description \code{summ()} prints output for a regression model in a fashion
#'  similar to \code{summary()}, but formatted differently with more options.
#'
#' @param model A `rq` model. At this time, `rqs` models (multiple `tau`
#'  parameters) are not supported.
#'
#' @param se One of "nid", "rank", "iid", "ker", or "boot". "nid" is default.
#'  See [quantreg::summary.rq()] documentation for more about these options.
#'
#' @param boot.sims If `se = "boot"`, the number of bootstrap replications to
#'  perform. This is passed as the `R` argument to `boot.rq`
#'
#' @param boot.method If `se = "boot"`, the type of bootstrapping method to
#'  use. Default is "xy", but see [quantreg::boot.rq()] for more options.
#'
#' @inheritParams summ.lm
#'
#' @details This method implements most of the things I think most users would
#'  asking `summary.rq` for. `hs`, `U`, and `gamma` are ignored.
#'
#'  Note that when using `se = "rank"`, there are no standard errors,
#'  test statistics, or p values calculated.
#'
#'  About the R1 fit statistic: Described in Koenker \& Machado (1999), this
#'  offers an interpretation similar to R-squared in OLS regression. While you
#'  could calculate R-squared for these models, it goes against the underlying
#'  theoretical rationale for them. Koenker himself is not a big fan of R1
#'  either, but it's something. See Koenker \& Machado (1999) for more info.
#'
#' @family summ
#'
#' @references
#'
#' Koenker, R., & Machado, J. A. F. (1999). Goodness of fit and related
#' inference processes for quantile regression.
#' *Journal of the American Statistical Association*, *94*, 1296–1310.
#' https://doi.org/10.1080/01621459.1999.10473882
#'
#' @examples
#'
#' if (requireNamespace("quantreg")) {
#'  library(quantreg)
#'  data(engel)
#'  fitrq <- rq(income ~ foodexp, data = engel, tau = 0.5)
#'  summ(fitrq)
#' }
#'
#'
#' @export
#'

summ.rq <- function(model, scale = FALSE,
  confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", .95),
  se = c("nid", "rank", "iid", "ker", "boot"),
  boot.sims = 1000, boot.method = "xy",
  vifs = getOption("summ-vifs", FALSE),
  digits = getOption("jtools-digits", 2), pvals = getOption("summ-pvals", TRUE),
  n.sd = 1, center = FALSE, transform.response = FALSE, data = NULL,
  model.info = getOption("summ-model.info", TRUE),
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

  # Warn about robust arg
  if ("robust" %in% names(dots)) {
    warn_wrap("The robust argument is not supported for rq objects. Use the
              se argument instead.")
  }

  if (se[1] == "rank") {
    # Rank test doesn't generate p values, standard errors
    confint <- TRUE
    pvals <- FALSE
  }

  the_call <- match.call()
  the_call[[1]] <- substitute(summ)
  the_env <- parent.frame(n = 2)

  # Alpha level to find critical t-statistic for confidence intervals
  alpha <- (1 - ci.width) / 2

  # Check missing obs
  missing <- length(model$na.action)

  # Standardized betas
  if (scale == TRUE) {
    model <- scale_mod(model, n.sd = n.sd,
                       scale.response = transform.response,
                       data = data)
  } else if (center == TRUE && scale == FALSE) {
    model <- center_mod(model, center.response = transform.response,
                        data = data)
  }

  # Using info from summary.rq
  if (se[1] == "boot") {
    sum <- summary(model, covariance = TRUE, se = se[1],
                   R = boot.sims, bsmethod = boot.method)
  } else {
    sum <- summary(model, covariance = TRUE, se = se[1], alpha = alpha)
  }

  j <- structure(j, standardize = scale, vifs = vifs, robust = FALSE,
                 digits = digits, model.info = model.info,
                 model.fit = model.fit,
                 n.sd = n.sd, center = center, call = the_call,
                 env = the_env, scale = scale, data = data,
                 transform.response = transform.response,
                 boot.sims = boot.sims, boot.method = boot.method)

  # Intercept?
  if (length(attr(model$terms, "order")) != 0) {
    df.int <- if (attr(model$terms, "intercept"))
      1L else 0L
  } else { # intercept only
    df.int <- 1
  }

  # Sample size used
  n <- length(model$residuals)
  j <- structure(j, n = n)

  # List of names of predictors
  ivs <- names(coef(model))

  # Unstandardized betas
  coefs <- unname(coef(model))
  params <- list("Est." = coefs)

  # Get R1 (Koenker & Machado, 1999)
  r1 <- R1(model)
  # AIC/BIC
  aic = AIC(model)
  bic = AIC(model, k = log(n))

  # List of names of predictors
  ivs <- names(coefficients(model))

  # Betas
  coefs <- unname(coef(model))
  params <- list("Est." = coefs)

  # VIFs
  if (vifs == TRUE) {
    tvifs <- rep(NA, length(ivs))
    the_vifs <- unname(vif(model, vcov = sum$cov,
                           mod.matrix = rq_model_matrix(model)))
    if (is.matrix(the_vifs)) {the_vifs <- the_vifs[,1]}
    tvifs[-1] <- the_vifs
    params[["VIF"]] <- tvifs
  }

  # Standard errors and t-statistics
  if (se[1] != "rank") {
    ses <- coef(sum)[,2]
    ts <- coef(sum)[,3]
    ps <- coef(sum)[,4]
  } else {
    ses <- rep(NA, nrow(coef(sum)))
    ts <- rep(NA, nrow(coef(sum)))
    ps <- rep(NA, nrow(coef(sum)))
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

  if (se[1] != "rank") {
    # Get the critical t
    tcrit <- abs(qt(alpha, df = sum$rdf))
    # Make confidence interval labels
    labs <- make_ci_labs(ci.width)
    # Get the lower and upper CI bounds
    lci <- coefs - (ses * tcrit)
    uci <- coefs + (ses * tcrit)
    # Store cis in list
    cis <- list(lci, uci)
    names(cis) <- labs
  } else {
    cis <- list(coef(sum)[, "lower bd"], coef(sum)[, "upper bd"])
    names(cis) <- make_ci_labs(ci.width)
  }

  params[names(cis)] <- cis

  which.cols <- which_columns(which.cols = which.cols, confint = confint,
                              ci.labs = make_ci_labs(ci.width), vifs = vifs,
                              pvals = pvals, t.col = "t val.")
  if (se[1] == "rank") {which.cols <- which.cols[which.cols %nin% "t val."]}
  mat <- create_table(params = params, which.cols = which.cols, ivs = ivs)

  j <- structure(j, r1 = r1, dv = names(model$model[1]), lmClass = class(model),
                 missing = missing, use_cluster = FALSE,
                 confint = confint, ci.width = ci.width, pvals = pvals,
                 test.stat = "t val.", stars = stars,
                 standardize.response = transform.response,
                 scale.response = transform.response,
                 transform.response = transform.response,
                 exp = FALSE, se = se[1], aic = aic, bic = bic)

  j$coeftable <- mat
  j$model <- model
  class(j) <- c("summ.rq", "summ")
  return(j)

}

### PRINT METHOD

#' @export
#' @importFrom crayon underline inverse italic

print.summ.rq <- function(x, ...) {

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  ctable <- j$coeftable

  if (x$model.info == TRUE) {

    method <-
      switch(j$model$method,
             "br" = "Barrodale-Roberts",
             "fn" = "Frisch-Newton",
             "pfn" = "Frisch-Newton (after pre-processing)",
             "sfn" = "Frisch-Newton (sparse algebra)",
             "lasso" = "lasso",
             "scad" = "Fan-Li SCAD",
             "fnc" = "Frisch-Newton (user-specified equality constraints)")

    type <- paste0("Quantile regression",
                   "\n  ", italic("Quantile (tau): "), j$model$tau, "\n  ",
                   italic("Method: "), method)
    print_mod_info(missing = x$missing, n = x$n, dv = x$dv, type = type)
  }

  if (x$model.fit == TRUE) {
    stats <- paste(italic("R\u00B9"), paste0("(", j$model$tau, ")"), " = ",
                   num_print(x$r1, digits = x$digits), sep = "")
    print_mod_fit(stats)
  }

  se_name <- switch(x$se,
                    "iid" = "IID",
                    "nid" = "Sandwich (Huber)",
                    "ker" = "Sandwich (kernel)",
                    "boot" = "bootstrap",
                    "rank" = "Koenker rank test")

  print_se_info(x$robust, x$use_cluster, manual = se_name)
  print(md_table(ctable, format = getOption("summ.table.format", "multiline"),
                 sig.digits = FALSE))

  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  if (!is.null(ss)) {cat("\n", ss, "\n", sep = "")}

}

#' @rdname knit_print.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, summ.rq)
#' } else {
#'   export(knit_print.summ.rq)
#' }

knit_print.summ.rq <- function(x, options = NULL, ...) {

  if (!nzchar(system.file(package = "kableExtra")) |
      getOption("summ-normal-print", FALSE)) {
    return(knitr::normal_print(x))
  }

  # saving input object as j
  j <- x
  # saving attributes as x (this was to make a refactoring easier)
  x <- attributes(j)

  format <- ifelse(knitr::is_latex_output(), yes = "latex", no = "html")
  o_opt <- getOption("kableExtra.auto_format", NULL)
  options(kableExtra.auto_format = FALSE)

  # Helper function to deal with table rounding
  ctable <- round_df_char(df = j$coeftable, digits = x$digits)

  # context <- huxtable::guess_knitr_output_format()
  # if (context == "") {context <- "screen"}

  if (x$model.info == TRUE) {
    method <-
      switch(j$model$method,
             "br" = "Barrodale-Roberts",
             "fn" = "Frisch-Newton",
             "pfn" = "Frisch-Newton (after pre-processing)",
             "sfn" = "Frisch-Newton (sparse algebra)",
             "lasso" = "lasso",
             "scad" = "Fan-Li SCAD",
             "fnc" = "Frisch-Newton (user-specified equality constraints)")

    mod_info <-
      mod_info_list(missing = x$missing, n = x$n, dv = x$dv,
                    type = "Quantile regression")
    obs <- mod_info$n
    if ("missing" %in% names(mod_info)) {
      obs <- paste0(obs, " (", mod_info$missing, " missing obs. deleted)")
    }
    mod_meta <- data.frame(
      datum = c("Observations", "Dependent variable", "Type",
                "  Quantile (tau)", "  Method"
                ),
      value = c(obs, mod_info$dv, mod_info$type, j$model$tau, method)
    )

    mod_meta %<>% to_kable(format = format, row.names = FALSE, col.names = NULL)

  } else {
    mod_meta <- NULL
  }

  if (x$model.fit == T && !is.null(x$modpval)) {
    stats <- paste(italic("R\u00B9"), paste0("(", j$model$tau, ")"), " = ",
                   num_print(x$r1, digits = x$digits), sep = "")
    stats <- data.frame(stat = c(paste0("R\u00B9 ", "(", j$model$tau, ")")),
                        value = c(num_print(x$r1, digits = x$digits))
    )

    stats %<>% to_kable(format = format, row.names = FALSE, col.names = NULL)

  } else {stats <- NULL}

  se_name <- switch(x$se,
                    "iid" = "IID",
                    "nid" = "Sandwich (Huber)",
                    "ker" = "Sandwich (kernel)",
                    "boot" = "bootstrap",
                    "rank" = "Koenker rank test")
  se_info <- get_se_info(x$robust, x$use_cluster, manual = se_name)
  # Notifying user if variables altered from original fit
  ss <- scale_statement(x$scale, x$center, x$transform.response, x$n.sd)
  ss <- if (!is.null(ss)) {paste(";", ss)} else {ss}
  cap <- paste0("Standard errors: ", se_info, ss)

  if (format == "html") {ctable %<>% escape_stars()}
  ctable %<>% to_kable(format = format, row.names = TRUE, footnote = cap)

  out <- paste(mod_meta, stats, ctable, collapse = "\n\n")
  options(kableExtra.auto_format = o_opt)
  if (format == "latex") {
    return(knitr::asis_output(out, meta = kableExtra_latex_deps))
  }
  knitr::asis_output(out)

}

#### Quantile regression helpers ##############################################

# Get R1 (Koenker & Machado, 1999)
#' @importFrom stats model.response
R1 <- function(model) {
  rho_1 <- model$rho
  null_resids <- model.frame(model)[[1]] - quantile(model.frame(model)[[1]],
                                                    model$tau)
  
  rho_0 <- sum(null_resids * (model$tau - (null_resids < 0)))
  
  return(1 - (rho_1 / rho_0))
}

rq_model_matrix <- function(object) {
  mt <- terms(object)
  m <- model.frame(object)
  y <- model.response(m)
  if (object$method == "sfn")
    x <- object$model$x
  else x <- model.matrix(mt, m, contrasts = object$contrasts)
  return(x)
}

rq.fit.br <- function(x, y, tau = 0.5, alpha = 0.1, ci = FALSE,
                      iid = TRUE, interp = TRUE, tcrit = TRUE, ...) {
  
  rq.fit.br(x, y, tau = tau, alpha = alpha, ci = ci, iid = iid,
            interp = interp, tcrit = tcrit)
  
}


#' @rdname glance.summ

glance.summ.rq <- function(x, ...) {

  m <- x$model

  n <- length(fitted(m))
  s <- summary(m, se = attr(x, "se"))
  base <- data.frame(tau = m[["tau"]], logLik = logLik(m), AIC = AIC(m),
             BIC = AIC(m, k = log(n)),
             df.residual = rep(s[["rdf"]], times = length(m[["tau"]])))

  base$r.1 <- attr(x, "r1")
  return(base)

}

#' @export

nobs.summ.rq <- function(object, ...) {

  return(length(fitted((object$model))))

}
