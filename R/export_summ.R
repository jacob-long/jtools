#' @title Export regression summaries to tables
#'
#' @description This function allows users to use the features of
#'   [summ()] (e.g., standardization, robust standard errors)
#'   in the context of shareable HTML, LaTeX, and
#'   Microsoft Word tables. It relies heavily on [huxtable::huxreg()]
#'   to do the table formatting. This is particularly useful for putting
#'   the results of multiple models into a single table.
#'
#' @param ... At minimum, a regression object(s). See details for more
#'   arguments.
#' @param error_format Which of standard error, confidence intervals, test
#'   statistics, or p values should be used to express uncertainty of estimates
#'   for regression coefficients? See details for more info.
#'   Default: "({std.error})"
#' @param error_pos Where should the error statistic defined in
#'   \code{error_style} be placed relative to the coefficient estimate?
#'   Default: "below"
#' @param ci_level If reporting confidence intervals, what should the
#'   confidence level be? By default, it is .95 if
#'   confidence intervals are requested in \code{error_format}.
#' @param statistics Which model summary statistics should be included?
#'   See \code{\link[huxtable]{huxreg}} for more on usage. The default
#'   for this function depends on the model type. See details for more on
#'   the defaults by model type.
#' @param model.names If you want to give your model(s) names at the top
#'   of each column, provide them here as a character vector.
#'   Otherwise, they will just be labeled by number. Default: NULL
#' @param coefs If you want to include only a subset of the coefficients in
#'   the table, specify them here in a character vector. If you want the
#'   table to show different names for the coefficients, give a named vector
#'   where the names are the preferred coefficient names. See details for more.
#' @param to.file Export the table to a Microsoft Word, PDF, or HTML document?
#'   This functionality relies on `huxtable`'s `quick_` functions
#'   ([huxtable::quick_docx()], [huxtable::quick_pdf()],
#'   [huxtable::quick_html()], [huxtable::quick_xlsx()]). Acceptable arguments
#'   are "Word" or "docx" (equivalent), "pdf", "html", or "xlsx". All are
#'   case insensitive. Default is NULL, meaning the table is not saved.
#' @param file.name File name with (optionally) file path to save the
#'   file. Ignored if `to.file` is FALSE. Default: NULL
#'
#' @return A `huxtable`.
#'
#' @details
#'
#'   There are many optional parameters not documented above. Any
#'   argument that you would want to pass to [summ()], for instance,
#'   will be used. Of particular interest may be the robust and scale
#'   arguments. Note that some \code{summ} arguments may not have any bearing
#'   on the table output.
#'
#'   The default model summary statistics reporting follows this logic:
#'
#'   \itemize{
#'     \item summ.lm = `c(N = "nobs", R2 = "r.squared")`,
#'     \item summ.glm = \code{c(N = "nobs", AIC = "AIC", BIC = "BIC",
#'                        `Pseudo R2` = "pseudo.r.squared")},
#'     \item summ.svyglm = `c(N = "nobs", R2 = "r.squared")`,
#'     \item summ.merMod = \code{c(N = "nobs", AIC = "AIC", BIC = "BIC",
#'                           `R2 (fixed)` = "r.squared.fixed",
#'                           `R2 (total)` = "r.squared")}
#'     \item summ.rq = `c(N = "nobs", tau = "tau", R1 = "r.1",
#'                        AIC = "AIC", BIC = "BIC")`
#'   }
#'
#'   Be sure to look at the [summ()] documentation for more on the calculation
#'   of these and other statistics, especially for mixed models.
#'
#'   If you set `statistics = "all"`, then the statistics argument
#'   passed to `huxreg` will be `NULL`, which reports whichever
#'   model statistics are available via `glance`. If you want no
#'   model summary statistics, set the argument to `character(0)`.
#'
#'   You have a few options for the `error_format` argument.
#'   You can include anything returned by [broom::tidy()]
#'   (see also [tidy.summ()]). For the most part, you will
#'   be interested in `std.error` (standard error), `statistic`
#'   (test statistic, e.g. t-value or z-value), `p.value`, or
#'   `conf.high` and `conf.low`, which correspond to the
#'   upper and lower bounds of the confidence interval for the estimate.
#'   Note that the default `ci_level` argument is .95, but you
#'   can alter that as desired.
#'
#'   To format the error statistics, simply put the statistics desired in
#'   curly braces wherever you want them in a character string. For example,
#'   if you want the standard error in parentheses, the argument would be
#'   `"({std.error})"`, which is the default. Some other ideas:
#'
#'   * `"({statistic})"`, which gives you the test statistic in
#'   parentheses.
#'
#'   * `"({statistic}, p = {p.value})"`, which gives the test
#'   statistic followed by a "p =" p value all in parentheses. Note that
#'   you'll have to pay special attention to rounding if you do this to keep
#'   cells sufficiently narrow.
#'
#'   * `"[{conf.low}, {conf.high}]"`, which gives the confidence
#'   interval in the standard bracket notation. You could also explicitly
#'   write the confidence level, e.g.,
#'    `"CI [{conf.low}, {conf.high}]"`.
#'
#'   For `coefs`, the argument is slightly different than what is default
#'   in `huxreg`. If you provide a named vector of coefficients, then
#'   the table will refer to the selected coefficients by the names of the
#'   vector rather than the coefficient names. For instance, if I want to
#'   include only the coefficients for the `hp` and `mpg` but have
#'   the table refer to them as "Horsepower" and "Miles/gallon", I'd provide
#'   the argument like this:
#'   `c("Horsepower" = "hp", "Miles/gallon" = "mpg")`
#'
#'   You can also pass any argument accepted by the
#'   [huxtable::huxreg()] function. A few that are likely to be
#'   oft-used are documented above, but visit `huxreg`'s documentation
#'   for more info.
#'
#'   For info on converting the [huxtable::huxtable()] object to
#'   HTML or LaTeX, see `huxtable`'s documentation.
#'
#' @examples
#' states <- as.data.frame(state.x77)
#' fit1 <- lm(Income ~ Frost, data = states)
#' fit2 <- lm(Income ~ Frost + Illiteracy, data = states)
#' fit3 <- lm(Income ~ Frost + Illiteracy + Murder, data = states)
#'
#' if (requireNamespace("huxtable")) {
#'   # Export all 3 regressions with "Model #" labels,
#'   # standardized coefficients, and robust standard errors
#'   export_summs(fit1, fit2, fit3,
#'                model.names = c("Model 1","Model 2","Model 3"),
#'                coefs = c("Frost Days" = "Frost",
#'                          "% Illiterate" = "Illiteracy",
#'                          "Murder Rate" = "Murder"),
#'                scale = TRUE, robust = TRUE)
#' }
#'
#' @seealso
#'
#'  \code{\link{summ}}
#'
#'  \code{\link[huxtable]{huxreg}}
#'
#' @export
#' @importFrom utils getFromNamespace

export_summs <- function(...,
                         error_format = "({std.error})",
                         error_pos = c("below", "right", "same"),
                         ci_level = .95, statistics = NULL,
                         model.names = NULL, coefs = NULL,
                         to.file = NULL,
                         file.name = NULL) {

  if (!requireNamespace("huxtable", quietly = TRUE)) {
    stop_wrap("Install the huxtable package to use the export_summs function.")
  }

  if (!requireNamespace("broom", quietly = TRUE)) {
    stop_wrap("Install the broom package to use the export_summs function.")
  }

  # Capture arguments
  dots <- list(...)

  # If first element of list is a list, assume the list is a list of models
  if (inherits(dots[[1]], 'list')) {

    mods <- dots[[1]]
    if (is.null(model.names) & !is.null(names(mods))) {
      model.names <- names(mods)
    }
    if (length(dots) > 1) {
      dots <- dots[-1]
    } else {dots <- NULL}

  } else {
  # Otherwise assume unnamed arguments are models and everything else is args
    if (!is.null(names(dots))) {
      mods <- dots[names(dots) == ""]
      dots <- dots[names(dots) != ""]
    } else {
      mods <- dots
      dots <- NULL
    }
  }

  # # Setting defaults for summ functions I want to add captions about
  robust <- getOption("summ-robust", FALSE)
  scale <- FALSE
  n.sd <- 1
  digits <- getOption("jtools-digits", 2)

  # For those critical arguments that require a note, see if they were
  # provided by the user and overwrite if so
  if ("robust" %in% names(dots)) {robust <- dots$robust}
  robust <- ifelse(all(robust != FALSE), yes = TRUE, no = FALSE)
  if ("scale" %in% names(dots)) {scale <- dots$scale}
  scale <- ifelse(all(scale == TRUE), yes = TRUE, no = FALSE)
  if ("n.sd" %in% names(dots)) {n.sd <- dots$n.sd}
  n.sd <- ifelse(length(unique(n.sd)) == 1, yes = n.sd[1], no = NULL)
  if (is.null(n.sd)) {scale <- FALSE}
  if ("digits" %in% names(dots)) {digits <- dots$digits}

  jsumms <- do.call("summs", as.list(c(list(mods), dots)))

  # If user gave model names, apply them now
  if (!is.null(model.names)) {

    names(jsumms) <- model.names

  } else {

    names(jsumms) <- paste("Model", seq_along(jsumms))

  }

  ## Setting statistics option
  if (is.null(statistics)) {

    # Create empty vector
    stats <- c()
    # Iterate through each model
    for (i in seq_along(jsumms)) {

      # Get class of model
      mod_type <- class(jsumms[[i]])[1]
      # If a summ object, get its default statistics
      statistics <- switch(mod_type,
             summ.lm = c(N = "nobs", R2 = "r.squared"),
             summ.glm = c(N = "nobs", AIC = "AIC", BIC = "BIC",
                          `Pseudo R2` = "pseudo.r.squared"),
             summ.svyglm = c(N = "nobs", R2 = "r.squared"),
             summ.rq = c(N = "nobs", tau = "tau", R1 = "r.1",
                         AIC = "AIC", BIC = "BIC"),
             summ.merMod = NULL
      )
      if (mod_type == "summ.merMod") { # Going to do merMod separately
        grp_names <- names(lme4::ranef(jsumms[[i]]$model))
        group_ns <- c()
        for (j in seq_along(grp_names)) {
          glance_name <- paste0("group.nobs.", grp_names[j])
          group_ns <- c(group_ns, glance_name)
          names(group_ns)[j] <- paste0("N (", grp_names[j], ")")
        }
        statistics <- c(N = "nobs", group_ns, AIC = "AIC", BIC = "BIC",
          `R2 (fixed)` = "r.squared.fixed", `R2 (total)` = "r.squared")
      }
      # Append those statistics to the vector
      stats <- c(stats, statistics)
    }
    # Keep just the unique entries
    statistics <- stats[!duplicated(stats)]

  } else if (identical(statistics,"all")) {

    statistics <- NULL

  }

  hux_formals <- formals(huxtable::huxreg)

  hux_args <- dots[names(dots) %in% names(hux_formals)]

  # if (dots$confint == TRUE) {
  hux_args <- as.list(c(jsumms, hux_args,
                        error_pos = error_pos[1],
                        error_format = error_format,
                        statistics = list(statistics),
                        ci_level = ci_level,
                        coefs = if (is.null(coefs)) {NULL} else {list(coefs)})
                      )

  if ("note" %nin% names(hux_args)) {
    if (scale == TRUE) {
      note <- paste("All continuous predictors are mean-centered and scaled",
              "by",  n.sd[1], "standard",  ifelse(n.sd > 1,
                                                  no = "deviation.",
                                                  yes = "deviations."))

      if (robust == TRUE) {
        note <- paste(note,
                  "Standard errors are heteroskedasticity robust. %stars%.")
      } else {
        note <- paste(note, "{stars}.")
      }

      hux_args$note <- note

    } else if (robust == TRUE & scale == FALSE) {
      note <- paste("Standard errors are heteroskedasticity robust. %stars%.")
      hux_args$note <- note
    }
  }

  if ("number_format" %nin% names(hux_args)) {
    hux_args$number_format <- paste("%.", digits, "f", sep = "")
  }

  out <- do.call(what = huxtable::huxreg, args = hux_args)
  # Reformat group-level Ns to be expressed as integers
  if (any(grepl("N (", out[[1]], fixed = TRUE))) {
    out <-
      huxtable::set_number_format(out,
                                  which(grepl("N (", out[[1]], fixed = TRUE)),
                                  2:(length(mods) + 1), "%7g")
  }

  if (!is.null(dots$to.word) && dots$to.word == TRUE) {
    warn_wrap('The to.word argument is deprecated. Use to.file = "word" or
              to.file = "docx" instead.')
    to.file <- "docx"
  }

  if (!is.null(dots$word.file)) {
    warn_wrap("The word.file argument is deprecated. Use file.name instead.")
    file.name <- dots$word.file
  }

  if (!is.null(to.file)) {
    # Make lowercase for ease and consistency with huxtable quick_ funs
    to.file <- tolower(to.file)
    if (to.file == "word") {to.file <- "docx"}

    if (to.file %nin% c("pdf", "html", "docx", "xlsx")) {
      warn_wrap('to.file must be one of "docx", "html", "pdf", or "xlsx" if
                you want to write to a file. File not written.')
    }
    if (to.file %in% c("docx", "xlsx")) {
      if (!nzchar(system.file(package = "officer"))) {
        stop_wrap("You need the 'officer' package to write to 
                  Microsoft Word or Excel.")
      }
      if (!nzchar(system.file(package = "flextable"))) {
        stop_wrap("You need the 'flextable' package to write to 
                  Microsoft Word or Excel.")
      }
    }
    if (is.null(file.name)) {
      msg_wrap("You did not provide a file name, so it will be called
               untitled.", to.file)
      file.name <- paste0("untitled.", to.file)
    }
    # Weird header is included in Word files
    # if (to.file == "docx") {out$header <- NULL}
    # Create the function name based on file type
    fun_name <- paste0("quick_", to.file)
    # Call the function
    do.call(fun_name, list(out, file = file.name),
            envir = getNamespace("huxtable"))

  }

  return(out)

}

#' @rdname glance.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'  S3method(generics::tidy, summ)
#' } else {
#'  export(tidy.summ)
#' }

tidy.summ <- function(x, conf.int = FALSE, conf.level = .95, ...) {

  if (all(c("summ.rq", "summ.svyglm") %nin% class(x))) {
    base <- generics::tidy(x$model, conf.int = conf.int,
                          conf.level = conf.level, ...)
  } else if ("summ.rq" %in% class(x)) {
    dots <- list(...)
    dots <- dots[names(dots) %in% c(names(formals("rq")),
                                    names(formals("rq.fit.br")),
                                    "alpha")]

    args <- as.list(c(list(x$model), conf.int = conf.int,
                      conf.level = conf.level, se.type = attr(x, "se"), dots))

    base <- do.call(generics::tidy, args)
  } else {
    base <- generics::tidy(x$model, conf.int = conf.int, 
                           conf.level = conf.level)
  }

  if ("S.E." %in% colnames(x$coeftable)) {
    # If conf.int == TRUE, summ does not have a S.E. column
    base$std.error[!is.na(base$std.error)] <- x$coeftable[,"S.E."]

  }

  # Need to find the statistic column without knowing what it will be called
  stat_col <- attributes(x)$test.stat

  # Filtering to not NA avoids issues with NAs being filled with
  # repetitive values due to replacement being multiple of its length

  if ("statistic" %in% names(base)) {
    base$statistic[!is.na(base$statistic)] <- x$coeftable[,stat_col]
  }

  if ("p" %in% colnames(x$coeftable)) {

    base[["p.value"]][!is.na(base$statistic)] <- x$coeftable[,"p"]

  } else if (!("p.value" %in% names(base))) {

    base[["p.value"]] <- NA

  }

  if (attr(x, "exp") == TRUE) {

    base$estimate <- x$coeftable[,"exp(Est.)"]

  }

  if (attributes(x)$confint == TRUE | conf.int == TRUE) {

    labs <- make_ci_labs(conf.level)
    lci_lab <- labs$lci
    uci_lab <- labs$uci

    if (attributes(x)$confint == TRUE & attributes(x)$ci.width == conf.level) {
      base$conf.low <- x$coeftable[,lci_lab]
      base$conf.high <- x$coeftable[,uci_lab]
    } else {
      conf.level <- as.numeric(deparse(conf.level))
      e <- environment()
      x <- update_summ(x, confint = TRUE, ci.width = conf.level, call.env = e)
      base$conf.low <- x$coeftable[,lci_lab]
      base$conf.high <- x$coeftable[,uci_lab]
    }

  }

  if (!is.null(attr(x, "coef_export"))) {

    the_coefs <- attr(x, "coef_export")
    base <- base[base$term %in% the_coefs,]
    if (!is.null(names(the_coefs))) {
      for (i in seq_along(the_coefs)) {
        if (names(the_coefs[i]) != "") {
          base$term[base$term == the_coefs[i]] <- names(the_coefs[i])
        }
      }
    }

  }
  
  if (attr(x, "vifs") == TRUE) {
    base$vif <- x$coeftable[,"VIF"]
  }
  
  # Moved later to avoid errors when there are undefined coefficients
  if ("statistic" %in% names(base) && any(is.na(base[["statistic"]]))) {
    
    base <- base[!is.na(base$statistic),]
    
  }

  num_cols <- sapply(base, is.numeric)
  zeroes <- check_if_zero(base[num_cols])
  if (any(zeroes == TRUE)) {
    basenums <- base[num_cols]
    basenums[zeroes] <- 0
    base[num_cols] <- basenums
  }

  return(base)

}

#' @rdname glance.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'  S3method(generics::tidy, summ.merMod)
#' } else {
#'  export(tidy.summ.merMod)
#' }

tidy.summ.merMod <- function(x, conf.int = FALSE, conf.level = .95, ...) {

  base <- generics::tidy(x$model, conf.int = conf.int,
                         conf.level = conf.level, ...)
  
  # Deleting random effects
  if ("statistic" %in% names(base) && any(is.na(base[["statistic"]]))) {
    base <- base[!is.na(base$statistic),]
  }
  
  if ("S.E." %in% colnames(x$coeftable)) {
    # If conf.int == TRUE, summ does not have a S.E. column
    base$std.error[!is.na(base$std.error)] <- x$coeftable[,"S.E."]
    
  }
  
  # Need to find the statistic column without knowing what it will be called
  stat_col <- attributes(x)$test.stat
  
  # Filtering to not NA avoids issues with NAs being filled with
  # repetitive values due to replacement being multiple of its length
  if ("statistic" %in% names(base)) {
    base$statistic[!is.na(base$statistic)] <- x$coeftable[,stat_col]
  }
  
  if ("p" %in% colnames(x$coeftable)) {
    
    base[["p.value"]][!is.na(base$statistic)] <- x$coeftable[,"p"]
    
  } else if (!("p.value" %in% names(base))) {
    
    base[["p.value"]] <- NA
    
  }
  
  if (attr(x, "exp") == TRUE) {
    
    base$estimate <- x$coeftable[,"exp(Est.)"]
    
  }
  
  if (attributes(x)$confint == TRUE | conf.int == TRUE) {
    
    labs <- make_ci_labs(conf.level)
    lci_lab <- labs$lci
    uci_lab <- labs$uci
    
    if (attributes(x)$confint == TRUE & attributes(x)$ci.width == conf.level) {
      base$conf.low <- x$coeftable[,lci_lab]
      base$conf.high <- x$coeftable[,uci_lab]
    } else {
      conf.level <- as.numeric(deparse(conf.level))
      e <- environment()
      x <- update_summ(x, confint = TRUE, ci.width = conf.level, call.env = e)
      base$conf.low <- x$coeftable[,lci_lab]
      base$conf.high <- x$coeftable[,uci_lab]
    }
    
  }
  
  if (!is.null(attr(x, "coef_export"))) {
    
    the_coefs <- attr(x, "coef_export")
    base <- base[base$term %in% the_coefs,]
    if (!is.null(names(the_coefs))) {
      for (i in seq_along(the_coefs)) {
        if (names(the_coefs[i]) != "") {
          base$term[base$term == the_coefs[i]] <- names(the_coefs[i])
        }
      }
    }
    
  }
  
  num_cols <- sapply(base, is.numeric)
  zeroes <- check_if_zero(base[num_cols])
  if (any(zeroes == TRUE)) {
    basenums <- base[num_cols]
    basenums[zeroes] <- 0
    base[num_cols] <- basenums
  }
  
  return(base)
  
}

#' @title Broom extensions for summ objects
#' @description These are functions used for compatibility with broom's tidying
#' functions to facilitate use with huxreg, thereby making
#' \code{\link{export_summs}} works.
#' @param x The \code{summ} object.
#' @param conf.int Include confidence intervals? Default is FALSE.
#' @param conf.level How wide confidence intervals should be, if requested.
#'   Default is .95.
#' @param ... Other arguments (usually ignored)
#' @return A data.frame with columns matching those appropriate for the model
#' type per \code{glance} documentation.
#'
#' @seealso
#'  \code{\link[generics]{glance}}
#'
#' @rdname glance.summ
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'  S3method(generics::glance, summ.lm)
#' } else {
#'  export(glance.summ.lm)
#' }
#' 
#' if (getRversion() >= "3.6.0") {
#'  S3method(generics::glance, summ.glm)
#' } else {
#'  export(glance.summ.glm)
#' }
#' 
#' if (getRversion() >= "3.6.0") {
#'  S3method(generics::glance, summ.svyglm)
#' } else {
#'  export(glance.summ.svyglm)
#' }
#' 
#' if (getRversion() >= "3.6.0") {
#'  S3method(generics::glance, summ.merMod)
#' } else {
#'  export(glance.summ.merMod)
#' }


glance.summ.lm <- function(x, ...) {

  base <- generics::glance(x$model)
  return(base)

}

#' @rdname glance.summ
#' @export

glance.summ.glm <- function(x, ...) {

  base <- generics::glance(x$model)
  base["pseudo.r.squared"] <- attr(x, "rsq")
  base["pseudo.r.squared.mcfadden"] <- attr(x, "rsqmc")
  return(base)

}

#' @importFrom stats deviance df.residual
#' @rdname glance.summ
#' @export

glance.summ.svyglm <- function(x, ...) {

  ## TODO: Provide more fit statistics here

  base <- matrix(rep(NA, times = 8), ncol = 8)
  colnames(base) <- c("null.deviance", "df.null", "logLik",
                      "AIC", "BIC", "deviance", "df.residual",
                      "r.squared")
  base <- as.data.frame(base)

  try({
    base[["AIC"]] <- AIC(x$model)[2]
    base[["logLik"]] <- suppressWarnings(logLik(x$model))
  }, silent = TRUE)
  base[["deviance"]] <- deviance(x$model)
  base[["df.residual"]] <- df.residual(x$model)
  base[["null.deviance"]] <- x$model$null.deviance
  base[["df.null"]] <- x$model$df.null

  if (attr(x, "linear") == TRUE) {

    base$r.squared <- attr(x, "rsq")

  } else {

    base$r.squared <- NA
    base["pseudo.r.squared"] <- attr(x, "rsq")
    base["pseudo.r.squared.mcfadden"] <- attr(x, "rsqmc")

  }

  return(base)

}

#' @rdname glance.summ
#' @export

glance.summ.merMod <- function(x, ...) {

  base <- generics::glance(x$model)
  if (lme4::isLMM(x$model)) {base$p.value <- NA}
  # Get attributes
  atts <- attributes(x)
  # Change NULLs to missing
  if (is.na(atts$rsqs[1])) {
    atts$rsqs <- list(Marginal = NA, Conditional = NA)
  }
  # Add r.squared columns
  base$r.squared <- atts$rsqs$Conditional
  base$r.squared.fixed <- atts$rsqs$Marginal
  groups <- lme4::ngrps(x$model)
  for (i in seq_along(groups)) {
    base[paste0("group.nobs.", names(groups)[i])] <- groups[i]
  }
  return(base)

}

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @export

nobs.summ <- function(object, ...) {

  return(nobs(object$model))

}
