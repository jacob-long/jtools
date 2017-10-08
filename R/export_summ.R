#' @title Export regression summaries to tables
#'
#' @description This function allows users to use the features of
#'   \code{\link{summ}} (e.g., standardization, robust standard errors)
#'   in the context of shareable HTML, LaTeX, and
#'   Microsoft Word tables. It relies heavily on \code{\link[huxtable]{huxreg}}
#'   to do the table formatting. This is particularly useful for putting
#'   the results of multiple models into a single table.
#'
#' @param ... At minimum, a regression object(s). See details for more arguments.
#' @param error_format Which of standard error, confidence intervals, test
#'   statistics, or p values should be used to express uncertainty of estimates
#'   for regression coefficients? See details for more info.
#'   Default: "({std.error})"
#' @param error_pos Where should the error statistic defined in
#'   \code{error_style} be placed relative to the coefficient estimate?
#'   Default: "below"
#' @param ci_level If reporting confidence intervals, what should the
#'   confidence level be? By default, it is .95 (95% interval) if
#'   confidence intervals are requested in \code{error_format}.
#' @param statistics Which model summary statistics should be included?
#'   See \code{\link[huxtable]{huxreg}} for more on usage. The default
#'   for this function depends on the model type. See details for more on
#'   the defaults by model type.
#' @param model.names If you want to give your model(s) names at the top
#'   of each column, provide them here as a character vector.
#'   Otherwise, they will just be labeled by number. Default: NULL
#' @param to.word Export the table to a Microsoft Word document?
#'   This functionality relies on the \pkg{officer}
#'   and \pkg{flextable} packages. Default: FALSE
#' @param word.file File name with (optionally) file path to save the Word
#'   file. Ignored if \code{to.word} is FALSE. Default: NULL
#'
#' @return If \code{to.word} is FALSE, a \code{\link[huxtable]{huxtable}}
#'   object. If \code{to.word} is TRUE, it just writes the table to file and
#'   returns nothing.
#'
#' @details There are many optional parameters not documented above. Any
#'   argument that you would want to pass to \code{\link{summ}}, for instance,
#'   will be used. Of particular interest may be the robust and standardize
#'   arguments. Note that some \code{summ} arguments may not have any bearing
#'   on the table output.
#'
#'   The default model summary statistics reporting follows this logic:
#'
#'   \itemize{
#'     \item summ.lm = c(N = "nobs", R2 = "r.squared"),
#'     \item summ.glm = c(N = "nobs", AIC = "AIC", BIC = "BIC"),
#'     \item summ.svyglm = c(N = "nobs", R2 = "r.squared"),
#'     \item summ.merMod = c(N = "nobs", AIC = "AIC", BIC = "BIC")
#'   }
#'
#'   If you set \code{statistics = "all"}, then the statistics argument
#'   passed to \code{huxreg} will be \code{NULL}, which reports whichever
#'   model statistics are available via \code{glance}. If you want no
#'   model summary statistics, set the argument to \code{character(0)}.
#'
#'   You have a few options for the \code{error_format} argument.
#'   You can include anything returned by \code{\link[broom]{tidy}}
#'   (see also \code{\link{tidy.summ}}). For the most part, you will
#'   be interested in \code{std.error} (standard error), \code{statistic}
#'   (test statistic, e.g. t-value or z-value), \code{p.value}, or
#'   \code{conf.high} and \code{conf.low}, which correspond to the
#'   upper and lower bounds of the confidence interval for the estimate.
#'   Note that the default \code{ci_level} argument is .95, but you
#'   can alter that as desired.
#'
#'   To format the error statistics, simply put the statistics desired in
#'   curly braces wherever you want them in a character string. For example,
#'   if you want the standard error in parentheses, the argument would be
#'   \code{"({std.error})"}, which is the default. Some other ideas:
#'
#'   \itemize{
#'
#'     \item \code{"({statistic})"}, which gives you the test statistic in
#'     parentheses.
#'     \item \code{"({statistic}, p = {p.value})"}, which gives the test
#'     statistic followed by a "p =" p value all in parentheses. Note that
#'     you'll have to pay special attention to rounding if you do this to keep
#'     cells sufficiently narrow.
#'     \item \code{"[{conf.low}, {conf.high}]"}, which gives the confidence
#'     interval in the standard bracket notation. You could also explicitly
#'     write the confidence level, e.g.,
#'      \code{"95\% CI [{conf.low}, {conf.high}]"}.
#'
#'   }
#'
#'   You can also pass any argument accepted by the
#'   \code{\link[huxtable]{huxreg}} function. A few that are likely to be
#'   oft-used are documented above, but visit \code{huxreg}'s documentation
#'   for more info.
#'
#'   For info on converting the \code{\link[huxtable]{huxtable}} object to
#'   HTML or LaTeX, see \code{huxtable}'s documentation.
#'
#' @examples
#'
#' fit1 <- lm(Income ~ Frost, data = as.data.frame(state.x77))
#' fit2 <- lm(Income ~ Frost + Illiteracy, data = as.data.frame(state.x77))
#' fit3 <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))
#'
#' # Export all 3 regressions with "Model #" labels,
#' # standardized coefficients, and robust standard errors
#' export_summs(fit1, fit2, fit3, model.names = c("Model 1","Model 2","Model 3"),
#'              standardize = TRUE, robust = TRUE)
#'
#' @seealso
#'
#'  \code{\link{summ}}
#'
#'  \code{\link[huxtable]{huxreg}}
#'
#'  \code{\link[ReporteRs]{writeDoc}}
#'
#' @export
#' @importFrom utils getFromNamespace
#'
export_summs <- function(...,
                         error_format = "({std.error})",
                         error_pos = c("below", "right", "same"),
                         ci_level = .95, statistics = NULL,
                         model.names = NULL, to.word = FALSE,
                         word.file = NULL) {

  if (!requireNamespace("huxtable", quietly = TRUE)) {

    stop("Install the huxtable package to use the export_summs function.")

  }

  if (!requireNamespace("broom", quietly = TRUE)) {

    stop("Install the broom package to use the export_summs function.")

  } else {

    library(broom)

  }

  # Capture arguments
  dots <- list(...)

  # If first element of list is a list, assume the list is a list of models
  if (inherits(dots[[1]], 'list')) {

    mods <- dots[[1]]

  } else {
  # Otherwise assume unnamed arguments are models and everything else is args

    if (!is.null(names(dots))) {

      mods <- dots[names(dots) == ""]

    } else {

      mods <- dots

    }

  }

  # Assuming all models are the same type as first
  mod_type <- class(j_summ(mods[[1]]))[1]

  # Now get the argument names for that version of summ
  summ_formals <- formals(getFromNamespace(mod_type, "jtools"))

  # Setting defaults for summ functions I want to add captions about
  robust <- FALSE
  standardize <- FALSE
  n.sd <- 1
  digits <- getOption("jtools-digits", 3)

  # Check for need to fit confidence intervals
  if (!grepl("conf.low", error_format, fixed = TRUE) &
    !grepl("conf.high", error_format, fixed = TRUE)) {

    ci_level <- NULL

  } else {

    dots$confint <- TRUE
    dots$ci.width <- ci_level

  }

  # Since I'm looping, I'm creating the list before the loop
  jsumms <- as.list(rep(NA, length(mods)))

  if (!is.null(names(dots))) {

    summ_args <- dots[names(dots) %in% names(summ_formals)]

    # For those critical arguments that require a note, see if they were
    # provided by the user and overwrite if so
    if ("robust" %in% names(summ_args)) {robust <- summ_args$robust}
    if ("standardize" %in% names(summ_args)) {standardize <- summ_args$standardize}
    if ("n.sd" %in% names(summ_args)) {n.sd <- summ_args$n.sd}
    if ("digits" %in% names(summ_args)) {digits <- summ_args$digits}

    for (i in 1:length(mods)) {

      the_args <- summ_args
      the_args$model <- mods[[i]]

      jsumms[[i]] <- do.call(what = summ, args = the_args)

    }

  } else {

    jsumms <- lapply(mods, FUN = summ)

  }

  # If user gave model names, apply them now
  if (!is.null(model.names)) {

    names(jsumms) <- model.names

  }

  ## Setting statistics option
  if (is.null(statistics)) {

    statistics <- switch(mod_type,
           summ.lm = c(N = "nobs", R2 = "r.squared"),
           summ.glm = c(N = "nobs", AIC = "AIC", BIC = "BIC"),
           summ.svyglm = c(N = "nobs", R2 = "r.squared"),
           summ.merMod = c(N = "nobs", AIC = "AIC", BIC = "BIC")
    )

  } else if (statistics == "all") {

    statistics <- NULL

  }

  if (!is.null(names(dots))) {

    hux_formals <- formals(huxtable::huxreg)

    hux_args <- dots[names(dots) %in% names(hux_formals)]

    hux_args <- as.list(c(jsumms, unlist(hux_args),
                          error_pos = error_pos[1],
                          error_format = error_format,
                          statistics = list(statistics),
                          ci_level = ci_level))

    if (!("note" %in% names(hux_args))) {

      if (standardize == TRUE) {

        note <- "All continuous predictors are mean-centered and scaled by 1 standard deviation."

        if (robust == TRUE) {

          note <- paste(note, "Standard errors are heteroskedasticity robust. %stars%.")

        } else {

          note <- paste(note, "{stars}.")

        }

        hux_args$note <- note

      } else if (robust == TRUE & standardize == FALSE) {

        note <- paste("Standard errors are heteroskedasticity robust. %stars%.")
        hux_args$note <- note

      }

    }

    if (!("number_format" %in% names(hux_args))) {

      hux_args$number_format <- paste("%.", digits, "f", sep = "")

    }

    out <- do.call(what = huxtable::huxreg, args = hux_args)


  } else {

    out <- huxtable::huxreg(jsumms, error_pos = error_pos[1],
                            error_format = error_format,
                            statistics = statistics,
                            ci_level = ci_level)

  }

  if (to.word == TRUE) {

    if (!requireNamespace("officer", quietly = TRUE)) {

      stop("Install the officer package to use the to.word functionality.")

    }

    if (!requireNamespace("flextable", quietly = TRUE)) {

      stop("Install the flextable package to use the to.word functionality.")

    }

    if (is.null(word.file)) {

      message("You did not provide a file name, so it will be called untitled.docx.")
      word.file <- "untitled.docx"

    }

    wout <- huxtable::as_flextable(out)
    wout$header <- NULL

    doc <- officer::read_docx()

    doc <- flextable::body_add_flextable(doc, wout)

    print(doc, target = word.file)

  }

  return(out)

}

#' @export tidy.summ
#' @rdname glance.summ

tidy.summ <- function(x, conf.int = FALSE, conf.level = .95, ...) {

  base <- broom::tidy(x$model, conf.int = conf.int, conf.level = conf.level, ...)

  if (conf.int == FALSE) {
    # If conf.int == TRUE, summ does not have a S.E. column
    base$std.error[!is.na(base$std.error)] <- x$coeftable[,"S.E."]

  }

  # Need to find the statistic column without knowing what it will be called
  stat_col <- attributes(x)$test.stat

  # Filtering to not NA avoids issues with NAs being filled with
  # repetitive values due to replacement being multiple of its length
  base$statistic[!is.na(base$statistic)] <- x$coeftable[,stat_col]

  if ("p" %in% colnames(x$coeftable)) {

    base$p.value[!is.na(base$statistic)] <- x$coeftable[,"p"]

  } else if (!("p.value" %in% names(base))) {

    base$p.value <- NA

  }

  if (any(is.na(base[["statistic"]]))) {

    base <- base[!is.na(base$statistic),]

  }

  if (conf.int == TRUE) {

    # Need to use this to get the right coeftable colnames
    alpha <- (1 - conf.level)/2

    lci_lab <- 0 + alpha
    lci_lab <- paste(round(lci_lab * 100,1), "%", sep = "")

    uci_lab <- 1 - alpha
    uci_lab <- paste(round(uci_lab * 100,1), "%", sep = "")

    base$conf.low[!is.na(base$conf.low)] <- x$coeftable[,lci_lab]
    base$conf.high[!is.na(base$conf.high)] <- x$coeftable[,uci_lab]

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
#'  \code{\link[broom]{glance}}
#'
#' @rdname glance.summ
#' @export glance.summ.lm

glance.summ.lm <- function(x, ...) {

  base <- broom::glance(x$model)
  return(base)

}

#' @export glance.summ.glm
#' @rdname glance.summ

glance.summ.glm <- function(x, ...) {

  base <- broom::glance(x$model)
  return(base)

}

#' @export glance.summ.svyglm
#' @importFrom stats deviance
#' @rdname glance.summ

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

  }

  return(base)

}

#' @export glance.summ.merMod
#' @rdname glance.summ

glance.summ.merMod <- function(x, ...) {

  base <- broom::glance(x$model)
  if (lme4::isLMM(x$model)) {base$p.value <- NA}
  return(base)

}

#' @export

nobs.summ <- function(object, ...) {

  return(nobs(object$model))

}

