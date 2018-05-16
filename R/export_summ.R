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
#'     \item summ.glm = `c(N = "nobs", AIC = "AIC", BIC = "BIC")`,
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

    stop("Install the huxtable package to use the export_summs function.",
         call. = FALSE)

  }

  if (!requireNamespace("broom", quietly = TRUE)) {

    stop("Install the broom package to use the export_summs function.",
         call. = FALSE)

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
  robust <- ifelse(all(robust) != FALSE, yes = TRUE, no = FALSE)
  if ("scale" %in% names(dots)) {scale <- dots$scale}
  scale <- ifelse(all(robust) == TRUE, yes = TRUE, no = FALSE)
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
             summ.glm = c(N = "nobs", AIC = "AIC", BIC = "BIC"),
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

  } else if (statistics == "all") {

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
    if (is.null(file.name)) {
      msg_wrap("You did not provide a file name, so it will be called
               untitled.", to.file)
      file.name <- paste0("untitled.", to.file)
    }
    # Weird header is included in Word files
    if (to.file == "docx") {out$header <- NULL}
    # Create the function name based on file type
    fun_name <- paste0("quick_", to.file)
    # Call the function
    do.call(fun_name, list(out, file = file.name),
            envir = getNamespace("huxtable"))

  }

  return(out)

}

#' @title Plot Regression Summaries
#' @description `plot_summs` and `plot_coefs` create regression coefficient
#'   plots with ggplot2.
#' @param ... regression model(s).
#' @param ci_level The desired width of confidence intervals for the
#'   coefficients. Default: 0.95
#' @param inner_ci_level Plot a thicker line representing some narrower span
#'   than `ci_level`. Default is NULL, but good options are .9, .8, or .5.
#' @param model.names If plotting multiple models simultaneously, you can
#'   provide a vector of names here. If NULL, they will be named sequentially
#'   as "Model 1", "Model 2", and so on. Default: NULL
#' @param coefs If you'd like to include only certain coefficients, provide
#'   them as a vector. If it is a named vector, then the names will be used
#'   in place of the variable names. See details for examples. Default: NULL
#' @param omit.coefs If you'd like to specify some coefficients to not include
#'   in the plot, provide them as a vector. This argument is overridden by
#'   `coefs` if both are provided. By default, the intercept term is omitted.
#'   To include the intercept term, just set omit.coefs to NULL.
#' @param color.class See [jtools_colors] for more on your color options.
#'   Default: 'CUD Bright'
#' @param plot.distributions Instead of just plotting the ranges, you may
#'   plot normal distributions representing the width of each estimate. Note
#'   that these are completely theoretical and not based on a bootstrapping
#'   or MCMC procedure, even if the source model was fit that way. Default is
#'   FALSE.
#' @param rescale.distributions If `plot.distributions` is TRUE, the default
#'   behavior is to plot each normal density curve on the same scale. If some
#'   of the uncertainty intervals are much wider/narrower than others, that
#'   means the wide ones will have such a low height that you won't be able
#'   to see the curve. If you set this parameter to TRUE, each curve will
#'   have the same maximum height regardless of their width.
#' @param exp If TRUE, all coefficients are exponentiated (e.g., transforms
#'   logit coefficents from log odds scale to odds). The reference line is
#'   also moved to 1 instead of 0.
#' @param point.shape When using multiple models, should each model's point
#'   estimates use a different point shape to visually differentiate each
#'   model from the others? Default is TRUE.
#' @param legend.title What should the title for the legend be? Default is
#'   "Model", but you can specify it here since it is rather difficult to
#'   change later via `ggplot2`'s typical methods.
#' @return A ggplot object.
#' @details A note on the distinction between `plot_summs` and `plot_coefs`:
#'   `plot_summs` only accepts models supported by [summ()] and allows users
#'   to take advantage of the standardization and robust standard error features
#'   (among others as may be relevant). `plot_coefs` supports any models that
#'   have a [broom::tidy()] method defined in the broom package, but of course
#'   lacks any additional features like robust standard errors. To get a mix
#'   of the two, you can pass `summ` objects to `plot_coefs` too.
#'
#'   For \code{coefs}, if you provide a named vector of coefficients, then
#'   the plot will refer to the selected coefficients by the names of the
#'   vector rather than the coefficient names. For instance, if I want to
#'   include only the coefficients for the \code{hp} and \code{mpg} but have
#'   the plot refer to them as "Horsepower" and "Miles/gallon", I'd provide
#'   the argument like this:
#'   \code{c("Horsepower" = "hp", "Miles/gallon" = "mpg")}
#'
#' @examples
#' states <- as.data.frame(state.x77)
#' fit1 <- lm(Income ~ Frost + Illiteracy + Murder +
#'            Population + Area + `Life Exp` + `HS Grad`,
#'            data = states, weights = runif(50, 0.1, 3))
#' fit2 <- lm(Income ~ Frost + Illiteracy + Murder +
#'            Population + Area + `Life Exp` + `HS Grad`,
#'            data = states, weights = runif(50, 0.1, 3))
#' fit3 <- lm(Income ~ Frost + Illiteracy + Murder +
#'            Population + Area + `Life Exp` + `HS Grad`,
#'            data = states, weights = runif(50, 0.1, 3))
#'
#' # Plot all 3 regressions with custom predictor labels,
#' # standardized coefficients, and robust standard errors
#' plot_summs(fit1, fit2, fit3,
#'            coefs = c("Frost Days" = "Frost", "% Illiterate" = "Illiteracy",
#'                      "Murder Rate" = "Murder"),
#'            scale = TRUE, robust = TRUE)
#'
#' @rdname plot_summs
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_vline scale_colour_brewer theme
#' @importFrom ggplot2 element_blank element_text ylab

plot_summs <- function(..., ci_level = .95, model.names = NULL, coefs = NULL,
                       omit.coefs = "(Intercept)", inner_ci_level = NULL,
                       color.class = "CUD Bright", plot.distributions = FALSE,
                       rescale.distributions = FALSE, exp = FALSE,
                       point.shape = TRUE, legend.title = "Model") {

  # Capture arguments
  dots <- list(...)

  # assume unnamed arguments are models and everything else is args
  if (!is.null(names(dots))) {
    mods <- dots[names(dots) == ""]
    ex_args <- dots[names(dots) != ""]
  } else {
    mods <- dots
    ex_args <- NULL
  }

  # Add mandatory arguments
  ex_args$confint <- TRUE
  ex_args$ci.width <- ci_level

  the_summs <- do.call("summs", as.list(c(list(models = mods), ex_args)))

  args <- as.list(c(the_summs, ci_level = ci_level,
            model.names = list(model.names), coefs = list(coefs),
            omit.coefs = list(omit.coefs),
            inner_ci_level = inner_ci_level, color.class = list(color.class),
            plot.distributions = plot.distributions,
            rescale.distributions = rescale.distributions, exp = exp,
            point.shape = point.shape, legend.title = legend.title, ex_args))

  do.call("plot_coefs", args = args)

}

#' @export
#' @rdname plot_summs
#' @importFrom ggplot2 ggplot aes_string geom_vline scale_colour_brewer theme
#' @importFrom ggplot2 element_blank element_text ylab

plot_coefs <- function(..., ci_level = .95, inner_ci_level = NULL,
                       model.names = NULL, coefs = NULL,
                       omit.coefs = c("(Intercept)", "Intercept"),
                       color.class = "CUD Bright", plot.distributions = FALSE,
                       rescale.distributions = FALSE,
                       exp = FALSE, point.shape = TRUE,
                       legend.title = "Model") {

  if (!requireNamespace("broom", quietly = TRUE)) {
    stop_wrap("Install the broom package to use the plot_coefs function.")
  }
  if (!requireNamespace("ggstance", quietly = TRUE)) {
    stop_wrap("Install the ggstance package to use the plot_coefs function.")
  }

  # Nasty workaround for R CMD Check warnings for global variable bindings
  model <- term <- estimate <- conf.low <- conf.high <- conf.low.inner <-
  conf.high.inner <- curve <- est <- NULL

  # Capture arguments
  dots <- list(...)

  if (!is.null(names(dots))) {
    mods <- dots[names(dots) == ""]
    ex_args <- dots[names(dots) != ""]
  } else {
    mods <- dots
    ex_args <- NULL
  }

  if (!is.null(omit.coefs) & !is.null(coefs)) {
    if (any(omit.coefs %nin% c("(Intercept)", "Intercept"))) {
      msg_wrap("coefs argument overrides omit.coefs argument. Displaying
               coefficients listed in coefs argument.")
    }
    omit.coefs <- NULL
  }

  # If user gave model names, apply them now
  if (!is.null(model.names)) {
    names(mods) <- model.names
  }

  # Create tidy data frame combining each model's tidy output
  tidies <- make_tidies(mods = mods, ex_args = ex_args, ci_level = ci_level,
                        model.names = model.names, omit.coefs = omit.coefs,
                        coefs = coefs)

  n_models <- length(unique(tidies$model))

  # Create more data for the inner interval
  if (!is.null(inner_ci_level)) {
    if (plot.distributions == FALSE | n_models == 1) {
      tidies_inner <- make_tidies(mods = mods, ex_args = ex_args,
                                  ci_level = inner_ci_level,
                                  model.names = model.names,
                                  omit.coefs = omit.coefs, coefs = coefs)

      tidies_inner$conf.low.inner <- tidies_inner$conf.low
      tidies_inner$conf.high.inner <- tidies_inner$conf.high
      tidies_inner <-
        tidies_inner[names(tidies_inner) %nin% c("conf.low", "conf.high")]

      tidies <- merge(tidies, tidies_inner, by = c("term", "model"),
                      suffixes = c("", ".y"))
    } else {
      msg_wrap("inner_ci_level is ignored when plot.distributions == TRUE and
                more than one model is used.")
      inner_ci_level <- NULL
    }
  }

  if (exp == TRUE) {
    if (plot.distributions == TRUE) {
      warn_wrap("Distributions cannot be plotted when exp == TRUE.")
      plot.distributions <- FALSE
    }
    exp_cols <- c("estimate", "conf.low", "conf.high")
    if (!is.null(inner_ci_level)) {
      exp_cols <- c(exp_cols, "conf.low.inner", "conf.high.inner")
    }
    tidies[exp_cols] <- exp(tidies[exp_cols])
  }

  p <- ggplot(data = tidies)

  # Checking if user provided the colors his/herself
  if (length(color.class) == 1 | length(color.class) != n_models) {
    colors <- get_colors(color.class, n_models)
  } else {
    colors <- color.class
  }

  # "dodge height" determined by presence of distribution plots
  dh <- as.numeric(!plot.distributions) * 0.5
  # Plot distribution layer first so it is beneath the pointrange
  if (plot.distributions == TRUE) {
    # Helper function to generate data for geom_polygon
    dist_curves <- get_dist_curves(tidies, order = rev(levels(tidies$term)),
                    models = levels(tidies$model),
                    rescale.distributions = rescale.distributions)
    # Draw the distributions
    p <- p + geom_polygon(data = dist_curves,
                          aes(x = est, y = curve,
                              group = interaction(term, model), fill = model),
                          alpha = 0.7, show.legend = FALSE) +
    scale_fill_manual(name = legend.title,
                      values = get_colors(color.class, n_models),
                      breaks = rev(levels(tidies$model)),
                      limits = rev(levels(tidies$model)))
  }

  # Plot the inner CI using linerange first, so the point can overlap it
  if (!is.null(inner_ci_level)) {
    p <- p + ggstance::geom_linerangeh(
      aes(y = term, xmin = conf.low.inner, xmax = conf.high.inner,
          colour = model), position = ggstance::position_dodgev(height = dh),
      size = 2, show.legend = length(mods) > 1)
  }

  # If there are overlapping distributions, the overlapping pointranges
  # are confusing and look bad. If plotting distributions with more than one
  # model, just plot the points with no ranges.
  if (plot.distributions == FALSE | n_models == 1) {
    # Plot the pointranges
    p <- p + ggstance::geom_pointrangeh(
      aes(y = term, x = estimate, xmin = conf.low,
                xmax = conf.high, colour = model, shape = model),
      position = ggstance::position_dodgev(height = dh),
      fill = "white", fatten = 3, size = 0.8,
      show.legend = length(mods) > 1) # omit legend if just a single model
  } else {
    p <- p + geom_point(
      aes(y = term, x = estimate, colour = model, shape = model),
      fill = "white", size = 3, stroke = 1, show.legend = TRUE)
  }

  # To set the shape aesthetic, I prefer the points that can be filled. But
  # there are only 6 such shapes, so I need to check how many models there are.
  if (point.shape == TRUE) {
    oshapes <- c(21:25, 15:18, 3, 4, 8)
    shapes <- oshapes[seq_len(n_models)]
  } else if (point.shape == FALSE) {
    shapes <- rep(21, times = n_models)
  }

  p <- p +
    geom_vline(xintercept = 1 - !exp, linetype = 2, size = .25) +
    scale_colour_manual(values = colors,
     limits = rev(levels(tidies$model)),
     breaks = rev(levels(tidies$model)),
     labels = rev(levels(tidies$model)),
     name = legend.title) +
    scale_y_discrete(limits = rev(levels(tidies$term)),
                     name = legend.title) +
    scale_shape_manual(limits = rev(levels(tidies$model)),
      values = shapes, name = legend.title) +
    theme_apa(legend.pos = "right", legend.font.size = 9,
              remove.x.gridlines = FALSE, legend.use.title = TRUE) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10)) +
    xlab(ifelse(exp, no = "Estimate", yes = "exp(Estimate)"))

  # Plotting distributions often causes distributions to poke out above top
  # of plotting area so I need to set manually
  if (plot.distributions == TRUE) {
    yrange <- ggplot_build(p)$layout$panel_ranges[[1]]$y.range
    xrange <- ggplot_build(p)$layout$panel_ranges[[1]]$x.range

    if (yrange[2] <= (length(unique(tidies$term)) + 0.8)) {
      upper_y <- length(unique(tidies$term)) + 0.8
    } else {upper_y <- yrange[2]}
    lower_y <- 0.8

    p <- p + coord_cartesian(ylim = c(lower_y, upper_y), xlim = xrange,
     expand = FALSE)
  }

  return(p)

}

make_tidies <- function(mods, ex_args, ci_level, model.names, omit.coefs,
                        coefs) {
  # Create empty list to hold tidy frames
  tidies <- as.list(rep(NA, times = length(mods)))

  for (i in seq_along(mods)) {

    if (!is.null(ex_args)) {

      method_stub <- find_S3_class("tidy", mods[[i]], package = "broom")
      # getS3method() only available in R >= 3.3
      the_method <- get(paste0("tidy.", method_stub), asNamespace("broom"),
                        mode = "function")
      method_args <- formals(the_method)

      method_args <-
        method_args[names(method_args) %nin% c("intervals", "prob")]

      extra_args <- ex_args[names(ex_args) %in% names(method_args)]

    } else {
      extra_args <- NULL
    }

    all_args <- as.list(c(x = list(mods[[i]]), conf.int = TRUE,
                          conf.level = ci_level,
                          intervals = TRUE, prob = ci_level,
                          extra_args))

    tidies[[i]] <- do.call(broom::tidy, args = all_args)
    if (!is.null(names(mods)) & any(names(mods) != "")) {
      tidies[[i]]$model <- names(mods)[i]
    } else {
      modname <- paste("Model", i)
      tidies[[i]]$model <- modname
    }

  }

  # Combine the tidy frames into one, long frame
  tidies <- do.call(rbind, tidies)

  # For consistency in creating the factors apply contrived names to model.names
  if (is.null(model.names)) {

    model.names <- unique(tidies$model)

  }

  # Drop omitted coefficients
  if (!is.null(omit.coefs)) {

    tidies <- tidies[tidies$term %nin% omit.coefs,]

  }

  # Creating factors with consistent ordering for coefficients too
  if (is.null(coefs)) {

    coefs <- unique(tidies$term)
    names(coefs) <- coefs

  } else {

    tidies <- tidies[tidies$term %in% coefs,]
    if (is.null(names(coefs))) {
      names(coefs) <- coefs
    }

  }

  # For some reason, the order of the legend and the dodged colors
  # only line up when they are reversed here and in the limits arg of
  # scale_colour_brewer...no clue why that has to be the case
  tidies$model <- factor(tidies$model, levels = rev(model.names))
  tidies$term <- factor(tidies$term, levels = coefs, labels = names(coefs))

  if (all(c("upper", "lower") %in% names(tidies)) &
      "conf.high" %nin% names(tidies)) {
    tidies$conf.high <- tidies$upper
    tidies$conf.low <- tidies$lower
  }

  # For merMod and other models, we may not get CIs for the random terms
  # and don't want blank rows on the plot.
  which_complete <- which(!is.na(tidies$conf.high) & !is.na(tidies$conf.low) &
                         !is.na(tidies$estimate))
  tidies <- tidies[which_complete,]
  tidies$term <- droplevels(tidies$term)

  return(tidies)

}

#' @importFrom stats dnorm
get_dist_curves <- function(tidies, order, models, rescale.distributions) {

  term_names <- tidies$term
  means <- tidies$estimate
  sds <- tidies$`std.error`
  heights <- c()

  cfs <- as.list(rep(NA, length(means)))
  for (i in seq_along(means)) {
    # If I wanted to vertically dodge the distributions, I'd use this...but
    # for now I am omitting it. Took long enough to figure out that I don't
    # want to delete it.
    # if (length(models) > 1) {
    #   groupid <- which(models == tidies$model[i])
    #   y_pos <- y_pos + 0.25 * ((groupid - 0.5) / length(models) - .5)
    # }
    ests <- seq(tidies$conf.low[i], tidies$conf.high[i], length = 198)
    ests <- c(ests[1], ests, ests[198])
    cfs[[i]] <- data.frame(
        term = rep(tidies$term[i], 200),
        model = rep(tidies$model[i], 200),
        est = ests,
        curve = c(0, dnorm(ests[2:199], mean = means[i], sd = sds[i]), 0)
    )
    this_curve <- cfs[[i]]$curve
    height <- max(this_curve)
    heights <- c(heights, height)

  }

  if (rescale.distributions == FALSE && max(heights) / min(heights) > 50) {
    msg_wrap("If some of the distribution curves are too short to see,
             consider rescaling your model coefficients or using the
             rescale.distributions = TRUE argument.")
  }

  for (i in seq_along(means)) {

    if (rescale.distributions == FALSE) {
      multiplier <- .6 / max(heights)
    } else {
      multiplier <- .6 / heights[i]
    }

    y_pos <- which(order == term_names[i])
    this_curve <- cfs[[i]]$curve
    this_curve <- (this_curve * multiplier) + y_pos
    cfs[[i]]$curve <- this_curve

  }

  out <- do.call("rbind", cfs)
  return(out)

}

#' @export tidy.summ
#' @rdname glance.summ

tidy.summ <- function(x, conf.int = FALSE, conf.level = .95, ...) {

  if (class(x)[1] != "summ.rq") {
    base <- broom::tidy(x$model, conf.int = conf.int, conf.level = conf.level,
                        ...)
  } else {
    dots <- list(...)
    dots <- dots[names(dots) %in% c(names(formals("rq")),
                                    names(formals("rq.fit.br")),
                                    "alpha")]

    args <- as.list(c(list(x$model), conf.int = conf.int,
                      conf.level = conf.level,
                 se.type = attr(x, "se"), dots))

    base <- do.call(broom::tidy, args)
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

    base$p.value[!is.na(base$statistic)] <- x$coeftable[,"p"]

  } else if (!("p.value" %in% names(base))) {

    base$p.value <- NA

  }

  if ("statistic" %in% names(base) && any(is.na(base[["statistic"]]))) {

    base <- base[!is.na(base$statistic),]

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
#' @importFrom stats deviance df.residual
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

#' @export

nobs.summ <- function(object, ...) {

  return(nobs(object$model))

}
