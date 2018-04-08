#' Perform a simple slopes analysis.
#'
#' \code{sim_slopes} conducts a simple slopes analysis for the purposes of
#' understanding two- and three-way interaction effects in linear regression.
#'
#' @param pred The predictor variable involved in the interaction.
#'
#' @param modx The moderator variable involved in the interaction.
#'
#' @param mod2 Optional. The name of the second moderator
#'  variable involved in the interaction.
#'
#' @param modxvals For which values of the moderator should simple slopes
#'   analysis be performed? Default is \code{NULL}. If \code{NULL}, then the
#'   values will be the customary +/- 1 standard deviation from the mean as
#'   well as the mean itself. There is no specific limit on the number of
#'   variables provided. If
#'   \code{"plus-minus"}, uses just +/- 1 standard
#'   deviation without the mean. You may also choose `"terciles"` to split
#'   the data into equally-sized groups and choose the point at the mean of
#'   each of those groups.
#'
#'   Factor variables
#'   are not particularly suited to simple slopes analysis, but you could have
#'   a numeric moderator with values of 0 and 1 and give \code{c(0,1)} to
#'   compare the slopes at the different conditions. Two-level factor
#'   variables are coerced to numeric 0/1 variables, but are not
#'   standardized/centered like they could be if your input data had a numeric
#'   0/1 variable.
#'
#' @param mod2vals For which values of the second moderator should the plot be
#'   facetted by? That is, there will be a separate plot for each level of this
#'   moderator. Defaults are the same as \code{modxvals}.
#'
#' @param centered A vector of quoted variable names that are to be
#'   mean-centered. If `"all"`, all non-focal predictors as well as
#'   the `pred` variable are centered. You
#'   may instead pass a character vector of variables to center. User can
#'   also use "none" to base all predictions on variables set at 0.
#'   The response variable, `modx`, and `mod2` variables are never
#'   centered.
#'
#' @param cond.int Should conditional intercepts be printed in addition to the
#'   slopes? Default is \code{FALSE}.
#'
#' @param johnson_neyman Should the Johnson-Neyman interval be calculated?
#'   Default is \code{TRUE}. This can be performed separately with
#'   \code{\link{johnson_neyman}}.
#'
#' @param jnplot Should the Johnson-Neyman interval be plotted as well? Default
#'   is \code{FALSE}.
#'
#' @param jnalpha What should the alpha level be for the Johnson-Neyman
#'   interval? Default is .05, which corresponds to a 95\% confidence interval.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 2. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired
#'   number.
#'
#' @param ... Arguments passed to \code{\link{johnson_neyman}}.
#'
#' @details This allows the user to perform a simple slopes analysis for the
#'   purpose of probing interaction effects in a linear regression. Two- and
#'   three-way interactions are supported, though one should be warned that
#'   three-way interactions are not easy to interpret in this way.
#'
#'   For more about Johnson-Neyman intervals, see \code{\link{johnson_neyman}}.
#'
#'   The function is tested with `lm`, `glm`, `svyglm`, and `merMod` inputs.
#'   Others may work as well, but are not tested. In all but the linear model
#'   case, be aware that not all the assumptions applied to simple slopes
#'   analysis apply.
#'
#' @return
#'
#'  \item{slopes}{A table of coefficients for the focal predictor at each
#'  value of the moderator}
#'  \item{ints}{A table of coefficients for the intercept at each value of the
#'    moderator}
#'  \item{modxvals}{The values of the moderator used in the analysis}
#'  \item{mods}{A list containing each regression model created to estimate
#'     the conditional coefficients.}
#'  \item{jn}{If \code{johnson_neyman = TRUE}, a list of `johnson_neyman`
#'  objects from \code{\link{johnson_neyman}}. These contain the values of the
#'  interval and the plots. If a 2-way interaction, the list will be of length
#'  1. Otherwise, there will be 1 `johnson_neyman` object for each value of the
#'  2nd moderator for 3-way interactions.}
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @inheritParams interact_plot
#' @inheritParams summ.lm
#'
#' @family interaction tools
#'
#' @seealso \code{\link{interact_plot}} accepts similar syntax and will plot the
#'   results with \code{\link[ggplot2]{ggplot}}.
#'
#'   \code{\link[rockchalk]{testSlopes}} performs a hypothesis test of
#'       differences and provides Johnson-Neyman intervals.
#'
#'   \code{\link[pequod]{simpleSlope}} performs a similar analysis.
#'
#' @references
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#'  multilevel regression: Inferential and graphical techniques.
#'  \emph{Multivariate Behavioral Research}, \emph{40}(3), 373-400.
#'  \url{http://dx.doi.org/10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple regression/correlation analyses for the behavioral sciences} (3rd
#' ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' @examples
#'
#' # Using a fitted model as formula input
#' fiti <- lm(Income ~ Frost + Murder * Illiteracy,
#'   data = as.data.frame(state.x77))
#' sim_slopes(model = fiti, pred = Murder, modx = Illiteracy)
#'
#' # With svyglm
#' if (requireNamespace("survey")) {
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals)
#'
#' # 3-way with survey and factor input
#' regmodel <- svyglm(api00 ~ ell * meals * sch.wide, design = dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals, mod2 = sch.wide)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd update getCall vcov relevel
#' @export
#'

sim_slopes <- function(model, pred, modx, mod2 = NULL, modxvals = NULL,
                       mod2vals = NULL, centered = "all", data = NULL,
                       cond.int = FALSE, johnson_neyman = TRUE, jnplot = FALSE,
                       jnalpha = .05, robust = FALSE,
                       digits = getOption("jtools-digits", default = 2),
                       confint = FALSE, ci.width = .95, cluster = NULL, ...) {

  # Allows unquoted variable names
  pred <- as.character(substitute(pred))
  modx <- as.character(substitute(modx))
  mod2 <- as.character(substitute(mod2))
  # To avoid unexpected behavior, need to un-un-parse mod2 when it is NULL
  if (length(mod2) == 0) {
    mod2 <- NULL
    mod2vals2 <- NULL
  }

  # Capture extra arguments
  dots <- list(...)
  if (length(dots) > 0) { # See if there were any extra args
    # Check for deprecated arguments
    ss_dep_check("sim_slopes", dots)

    # Get j_n args from dots
    if (johnson_neyman == TRUE) {
      jn_arg_names <- names(formals("johnson_neyman"))
      if (any(names(dots) %in% jn_arg_names)) {
        jn_args <- list()
        for (n in names(dots)[which(names(dots) %in% jn_arg_names)]) {
          jn_args[[n]] <- dots[[n]]
        }
      }
    }

    if ("robust.type" %in% names(dots)) {
      warn_wrap("The robust.type argument is deprecated. Please specify the
       type as the value for the 'robust' argument instead.", call. = FALSE)
      robust <- dots$robust.type
    }
  }

  # Create object to return
  ss <- list()

  ss <- structure(ss, digits = digits)

  if (!is.null(modxvals) && !is.vector(modxvals)) {
    stop("The modxvals argument must be a vector of at least length 2 if it is",
         " used.")
  }

  # Is it a svyglm?
  if (class(model)[1] == "svyglm" || class(model)[1] == "svrepglm") {

    survey <- TRUE
    design <- model$survey.design
    d <- design$variables

    # Focal vars so the weights don't get centered
    fvars <- as.character(attributes(terms(model))$variables)
    # for some reason I can't get rid of the "list" as first element
    fvars <- fvars[2:length(fvars)]
    all.vars <- fvars

    facvars <- c()
    for (v in fvars) {
      if (is.factor((d[,v])) && length(unique(d[,v])) > 2) {
        facvars <- c(facvars, v)
      }
    }

  } else {

    survey <- FALSE
    # Duplicating the dataframe so it can be manipulated as needed
    if (is.null(data)) {
      d <- model.frame(model)
      # Check to see if model.frame names match formula names
      varnames <- names(d)
      # Drop weights and offsets
      varnames <- varnames[varnames %nin% c("(offset)","(weights)")]
      if (any(varnames %nin% all.vars(formula(model)))) {

        warn_wrap("Variable transformations in the model formula
                  detected. Trying to use ", as.character(getCall(model)$data),
                  " from global environment instead. This could cause incorrect
                  results if ", as.character(getCall(model)$data), " has been
                  altered since the model was fit. You can manually provide the
                  data to the \"data =\" argument.", call. = FALSE)
      }
    } else {
      d <- data
    }
    design <- NULL

    fvars <- as.character(attributes(terms(model))$variables)
    # for some reason I can't get rid of the "list" as first element
    fvars <- fvars[2:length(fvars)]
    all.vars <- fvars

    facvars <- c()
    for (v in fvars) {
      if (is.factor((d[,v])) && length(unique(d[,v])) > 2) {
        facvars <- c(facvars, v)
      }
    }

  }

  # Check for factor predictor
  if (is.factor(d[[pred]])) {
    # I could assume the factor is properly ordered, but that's too risky
    stop(wrap_str("Focal predictor (\"pred\") cannot be a factor. Either
          use it as modx, convert it to a numeric dummy variable
          or use the cat_plot function for factor by factor interaction
          plots."))
  }

  # weights?
  if (survey == FALSE && ("(weights)" %in% names(d) |
                          !is.null(getCall(model)$weights))) {
    weights <- TRUE
    wname <- as.character(getCall(model)["weights"])
    if (any(colnames(d) == "(weights)")) {
      colnames(d)[which(colnames(d) == "(weights)")] <- wname
    }
    wts <- d[[wname]]

  } else {

    weights <- FALSE
    wname <- NULL
    wts <- rep(1, times = nrow(d))

  }

  # offset?
  if (!is.null(model.offset(model.frame(model)))) {

    off <- TRUE
    offname <- as.character(getCall(model)$offset[-1]) # subset gives bare name

    # Getting/setting offset name depending on whether it was specified in
    # argument or formula
    if (any(colnames(d) == "(offset)") & !is.null(offname)) {
      colnames(d)[which(colnames(d) == "(offset)")] <- offname
    } else if (any(colnames(d) == "(offset)") & is.null(offname)) {

      offname <- "(offset)"

      # This strategy won't work for svyglm
      if (survey == TRUE) {

        stop("For svyglm with offsets, please specify the offset with the
             'offset =' argument rather than in the model formula.")

      }

    }

    # See if offset term was logged
    if (offname == "(offset)") {
      offterm <- regmatches(as.character(formula(model)),
                            regexpr("(?<=(offset\\()).*(?=(\\)))",
                                    as.character(formula(model)), perl = TRUE))
      if (grepl("log(", offterm, fixed = TRUE)) {

        d[[offname]] <- exp(d[[offname]])

      }

    }

    # Exponentiate offset if it was logged
    if ("log" %in% as.character(getCall(model)$offset)) {
      d[[offname]] <- exp(d[[offname]])
    }

  } else {

      off <- FALSE
      offname <- NULL

  }

  # Pulling the name of the response variable for labeling
  formula <- formula(model)
  formula <- paste(formula[2],formula[1],formula[3])

  resp <- sub("(.*)(?=~).*", x = formula, perl = T, replacement = "\\1")
  resp <- trimws(resp)

  # Saving key arguments as attributes of return object
  ss <- structure(ss, resp = resp, modx = modx, mod2 = mod2, pred = pred,
                  cond.int = cond.int)

### Centering #################################################################

  # Update facvars by pulling out all non-focals
  facvars <-
    facvars[!(facvars %in% c(pred, resp, modx, mod2, wname, offname))]

  # Use utility function shared by all interaction functions
  c_out <- center_ss(d = d, weights = wts, facvars = facvars,
              fvars = fvars, pred = pred,
              resp = resp, modx = modx, survey = survey,
              design = design, mod2 = mod2, wname = wname,
              offname = offname, centered = centered)

  design <- c_out$design
  d <- c_out$d
  fvars <- c_out$fvars
  facvars <- c_out$facvars

### Getting moderator values ##################################################

  modxvals2 <- mod_vals(d, modx, modxvals, survey, wts, design,
                        modx.labels = NULL, any.mod2 = !is.null(mod2),
                        sims = TRUE)

  if (is.factor(d[[modx]]) & johnson_neyman == TRUE) {
        warn_wrap("Johnson-Neyman intervals are not available for factor
                   moderators.", call. = FALSE)
        johnson_neyman <- FALSE
  }

  # Now specify def or not (for labeling w/ print method)
  if (is.character(modxvals) | is.null(modxvals)) {

    ss <- structure(ss, def = TRUE)

  } else {

    ss <- structure(ss, def = FALSE)

  }

  # Don't want def = TRUE for factors even though they are character
  if (is.factor(d[[modx]])) {ss <- structure(ss, def = FALSE)}

  if (!is.null(mod2)) {

    if (!is.factor(d[[mod2]])) {
      mod2vals2 <- mod_vals(d, mod2, mod2vals, survey, wts, design,
                            modx.labels = NULL, any.mod2 = !is.null(mod2),
                            sims = TRUE)
    } else {

      if (is.null(mod2vals)) {
        mod2vals2 <- levels(d[[mod2]])
      } else {
        if (all(mod2vals %in% levels(d[[mod2]]))) {
          mod2vals2 <- mod2vals
        } else {
          warn_wrap("mod2vals argument must include only levels of the
                    factor. Using all factor levels instead.", call. = FALSE)
          mod2vals2 <- levels(d[[mod2]])
        }
      }

    }

    # Now specify def or not
    if (is.character(mod2vals) | is.null(mod2vals)) {

      ss <- structure(ss, def2 = TRUE)

    } else {

      ss <- structure(ss, def2 = FALSE)

    }

    # Don't want def = TRUE for factors even though they are character
    if (is.factor(d[[mod2]])) {ss <- structure(ss, def2 = FALSE)}

  }

#### Fit models ##############################################################

  # Since output columns are conditional, I call summ here to see what they will
  # be. I set vifs = FALSE to make sure it isn't fit due to user options.
  the_col_names <- colnames(summ(model, confint = confint, ci.width = ci.width, 
                                 vifs = FALSE)$coeftable)

  # Need to make a matrix filled with NAs to store values from looped
  # model-making
  holdvals <- rep(NA, length(modxvals2) * (length(the_col_names) + 1))
  retmat <- matrix(holdvals, nrow = length(modxvals2))

  # Create a list to hold Johnson-Neyman objects
  jns <- list()

  # Value labels
  colnames(retmat) <- c(paste("Value of ", modx, sep = ""), the_col_names)

  # Create another matrix to hold intercepts (no left-hand column needed)
  retmati <- retmat

  mod2val_len <- length(mod2vals2)
  if (mod2val_len == 0) {mod2val_len <- 1}
  modxval_len <- length(modxvals2)

  # Make empty list to put actual models into
  mods <- rep(list(NA), times = mod2val_len)

  # Make empty list to hold above list if 2nd mod used
  if (!is.null(mod2)) {

    # Make empty list to put each matrix into
    mats <- rep(list(NA), times = mod2val_len)
    imats <- rep(list(NA), times = mod2val_len)

  }

  # Looping through (perhaps non-existent)
  for (j in seq_len(mod2val_len)) {

    # We don't want to do the J-N interval with the 1st moderator adjusted,
    # so we do it here. Requires an extra model fit.

    # Creating extra "copy" of model frame to change for model update
    if (survey == FALSE) {
      dt <- d
    } else {
      # Create new design to modify
      designt <- design

      # Create new df to modify
      dt <- d
    }

    if (!is.null(mod2)) { # We *do* need to adjust the 2nd moderator for J-N

      # The moderator value-adjusted variable
      if (!is.factor(dt[[mod2]])) {
        dt[[mod2]] <- dt[[mod2]] - mod2vals2[j]
      } else {
        dt[[mod2]] <- relevel(dt[[mod2]], ref = mod2vals2[j])
      }

    }

    # Update design
    if (survey == TRUE) {
      designt$variables <- dt
      # Update model
      ## Have to do all this to avoid adding survey to dependencies
      call <- getCall(model)
      call$design <- designt
      call[[1]] <- survey::svyglm
      newmod <- eval(call)
    } else {
      # Creating the model
      newmod <- update(model, data = dt)
    }

    # Getting SEs, robust or otherwise
    if (robust != FALSE) {

      # For J-N
      covmat <- do_robust(newmod, robust, cluster, dt)$vcov

    } else {

      # For J-N
      covmat <- vcov(newmod)

    }

    if (robust == FALSE) {covmat <- NULL}

    if (johnson_neyman == TRUE) {
      args <- list(newmod, pred = pred, modx = modx, vmat = covmat,
                         plot = jnplot, alpha = jnalpha, digits = digits)
      if (exists("jn_args")) {args <- as.list(c(args, jn_args))}
      jn <- do.call("johnson_neyman", args)
    } else {

      jn <- NULL

    }

    if (j != 0) {
        jns[[j]] <- jn
    }

  # Looping so any amount of moderator values can be used
  for (i in seq_along(modxvals2)) {

    dt <- d

    # Create new design to modify
    if (survey == TRUE) {designt <- design}

    # The moderator value-adjusted variable
    if (!is.factor(dt[[modx]])) {
      dt[[modx]] <- dt[[modx]] - modxvals2[i]
    } else {
      dt[[modx]] <- relevel(dt[[modx]], ref = modxvals2[i])
    }

    if (!is.null(mod2)) {

      # The moderator value-adjusted variable
      if (!is.factor(dt[[mod2]])) {
        dt[[mod2]] <- dt[[mod2]] - mod2vals2[j]
      } else {
        dt[[mod2]] <- relevel(dt[[mod2]], ref = mod2vals2[j])
      }


    }

    # Creating the model
    if (survey == TRUE) {
      # Update design
      designt$variables <- dt

      # Update model
      ## Have to do all this to avoid adding survey to dependencies
      call <- getCall(model)
      call$design <- designt
      call[[1]] <- survey::svyglm
      newmod <- eval(call)
    } else {
      newmod <- update(model, data = dt)
    }

    # Getting SEs, robust or otherwise
    if (robust == TRUE) {

      # Use j_summ to get the coefficients
      sum <- jtools::j_summ(newmod, robust = robust, model.fit = FALSE,
                            confint = confint, ci.width = ci.width,
                            vifs = FALSE)

    } else {

      sum <- jtools::j_summ(newmod, model.fit = FALSE, confint = confint,
                            ci.width = ci.width, vifs = FALSE)

    }

    summat <- sum$coeftable
    slopep <- summat[pred, ]
    intp <- summat["(Intercept)", ]

    retmat[i,1] <- modxvals2[i]
    retmat[i,2:ncol(retmat)] <- slopep[]

    retmati[i,1] <- modxvals2[i]
    retmati[i,2:ncol(retmat)] <- intp[]

    mods[[i + (j - 1) * modxval_len]] <- newmod

  }

    if (!is.null(mod2)) {

      mats[[j]] <- retmat
      imats[[j]] <- retmati

      # Now reset the return matrices
      holdvals <- rep(NA, length(modxvals2) * ncol(retmat))
      retmat <- matrix(holdvals, nrow = length(modxvals2))

      # Create another matrix to hold intercepts (no left-hand column needed)
      retmati <- retmat

      # Value labels
      colnames(retmat) <-
        c(paste("Value of ", modx, sep = ""), names(slopep))
      colnames(retmati) <-
        c(paste("Value of ", modx, sep = ""), names(slopep))

    }

  } # end mod2 loop


    ss <- structure(ss, modxvals = modxvals2, robust = robust,
                    cond.int = cond.int, johnson_neyman = johnson_neyman,
                    jnplot = jnplot, jns = jns)

    ss$mods <- mods
    ss$jn <- jns

    if (!is.null(mod2)) {
      ss$slopes <- mats
      ss$ints <- imats
      ss <- structure(ss, mod2vals = mod2vals2)
    } else {
      ss$slopes <- retmat
      ss$ints <- retmati
    }

    class(ss) <- "sim_slopes"

#### build jnplot for 3-way interactions ######################################

  # If 3-way interaction and the user has `cowplot`, here's where we make the
  # final output
  if (!is.null(mod2) & johnson_neyman == TRUE & jnplot == TRUE) {

    # plots <- as.list(rep(NA, length(mod2vals) + 2))
    plots <- as.list(rep(NA, length(mod2vals)))
    the_legend <- NULL

    for (j in seq_along(jns)) {

    # Tell user we can't plot if they don't have cowplot installed
    if (jnplot == TRUE & !is.null(mod2) &
      !requireNamespace("cowplot", quietly = TRUE)) {

      msg <- wrap_str("To plot Johnson-Neyman plots for 3-way interactions,
                       you need the cowplot package.")
      warning(msg)
      jnplot <- FALSE # No more attempts at plotting

    } else if (jnplot == TRUE & !is.null(mod2)) {

      if (is.null(the_legend)) {
        # We save the legend the first time around to use w/ cowplot
        the_legend <-
          cowplot::get_legend(jns[[j]]$plot +
             theme_apa(legend.font.size = 8) +
               ggplot2::theme(legend.position = "bottom"))

        # Now we get rid of it for the actual plotting of the first plot
        jns[[j]]$plot <- jns[[j]]$plot +
          ggplot2::theme(legend.position = "none")

      } else {
        # For each subsequent plot, we don't need to save the legend,
        # just need to get rid of it
        jns[[j]]$plot <- jns[[j]]$plot +
          ggplot2::theme(legend.position = "none")
      }

      # Add a label for cowplot
      mod2lab <- names(mod2vals2)[j]
      if (is.null(mod2lab)) {mod2lab <- mod2vals2[j]}
      jns[[j]]$plot <-
        jns[[j]]$plot + ggplot2::ggtitle(paste(mod2, "=", mod2lab)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 11))

      # Add the plot to the plot list at whatever the current end is
      index <- j
      plots[[index]] <- jns[[j]]$plot

    }

  }

  if (length(plots) %% 2 == 1) {

    plots[[length(plots) + 1]] <- the_legend
    just_plots <- cowplot::plot_grid(plotlist = plots, align = "auto",
                                         ncol = 2, vjust = 0, scale = 1)
    with_legend <- just_plots

  } else {

    just_plots <- cowplot::plot_grid(plotlist = plots, ncol = 2)
    with_legend <- cowplot::plot_grid(just_plots, the_legend,
                                      rel_heights = c(1,.1), nrow = 2)

  }

  # Now we put it all together--vjust is at a non-default level
  ss$jnplot <- with_legend

  } else if (johnson_neyman == TRUE & jnplot == TRUE) {

    ss$jnplot <- jns[[1]]$plot

  }

  if (jnplot == FALSE | johnson_neyman == FALSE) {

    ss$jnplot <- NULL

  }

  return(ss)

}


#### PRINT METHOD ############################################################

#' @export
#' @importFrom cli cat_rule rule
#' @importFrom crayon red bold

print.sim_slopes <- function(x, ...) {

  # This is to make refactoring easier after switch to attributes
  ss <- x
  x <- attributes(x)

  # This helps deal with the fact sometimes mod2vals has no length, so we want
  # to loop just once
  if (!is.null(x$mod2)) {
    length <- length(x$mod2vals)
  } else {
    length <- 1
  }

  # Loop through each value of second moderator...if none, just one loop
  for (j in 1:length) {

    # If we're using second moderator, need to make things make sense
    # to inner loop
    if (!is.null(x$mod2)) {

      m <- NULL
      m$slopes <- as.data.frame(ss$slopes[[j]], stringsAsFactors = FALSE)
      m$ints <- as.data.frame(ss$ints[[j]], stringsAsFactors = FALSE)

      if (class(x$mod2vals) != "character") {
        x$mod2vals <- format(x$mod2vals, nsmall = x$digits, digits = 0)
      }

      # Printing output to make it clear where each batch of second moderator
      # slopes begins
      if (x$def2 == FALSE) {
        cat(rule(center = paste0("While ", x$mod2, " (2nd moderator) ",
                               "= ", x$mod2vals[j]), line = "bar8"), "\n\n")
      } else {
        # If the user went with default +/- SD or used a factor variable,
        # we use the labels
        cat(rule(center = paste0("While ", x$mod2, " (2nd moderator)", " = ",
                 x$mod2vals[j], " (", names(x$mod2vals)[j], ")"),
                 line = "bar8"), "\n\n")
      }


      if (x$johnson_neyman == TRUE) {

        # For 3-way interactions, we don't want each plot being printed
        attributes(x$jns[[j]])$plot <- FALSE
        print(x$jns[[j]])

      }

    } else {
      m <- ss
      m$slopes <- as.data.frame(ss$slopes, stringsAsFactors = FALSE)
      m$ints <- as.data.frame(ss$ints, stringsAsFactors = FALSE)

      if (x$johnson_neyman == TRUE) {
        print(x$jns[[j]])
      }

    }

    # Clearly label simple slopes
    cat(bold(underline("SIMPLE SLOPES ANALYSIS")), "\n\n")

    for (i in seq_along(x$modxvals)) {

      if (class(x$modxvals) != "character") {
        x$modxvals <- format(x$modxvals, nsmall = x$digits, digits = 0)
      }

      # Use the labels for the automatic +/- 1 SD
      if (x$def == TRUE) {

        slopes <- 
          as.data.frame(lapply(m$slopes[i,2:ncol(m$slopes)], as.numeric),
            check.names = FALSE)

        cat(italic(paste0("Slope of ", x$pred, " when ", x$modx, " = ",
            x$modxvals[i], " (", names(x$modxvals)[i], ")",
            ": \n")))
        print(format(slopes, nsmall = x$digits, digits = 0),
              row.names = FALSE)

        # Print conditional intercept
        if (x$cond.int == TRUE) {

          ints <- as.data.frame(lapply(m$ints[i,2:ncol(m$slopes)], as.numeric),
                    check.names = FALSE)

          cat(italic(paste0("Conditional intercept"," when ", x$modx, " = ",
              x$modxvals[i], " (", names(x$modxvals)[i], ")",
              ": \n")))
          print(format(ints, nsmall = x$digits, digits = 0),
                row.names = FALSE)
          cat("\n")
        } else {cat("\n")}

      } else { # otherwise don't use labels

        slopes <-
          as.data.frame(lapply(m$slopes[i, 2:ncol(m$slopes)], as.numeric),
            check.names = FALSE)

        cat(italic(paste0("Slope of ", x$pred, " when ", x$modx, " = ",
            x$modxvals[i],
            ": \n")))
        print(format(slopes, nsmall = x$digits, digits = 0), row.names = FALSE)

        # Print conditional intercept
        if (x$cond.int == TRUE) {

          ints <- as.data.frame(lapply(m$ints[i, 2:ncol(m$slopes)], as.numeric),
                    check.names = FALSE)

          cat(italic(paste0("Conditional intercept", " when ", x$modx, " = ",
              x$modxvals[i], ": \n")))
          print(format(ints, nsmall = x$digits, digits = 0), row.names = FALSE)
          cat("\n")
        } else {cat("\n")}

      }
    }
  } # end mod2 loop

  if (!is.null(x$mod2) && x$jnplot == TRUE) {
    print(ss$jnplot)
  }

}

