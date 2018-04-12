#' Probe interaction effects via simple slopes and plotting
#'
#' \code{probe_interaction} is a convenience function that allows users to call
#' both \code{\link{sim_slopes}} and \code{\link{interact_plot}} with a single
#' call.
#'
#' @usage probe_interaction(model, pred, modx, mod2 = NULL, ...)
#'
#' @param model A regression model of type \code{lm} or
#'    \code{\link[survey]{svyglm}}.
#'    It should contain the interaction of interest.
#'
#' @param pred The predictor variable involved in the interaction.
#'
#' @param modx The moderator variable involved in the interaction.
#'
#' @param mod2 Optional. The name of the second moderator
#'  variable involved in the interaction.
#'
#' @param ... Other arguments accepted by \code{\link{sim_slopes}} and
#'  \code{\link{interact_plot}}
#'
#' @details
#'
#' This function simply merges the nearly-equivalent arguments needed to call
#' both \code{\link{sim_slopes}} and \code{\link{interact_plot}} without the
#' need for re-typing their common arguments. Note that each function is called
#' separately and they re-fit a separate model for each level of each
#' moderator; therefore, the runtime may be considerably longer than the
#' original model fit. For larger models, this is worth keeping in mind.
#'
#' Sometimes, you may want different parameters when doing simple slopes
#' analysis compared to when plotting interaction effects. For instance, it is
#' often easier to interpret the regression output when variables are
#' standardized; but plots are often easier to understand when the variables
#' are in their original units of measure.
#'
#' \code{probe_interaction} does not
#' support providing different arguments to each function. If that is needed,
#' use \code{sim_slopes} and \code{interact_plot} directly.
#'
#' @return
#'
#' \item{simslopes}{The \code{sim_slopes} object created.}
#' \item{interactplot}{The \code{ggplot} object created by
#' \code{interact_plot}.}
#'
#' @family interaction tools
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @examples
#'
#' # Using a fitted model as formula input
#' fiti <- lm(Income ~ Frost + Murder * Illiteracy,
#'   data=as.data.frame(state.x77))
#' probe_interaction(model = fiti, pred = Murder, modx = Illiteracy,
#'                   modxvals = "plus-minus")
#' # 3-way interaction
#' fiti3 <- lm(Income ~ Frost * Murder * Illiteracy,
#'   data=as.data.frame(state.x77))
#' probe_interaction(model = fiti3, pred = Murder, modx = Illiteracy,
#'                   mod2 = Frost, mod2vals = "plus-minus")
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell * meals + sch.wide, design = dstrat)
#' probe_interaction(model = regmodel, pred = ell, modx = meals,
#'                   modxvals = "plus-minus", cond.int = TRUE)
#'
#' # 3-way with survey and factor input
#' regmodel3 <- svyglm(api00 ~ ell * meals * sch.wide, design = dstrat)
#' probe_interaction(model = regmodel3, pred = ell, modx = meals,
#'                   mod2 = sch.wide)
#' # Can try different configurations of 1st vs 2nd mod
#' probe_interaction(model = regmodel3, pred = ell, modx = sch.wide,
#'                   mod2 = meals)
#'
#' @export


probe_interaction <- function(model, pred, modx, mod2 = NULL, ...) {

  # Create list of acceptable arguments to both functions
  ssnames <- names(formals(sim_slopes))
  ipnames <- names(formals(interact_plot))

  # Capture the arguments
  dots <- eval(substitute(alist(...)))

  # Capture explicit args
  args <- match.call()

  # Add the actual arguments
  dots <- list(unlist(dots), model = args$model, modx = args$modx,
               pred = args$pred, mod2 = args$mod2)
  dots <- unlist(dots)

  # Create list of arguments accepted by sim_slopes
  ssargs <- dots[names(dots) %in% ssnames]
  # Create list of arguments accepted by interact_plot
  ipargs <- dots[names(dots) %in% ipnames]

  # the "alpha" argument in johnson_neyman is "jnalpha" for sim_slopes
  if ("alpha" %in% names(dots)) {
    ssargs[["jnalpha"]] <- dots[["alpha"]]
  }

  # Call sim_slopes
  ss <- do.call("sim_slopes", ssargs)
  # Call interact_plot
  ip <- do.call("interact_plot", ipargs)

  # Save both to output object
  out <- list()
  out$simslopes <- ss
  out$interactplot <- ip

  # Set class for print method
  class(out) <- "probe_interaction"

  return(out)

}

#' @export

print.probe_interaction <- function(x, ...) {

  print(x$simslopes)
  print(x$interactplot)

}

#### Non-exported functions ###################################################

## Utility function for getting values of moderator values for interaction
## functions

mod_vals <- function(d, modx, modxvals, survey, weights,
                     design = design, modx.labels = NULL,
                     any.mod2 = FALSE, is.mod2 = FALSE,
                     sims = FALSE) {

  # Get moderator mean
  if (survey == FALSE & !is.factor(d[[modx]])) {

    modmean <- weighted.mean(d[[modx]], weights, na.rm = TRUE)
    modsd <- wtd.sd(d[[modx]], weights)

  } else if (survey == TRUE & !is.factor(d[[modx]])) {

    modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
    # Have to construct the formula this way since the syntax for svymean
    # differs from mean
    modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")),
                               design = design)

  }

  # Testing whether modxvals refers to pre-defined arg or list of factor levels
  char1 <- FALSE
  if (is.character(modxvals) & length(modxvals) == 1) {char1 <- TRUE}

  # If using a preset, send to auto_mod_vals function
  if (!is.factor(d[[modx]]) & (is.null(modxvals) | is.character(modxvals))) {

    modxvals2 <- auto_mod_vals(d, modxvals = modxvals, modx = modx,
                               modmean = modmean, modsd = modsd,
                               modx.labels = modx.labels, mod2 = is.mod2,
                               sims = sims)

  }

  # For user-specified numbers or factors, go here
  if (is.null(modxvals) && is.factor(d[[modx]])) {

    modxvals2 <- levels(d[[modx]])
    if (is.null(modx.labels)) {

      modx.labels <- levels(d[[modx]])

    }
    names(modxvals2) <- modx.labels

  } else if (!is.null(modxvals) & (is.numeric(modxvals) | char1 == FALSE)) {
    # Use user-supplied values otherwise

    if (!is.null(modx.labels)) {
      # What I'm doing here is preserving the label order
      names(modxvals) <- modx.labels
      if (!is.mod2 & !is.factor(d[[modx]])) {
        modxvals2 <- rev(modxvals)
      } else {
        modxvals2 <- modxvals
      }
      modx.labels <- names(modxvals2)

    } else {

      names(modxvals) <- modxvals
      if (!is.mod2 & !is.factor(d[[modx]])) {
        modxvals2 <- rev(modxvals)
      } else {
        modxvals2 <- modxvals
      }
      modx.labels <- names(modxvals2)

    }

  }

  if (is.null(modx.labels)) {
    # Name the modx.labels object with modxvals2 names

    modx.labels <- names(modxvals2)

  }

  if (is.numeric(modxvals2)) {
    # The proper order for interact_plot depends on presence of second moderator
    modxvals2 <- sort(modxvals2, decreasing = !any.mod2)

  }

  return(modxvals2)

}

## Gets the preset values, e.g., mean plus/minus 1 SD

auto_mod_vals <-
  function(d, modx, modxvals, modmean, modsd, modx.labels = NULL,
           mod2 = FALSE, sims = FALSE) {

    # Default to +/- 1 SD unless modx is factor
    if (is.null(modxvals) & length(unique(d[[modx]])) > 2) {

      modxvals2 <- c(modmean + modsd,
                     modmean,
                     modmean - modsd)
      if (mod2 == FALSE) {
        names(modxvals2) <- c("+ 1 SD", "Mean", "- 1 SD")
      } else {
        names(modxvals2) <- c(paste("Mean of", modx, "+ 1 SD"),
                              paste("Mean of", modx),
                              paste("Mean of", modx, "- 1 SD"))
      }

    } else if (!is.null(modxvals) && modxvals == "plus-minus") { # No mean

      modxvals2 <- c(modmean + modsd, modmean - modsd)
      if (mod2 == FALSE) {
        names(modxvals2) <- c("+ 1 SD", "- 1 SD")
      } else {
        names(modxvals2) <- c(paste("Mean of", modx, "+ 1 SD"),
                              paste("Mean of", modx, "- 1 SD"))
      }

    } else if (!is.null(modxvals) && modxvals == "terciles") {

      x_or_2 <- switch(as.character(mod2),
                       "TRUE" = "2",
                       "FALSE" = "x")
      group_name <- paste0("mod", x_or_2)
      d[[group_name]] <- cut2(d[[modx]], g = 3, levels.mean = TRUE)
      modxvals2 <- as.numeric(levels(d[[group_name]]))

      if (mod2 == FALSE) {
        names(modxvals2) <- c("Lower tercile", "Middle tercile",
                              "Upper tercile")
      } else {
        names(modxvals2) <- c(paste("Lower tercile of", modx),
                              paste("Middle tercile of", modx),
                              paste("Upper tercile of", modx))
      }

    } else if (is.null(modxvals) & length(unique(d[[modx]])) == 2) {

      modxvals2 <- as.numeric(levels(factor(d[[modx]])))
      if (!is.null(modx.labels)) {

        names(modxvals2) <- modx.labels

      } else {

        if (mod2 == TRUE & sims == FALSE) {
          names(modxvals2) <-
            sapply(modxvals2, FUN = function(x) {paste(modx, "=", round(x,3))})
        } else {
          names(modxvals2) <- modxvals2
        }

      }

    }

    return(modxvals2)

  }


## Centering

center_ss <- function(d, weights, facvars = NULL, fvars, pred, resp, modx,
                        survey, design = NULL, mod2, wname, offname, centered) {

  # Just need to pick a helper function based on survey vs no survey
  if (survey == TRUE) {

    out <- center_ss_survey(d, weights, facvars, fvars, pred, resp, modx,
                              survey, design, mod2, wname, offname, centered)

  } else {

    out <- center_ss_non_survey(d, weights, facvars, fvars, pred, resp, modx,
                                  mod2, wname, offname, centered)

  }

  return(out)


}

## If not svydesign, centering is fairly straightforward

center_ss_non_survey <- function(d, weights, facvars = NULL, fvars, pred,
                                   resp, modx, mod2, wname, offname, centered) {

  omitvars <- c(resp, modx, mod2, wname, offname)

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (centered != "all" && centered != "none") {

    if (any(omitvars %in% centered)) {
      warning("Moderators, outcome variables, and weights/offsets",
              " cannot be centered.")
      centered <- centered[centered %nin% omitvars]
    }
    if (length(centered) > 0) {
      d <- gscale(vars = centered, data = d, center.only = TRUE,
                  weights = weights)
    }

    for (v in fv2) {

      if (is.factor(d[[v]]) &&
          length(unique(d[[v]])) == 2 && v %nin% centered) {

        facvars <- c(facvars, v)

      }

    }

  } else if (centered == "all") {

    # saving all vars except response
    vars <- names(d)[names(d) %nin% omitvars]

    d <- gscale(vars = vars, data = d, center.only = TRUE,
                weights = weights)

  } else if (centered == "none") {

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[[v]]) & length(unique(d[[v]])) == 2) {

        facvars <- c(facvars, v)

      }
    }

  }

  # Fixes a data type error with predict() later
  d <- as.data.frame(d)

  out <- list(d = d, facvars = facvars, fvars = fvars, design = NULL)

  return(out)

}

## Svydesigns get their own function to make control flow easier to follow

center_ss_survey <- function(d, weights, facvars = NULL, fvars, pred, resp,
                             modx, survey, design, mod2, wname, offname,
                             centered) {

  omitvars <- c(resp, modx, mod2, wname, offname)

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (centered != "all" && centered != "none") {

    if (any(omitvars %in% centered)) {
      warning("Moderators, outcome variables, and weights/offsets",
              " cannot be centered.")
      centered <- centered[centered %nin% omitvars]
    }
    design <- gscale(vars = centered, data = design, center.only = TRUE)
    d <- design$variables

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[[v]]) &&
          length(unique(d[[v]])) == 2 && v %nin% centered) {

        facvars <- c(facvars, v)

      }
    }

  } else if (centered == "none") {

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[[v]]) && length(unique(d[[v]])) == 2) {

        facvars <- c(facvars, v)

      }
    }

  } else if (centered == "all") {

    # Center all non-focal
    ndfvars <- fvars[fvars %nin% omitvars]

    if (length(ndfvars) > 0) {
      design <- gscale(vars = ndfvars, data = design, center.only = TRUE)
      d <- design$variables
    }

  }

  out <- list(d = d, design = design, facvars = facvars, fvars = fvars)

  return(out)

}

## Centering (w/o data change)

center_values <- function(d, weights, omitvars, survey, design = NULL,
                          centered) {

  # Just need to pick a helper function based on survey vs no survey
  if (survey == TRUE) {

    out <- center_values_survey(d, omitvars, design = design, centered)

  } else {

    out <- center_values_non_survey(d, weights, omitvars, centered)

  }

  return(out)


}

## If not svydesign, centering is fairly straightforward

center_values_non_survey <- function(d, weights, omitvars, centered) {

  # Handling user-requested centered vars
  if (centered != "all" && centered != "none") {

    if (any(omitvars %in% centered)) {
      warning("Focal predictors, outcome variables, and weights/offsets",
              " cannot be centered.")
      centered <- centered[centered %nin% omitvars]
    }
    if (length(centered) > 0) {
      vals <- sapply(d[centered], weighted.mean, weights = weights,
                     na.rm = TRUE)
    }

  } else if (centered == "all") {

    # Centering the non-focal variables to make the slopes more interpretable
    vars <- names(d)[names(d) %nin% omitvars]

    if (length(vars) > 0) {
      vals <- sapply(d[vars], weighted.mean, weights = weights)
    }

  } else if (centered == "none") {

    # Do nothing

  }

  if (!exists("vals")) {
    vals <- NULL
  }

  out <- list(vals = vals)

  return(out)

}

## Svydesigns get their own function to make control flow easier to follow

center_values_survey <- function(d, omitvars, design = NULL,
                                 centered) {

  # Handling user-requested centered vars
  if (centered != "all" && centered != "none") {

    if (any(omitvars %in% centered)) {
      warning("Focal predictors, outcome variables, and weights/offsets",
              " cannot be centered.")
      centered <- centered[centered %nin% omitvars]
    }

    if (length(centered) > 0) {
      vals <- survey::svymean(survey::make.formula(centered), design)
    }
    d <- design$variables

  } else if (centered == "all") {

    vars <- names(d)[names(d) %nin% omitvars]

    if (length(vars) > 0) {
      vals <- survey::svymean(survey::make.formula(vars), design)
    }

  } else if (centered == "none") {

    # Do nothing

  }

  # Need to coerce svystat object to vector, which makes it lose names
  if (exists("vals")) {
    valnames <- names(vals)
    vals <- as.vector(vals)
    names(vals) <- valnames
  } else {
    vals <- NULL
  }

  out <- list(vals = vals)

  return(out)

}

#### Send deprecation warnings ##############################################

ss_dep_check <- function(fun_name, dots) {

  dep_names <- c("scale", "standardize")
  if (any(names(dots) %in% dep_names)) {
    warn_wrap(fun_name, " no longer supports variable scaling. You can use
              gscale to scale your data or scale_mod to scale your model.")
  }

}

#### predict helpers ########################################################

get_offname <- function(model, survey) {

  # subset gives bare name
  offname <-
    as.character(getCall(model)$offset)[length(getCall(model)$offset)]
  # Sometimes it's character(0)
  if (length(offname) == 0) {offname <- NULL}

  if (is.null(offname)) {
    index <- attr(terms(model), "offset")
    offname <- all.vars(terms(model))[index]
  }

  return(offname)

}

data_checks <- function(model, data, predvals = NULL,
                        modxvals, mod2vals, pred.labels, modx.labels,
                        mod2.labels, env = parent.frame()) {

  # Avoid CRAN barking
  d <- facvars <- wts <- wname <- NULL

  # Duplicating the dataframe so it can be manipulated as needed
  if (is.null(data)) {
    d <- model.frame(model)
    # Check to see if model.frame names match formula names
    varnames <- names(d)
    # Drop weights and offsets
    varnames <- varnames[varnames %nin% c("(offset)","(weights)")]
    if (any(varnames %nin% all.vars(as.formula(formula(model))))) {

      warn_wrap("Variable transformations in the model formula
      detected. Trying to use ", as.character(getCall(model)$data), " from
      global environment. This could cause incorrect results if ",
      as.character(getCall(model)$data), " has been altered since the model was
      fit. You can manually provide the data to the \"data =\" argument.",
      call. = FALSE)

      d <- data <- eval(getCall(model)$data)
    }
  } else {
    d <- data
  }

  if (length(predvals) == 1) {stop("predvals must be at least a length of 2.",
                                   call. = FALSE)}
  if (length(modxvals) == 1 && modxvals %nin% c("plus-minus", "terciles")) {
    stop("modxvals must be at least a length of 2.",
         call. = FALSE)
  }
  if (length(mod2vals) == 1 && mod2vals %nin% c("plus-minus", "terciles")) {
    stop("mod2vals must be at least a length of 2.",
          call. = FALSE)
  }

  # weights?
  if (("(weights)" %in% names(d) | !is.null(getCall(model)$weights))) {
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

  facvars <- names(which(sapply(d, function(x) {
    is.factor(x) | is.character(x)
    })
  ))

  env$facvars <- facvars
  env$d <- d
  env$wts <- wts
  env$weights <- weights
  env$wname <- wname

}

prep_data <- function(model, d, pred, modx, mod2, predvals = NULL, modxvals,
                      mod2vals, survey, pred.labels = NULL, modx.labels,
                      mod2.labels, wname, weights, wts, linearity.check,
                      interval, set.offset, facvars, centered,
                      preds.per.level, force.cat = FALSE) {
  # offset?
  if (!is.null(model.offset(model.frame(model)))) {

    off <- TRUE
    offname <- get_offname(model, survey)

  } else {

    off <- FALSE
    offname <- NULL

  }

  if (is.factor(d[[pred]]) | is.character(d[[pred]])) {
    facpred <- TRUE
    if (is.character(d[[pred]])) {d[[pred]] <- factor(d[[pred]])}
  } else if (force.cat == FALSE) {
    facpred <- FALSE
  } else {
    facpred <- TRUE
  }

  # Setting default for colors
  if (!is.null(modx) && (is.factor(d[[modx]]) | is.character(d[[modx]]))) {
    facmod <- TRUE
    if (is.character(d[[modx]])) {d[[modx]] <- factor(d[[modx]])}
    # # Unrelated, but good place to throw a warning
    # if (!is.null(modxvals) && length(modxvals) != nlevels(d[[modx]])) {
    #   warning("All levels of factor must be used. Ignoring modxvals",
    #           " argument...")
    #   modxvals <- NULL
    # }
  } else if (force.cat == FALSE | is.null(modx)) {
    facmod <- FALSE
  } else if (!is.null(modx)) {
    facmod <- TRUE
  }

  # Fix character mod2 as well
  if (!is.null(mod2) && is.factor(d[[mod2]])) {
    facmod2 <- TRUE
  } else if (!is.null(mod2) && is.character(d[[mod2]])) {
    d[[mod2]] <- factor(d[[mod2]])
    facmod2 <- TRUE
  } else if (force.cat == FALSE | is.null(mod2)) {
    facmod2 <- FALSE
  } else if (!is.null(mod2)) {
    facmod2 <- TRUE
  }


  # Get the formula from lm object if given
  formula <- as.formula(formula(model))

  # Pulling the name of the response variable for labeling
  resp <- all.vars(formula)[1]

  # Drop unneeded columns from data frame
  if (off == TRUE) {offs <- d[[offname]]}
  d <- d[all.vars(formula)]
  # For setting dimensions correctly later
  nc <- sum(names(d) %nin% c(wname, offname))
  if (off == TRUE) {d[[offname]] <- offs}


### Centering ##################################################################

  # Update facvars by pulling out all non-focals
  facvars <-
    facvars[facvars %nin% c(pred, resp, modx, mod2, wname, offname)]

  # Create omitvars variable; we don't center any of these
  omitvars <- c(pred, resp, modx, mod2, wname, offname, facvars,
                "(weights)", "(offset)")

  if (survey == FALSE) {
    design <- NULL
  } else {
    design <- model$survey.design
  }

  # Use utility function shared by all interaction functions
  c_out <- center_values(d = d, weights = wts, omitvars = omitvars,
                         survey = survey, design = design, centered = centered)

  vals <- c_out$vals

### Getting moderator values ##################################################

  if (facpred == TRUE) {

    predvals <- mod_vals(d = d, modx = pred, modxvals = predvals,
                         survey = survey, weights = wts,
                         design = design,
                         modx.labels = pred.labels, is.mod2 = TRUE)
    pred.labels <- names(predvals)

  }

  if (!is.null(modx)) {

    modxvals2 <- mod_vals(d = d, modx = modx, modxvals = modxvals,
                          survey = survey, weights = wts,
                          design = design,
                          modx.labels = modx.labels, any.mod2 = !is.null(mod2))
    modx.labels <- names(modxvals2)

  } else {

    modxvals2 <- NULL

  }

  if (!is.null(mod2)) {

    mod2vals2 <- mod_vals(d = d, modx = mod2, modxvals = mod2vals,
                          survey = survey, weights = wts,
                          design = design,
                          modx.labels = mod2.labels, any.mod2 = !is.null(mod2),
                          is.mod2 = TRUE)
    mod2.labels <- names(mod2vals2)

  } else {

    mod2vals2 <- NULL

  }

### Drop unwanted factor levels ###############################################


  if (facpred == TRUE && !is.numeric(d[[pred]])) {

    d <- drop_factor_levels(d = d, var = pred, values = predvals,
                            labels = pred.labels)

  }

  if (facmod == TRUE && !is.numeric(d[[modx]])) {

    d <- drop_factor_levels(d = d, var = modx, values = modxvals2,
                            labels = modx.labels)

  }

  if (facmod2 == TRUE && !is.numeric(d[[mod2]])) {

    d <- drop_factor_levels(d = d, var = mod2, values = mod2vals2,
                            labels = mod2.labels)

  }


### Prep original data for splitting into groups ##############################

  # Only do this if going to plot points
  if (!is.null(modx)) {
    d <- split_int_data(d = d, modx = modx, mod2 = mod2,
                        linearity.check = linearity.check, modxvals = modxvals,
                        modxvals2 = modxvals2, mod2vals = mod2vals,
                        mod2vals2 = mod2vals2)
  }

#### Creating predicted frame #################################################

  if (facpred == FALSE) {
    pm <- make_pred_frame_cont(d = d, pred = pred, modx = modx,
                          modxvals2 = modxvals2, mod2 = mod2,
                          mod2vals2 = mod2vals2, interval = interval, nc = nc,
                          facvars = facvars, off = off, offname = offname,
                          set.offset = set.offset, vals = vals, resp = resp,
                          wname = wname, preds.per.level = preds.per.level)
  } else {
    pm <- make_pred_frame_cat(d = d, pred = pred, modx = modx,
                          modxvals2 = modxvals2, mod2 = mod2,
                          mod2vals2 = mod2vals2, interval = interval, nc = nc,
                          facvars = facvars, off = off, offname = offname,
                          set.offset = set.offset, vals = vals, resp = resp,
                          wname = wname, preds.per.level = preds.per.level)
  }

  out <- list(pm = pm, d = d, resp = resp, facmod = facmod,
              predvals = predvals, pred.labels = pred.labels,
              modxvals2 = modxvals2, modx.labels = modx.labels,
              mod2vals2 = mod2vals2, mod2.labels = mod2.labels)
  return(out)

}

split_int_data <- function(d, modx, mod2, linearity.check, modxvals, modxvals2,
                           mod2vals, mod2vals2) {
  if ((!is.null(mod2) | linearity.check == TRUE) & !is.factor(d[[modx]])) {

    # Use ecdf function to get quantile of the modxvals
    mod_val_qs <- ecdf(d[[modx]])(sort(modxvals2))

    # Now I am going to split the data in a way that roughly puts each modxval
    # in the middle of each group. mod_val_qs is a vector of quantiles for each
    # modxval, so I will now build a vector of the midpoint between each
    # neighboring pair of quantiles — they will become the cutpoints for
    # splitting the data into groups that roughly correspond to the modxvals
    cut_points <- c() # empty vector
    # Iterate to allow this to work regardless of number of modxvals
    for (i in 1:(length(modxvals2) - 1)) {

      cut_points <- c(cut_points, mean(mod_val_qs[i:(i + 1)]))

    }

    # Add Inf to both ends to encompass all values outside the cut points
    cut_points <- c(-Inf, quantile(d[[modx]], cut_points), Inf)

    # Create variable storing this info as a factor
    d$modx_group <- cut(d[[modx]], cut_points, labels = names(sort(modxvals2)))

    if (!is.null(modxvals) && modxvals == "terciles") {
      d$modx_group <- factor(cut2(d[[modx]], g = 3, levels.mean = TRUE),
                             labels = c("Lower tercile", "Middle tercile",
                                        "Upper tercile"))
    }

  }

  if (!is.null(mod2) && !is.factor(d[[mod2]])) {

    mod_val_qs <- ecdf(d[[mod2]])(sort(mod2vals2))


    cut_points2 <- c()
    for (i in 1:(length(mod2vals2) - 1)) {

      cut_points2 <- c(cut_points2, mean(mod_val_qs[i:(i + 1)]))

    }

    cut_points2 <- c(-Inf, quantile(d[[mod2]], cut_points2), Inf)

    d$mod2_group <- cut(d[[mod2]], cut_points2,
                        labels = names(sort(mod2vals2)))

    if (!is.null(mod2vals) && mod2vals == "terciles") {
      d$mod2_group <- factor(cut2(d[[mod2]], g = 3, levels.mean = TRUE),
                             labels = c(paste("Lower tercile of", mod2),
                                        paste("Middle tercile of", mod2),
                                        paste("Upper tercile of", mod2)))
    }

  }

  return(d)

}

make_pred_frame_cont <- function(d, pred, modx, modxvals2, mod2, mod2vals2,
                            interval, nc, facvars, off, offname, set.offset,
                            vals, resp, wname, preds.per.level) {

  # Makes accommodating effect_plot easier
  num_levels <- max(c(1, length(modxvals2)))

  # Creating a set of dummy values of the focal predictor for use in predict()
  xpreds <- seq(from = range(d[!is.na(d[[pred]]), pred])[1],
                to = range(d[!is.na(d[[pred]]), pred])[2],
                length.out = preds.per.level)
  xpreds <- rep(xpreds, num_levels)

  # Skip if effect_plot
  if (!is.null(modx)) {
    # Create values of moderator for use in predict()
    facs <- rep(modxvals2[1], preds.per.level)

    # Looping here allows for a theoretically limitless amount of
    # moderator values
    for (i in 2:length(modxvals2)) {
      facs <- c(facs, rep(modxvals2[i], preds.per.level))
    }
  } else {
    facs <- 1
  }

  # Takes some rejiggering to get this right with second moderator
  if (!is.null(mod2)) {
    # facs and xpreds will be getting longer, so we need originals for later
    facso <- facs
    xpredso <- xpreds
    # facs2 is second moderator. Here we are creating first iteration of values
    facs2 <- rep(mod2vals2[1], length(facs))
    # Now we create the 2nd through however many levels iterations
    for (i in 2:length(mod2vals2)) {
      # Add the next level of 2nd moderator to facs2
      # the amount depends on the how many values were in *original* facs
      facs2 <- c(facs2, rep(mod2vals2[i], length(facso)))
      # We are basically recreating the whole previous set of values, each
      # with a different value of 2nd moderator. They have to be in order
      # since we are using geom_path() later.
      facs <- c(facs, facso)
      xpreds <- c(xpreds, xpredso)
    }
  }

  # Creating matrix for use in predict()
  if (interval == TRUE) { # Only create SE columns if intervals needed
    if (is.null(mod2)) {
      pm <- matrix(rep(0, preds.per.level * (nc + 2) * num_levels),
                   ncol = (nc + 2))
    } else {
      pm <- matrix(rep(0, (nc + 2) * length(facs)), ncol = (nc + 2))
    }
  } else {
    if (is.null(mod2)) {
      pm <- matrix(rep(0, preds.per.level * nc * num_levels), ncol = nc)
    } else {
      pm <- matrix(rep(0, nc * length(facs)), ncol = nc)
    }
  }

  # Naming columns
  if (interval == TRUE) { # if intervals, name the SE columns
    colnames(pm) <- c(colnames(d)[colnames(d) %nin% c("modx_group","mod2_group",
                                                      offname, wname)],
                      "ymax", "ymin")
  } else {
    colnames(pm) <- c(colnames(d)[colnames(d) %nin% c("modx_group",
                                                      "mod2_group", offname,
                                                      wname)])
  }
  # Convert to dataframe
  pm <- as.data.frame(pm)
  # Add values of moderator to df
  if (!is.null(modx)) {
    pm[[modx]] <- facs
  }
  if (!is.null(mod2)) { # if second moderator
    pm[[mod2]] <- facs2
  }

  # Add values of focal predictor to df
  pm[[pred]] <- xpreds

  # Set factor predictors arbitrarily to their first level
  if (length(facvars) > 0) {
    for (v in facvars) {
      pm[[v]] <- levels(d[[v]])[1]
    }
  }

  if (off == TRUE) {
    if (is.null(set.offset)) {
      offset.num <- median(d[[offname]])
    } else {
      offset.num <- set.offset
    }

    pm[[offname]] <- offset.num
    msg <- paste("Outcome is based on a total of", offset.num, "exposures")
    message(msg)
  }

  # Adding mean values to newdata in lieu of actually re-fitting model
  if (!is.null(vals)) {
    vals <- vals[names(vals) %nin% c(offname,modx,mod2,pred,resp)]
    for (var in names(vals)) {
      pm[[var]] <- rep(vals[var], times = nrow(pm))
    }
  }

  return(pm)

}

make_pred_frame_cat <- function(d, pred,
                                modx, modxvals2, mod2, mod2vals2,
                                interval, nc, facvars, off, offname, set.offset,
                                vals, resp, wname, preds.per.level) {
  # Creating a set of dummy values of the focal predictor for use in predict()
  pred_len <- length(unique((d[[pred]])))

  if (!is.null(modx)) {
    combos <- expand.grid(unique(d[[pred]]), unique(d[[modx]]))
    combo_pairs <- paste(combos[[1]], combos[[2]])
    og_pairs <- paste(d[[pred]], d[[modx]])
    combos <- combos[combo_pairs %in% og_pairs,]
    xpred_len <- nrow(combos)
  } else {
    xpred_len <- pred_len
    combos <- as.data.frame(unique(d[[pred]]))
  }

  # Takes some rejiggering to get this right with second moderator
  if (!is.null(mod2)) {
    combos <- expand.grid(unique(d[[pred]]), unique(d[[modx]]),
                          unique(d[[mod2]]))
    combo_pairs <- paste(combos[[1]], combos[[2]], combos[[3]])
    og_pairs <- paste(d[[pred]], d[[modx]], d[[mod2]])
    combos <- combos[combo_pairs %in% og_pairs,]
    xpred_len <- nrow(combos)
  }

  # Creating matrix for use in predict()
  if (interval == TRUE) { # Only create SE columns if intervals needed
    pm <- matrix(rep(0, xpred_len * (nc + 2)), ncol = (nc + 2))
  } else {
    pm <- matrix(rep(0, xpred_len * nc), ncol = nc)
  }

  # Naming columns
  if (interval == TRUE) { # if intervals, name the SE columns
    colnames(pm) <- c(colnames(d), "ymax", "ymin")
  } else {
    colnames(pm) <- colnames(d)
  }

  # Convert to dataframe
  pm <- as.data.frame(pm)

  # Add values of moderator to df
  if (!is.null(modx)) {
    pm[[modx]] <- combos[[2]]
  }
  if (!is.null(mod2)) { # if second moderator
    pm[[mod2]] <- combos[[3]]
  }

  # Add values of focal predictor to df
  pm[[pred]] <- combos[[1]]

  # Set factor covariates arbitrarily to their first level
  if (length(facvars) > 0) {
    for (v in facvars) {
      pm[[v]] <- levels(d[[v]])[1]
    }
  }

  if (off == TRUE) {
    if (is.null(set.offset)) {
      offset.num <- median(d[[offname]])
    } else {
      offset.num <- set.offset
    }

    pm[[offname]] <- offset.num
    msg <- paste("Outcome is based on a total of", offset.num, "exposures\n")
    message(msg)
  }

  # Adding mean values to newdata in lieu of actually re-fitting model
  if (!is.null(vals)) {
    vals <- vals[names(vals) %nin% c(offname, modx, mod2, pred, resp)]
    for (var in names(vals)) {
      pm[[var]] <- rep(vals[var], times = nrow(pm))
    }
  }

  return(pm)

}

drop_factor_levels <- function(d, var, values, labels) {

  d <- d[d[[var]] %in% values,]
  d[[var]] <- factor(d[[var]], levels = values, labels = labels)
  return(d)

}
