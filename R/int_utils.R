#' Probe interaction effects via simple slopes and plotting
#'
#' \code{probe_interaction} is a convenience function that allows users to call
#' both \code{\link{sim_slopes}} and \code{\link{interact_plot}} with a single
#' call.
#'
#' @usage probe_interaction(model, pred, modx, mod2 = NULL, ...)
#'
#' @param model A regression model of type \code{lm} or \code{\link[survey]{svyglm}}.
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
                     any.mod2 = FALSE, is.mod2 = FALSE, sims = FALSE) {

  # Get moderator mean
  if (survey == FALSE & !is.factor(d[,modx])) {

    modmean <- weighted.mean(d[,modx], weights, na.rm = TRUE)
    modsd <- wtd.sd(d[,modx], weights)

  } else if (survey == TRUE & !is.factor(d[,modx])) {

    modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
    # Have to construct the formula this way since the syntax for svymean
    # differs from mean
    modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")),
                               design = design)

  }

  # Testing whether modxvals refers to pre-defined arg or list of factor levels
  char1 <- FALSE
  if (is.character(modxvals) & length(modxvals) > 1) {char1 <- TRUE}

  # If using a preset, send to auto_mod_vals function
  if (!is.factor(d[,modx]) & (is.null(modxvals) | is.character(modxvals))) {

    modxvals2 <- auto_mod_vals(d, modxvals = modxvals, modx = modx,
                               modmean = modmean, modsd = modsd,
                               modx.labels = modx.labels, mod2 = is.mod2,
                               sims = sims)

  }

  # For user-specified numbers or factors, go here
  if (is.null(modxvals) && is.factor(d[,modx]) && sims == FALSE) {

    modxvals2 <- levels(d[,modx])
    if (is.null(modx.labels)) {

      modx.labels <- levels(d[,modx])

    }
    names(modxvals2) <- modx.labels

  } else if (is.factor(d[,modx]) & sims == TRUE) {

    if (length(unique(d[,modx])) == 2) {
      # We can work with a two-level factor
      names <- levels(d[,modx])
      condition <- suppressWarnings(all(is.na(as.numeric(levels(d[,modx])))))

      if (condition) {
        modxvals2 <- c(0,1)
      } else {
        modxvals2 <- sort(as.numeric(levels(d[,modx])), decreasing = FALSE)
      }

      names(modxvals2) <- names
    } else if (length(unique(d[,modx])) != 2) {

      stop("Factor moderators can only have exactly 2 levels.")

    }

  } else if (!is.null(modxvals) & (is.numeric(modxvals) | char1 == TRUE)) {
    # Use user-supplied values otherwise

    if (!is.null(modx.labels)) {
      # What I'm doing here is preserving the label order
      names(modxvals) <- modx.labels
      modxvals2 <- sort(modxvals, decreasing = T)
      modx.labels <- names(modxvals2)

    } else {

      names(modxvals) <- modxvals
      modxvals2 <- sort(modxvals, decreasing = T)
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
    if (is.null(modxvals) & length(unique(d[,modx])) > 2) {

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

    } else if (is.null(modxvals) & length(unique(d[,modx])) == 2) {

      modxvals2 <- as.numeric(levels(factor(d[,modx])))
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

center_vals <- function(d, weights, facvars = NULL, fvars, pred, resp, modx,
                        survey, design = NULL, mod2, wname, offname, centered,
                        scale, n.sd) {

  # Just need to pick a helper function based on survey vs no survey
  if (survey == TRUE) {

    out <- center_vals_survey(d, weights, facvars, fvars, pred, resp, modx,
                              survey, design, mod2, wname, offname, centered,
                              scale, n.sd)

  } else {

    out <- center_vals_non_survey(d, weights, facvars, fvars, pred, resp, modx,
                                  mod2, wname, offname, centered,
                                  scale, n.sd)

  }

  return(out)


}

## If not svydesign, centering is fairly straightforward

center_vals_non_survey <- function(d, weights, facvars = NULL, fvars, pred,
                                   resp, modx, mod2, wname, offname, centered,
                                   scale, n.sd) {

  omitvars <- c(pred, resp, modx, mod2, wname, offname)
  all_omitvars <- c(resp, wname, offname)

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (!is.null(centered) && centered != "all" && centered != "none") {

    d <- gscale(x = centered, data = d, center.only = !scale,
                weights = weights, n.sd = n.sd)

    for (v in fv2) {

      if (is.factor(d[,v]) && length(unique(d[,v])) == 2 && v %nin% centered) {

        facvars <- c(facvars, v)

      }

    }

  } else if (!is.null(centered) && centered == "all") {

    # saving all vars except response
    vars <- names(d)[names(d) %nin% all_omitvars]

    d <- gscale(x = vars, data = d, center.only = !scale,
                weights = weights, n.sd = n.sd)

  } else if (!is.null(centered) && centered == "none") {

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[,v]) & length(unique(d[,v])) == 2) {

        facvars <- c(facvars, v)

      }
    }

  } else {
    # Centering the non-focal variables to make the slopes more interpretable
    vars <- names(d)[names(d) %nin% omitvars]

    d <- gscale(x = vars, data = d, center.only = !scale, weights = wname,
                n.sd = n.sd)

  }

  # Fixes a data type error with predict() later
  d <- as.data.frame(d)

  out <- list(d = d, facvars = facvars, fvars = fvars, design = NULL)

  return(out)

}

## Svydesigns get their own function to make control flow easier to follow

center_vals_survey <- function(d, weights, facvars = NULL, fvars, pred, resp,
                               modx, survey, design, mod2, wname, offname,
                               centered,
                               scale, n.sd) {

  omitvars <- c(pred, resp, modx, mod2, wname, offname)
  all_omitvars <- c(resp, wname, offname)

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (!is.null(centered) && centered != "all" && centered != "none") {

    design <- gscale(x = centered, data = design, center.only = !scale,
                     n.sd = n.sd)
    d <- design$variables

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[,v]) && length(unique(d[,v])) == 2 && v %nin% centered) {

        facvars <- c(facvars, v)

      }
    }

  } else if (!is.null(centered) && centered == "none") {

    # Dealing with two-level factors that aren't part
    # of an interaction/focal pred
    for (v in fv2) {
      if (is.factor(d[,v]) && length(unique(d[,v])) == 2) {

        facvars <- c(facvars, v)

      }
    }

  } else if (!is.null(centered) && centered == "all") {

    ndfvars <- fvars[fvars %nin% all_omitvars]

    if (length(ndfvars) > 0) {
      design <- gscale(x = ndfvars, data = design, center.only = !scale,
                       n.sd = n.sd)
      d <- design$variables
    }

  } else {
    # Center all non-focal
    nfvars <- fvars[fvars %nin% omitvars]

    if (length(nfvars) > 0) {
      design <- gscale(x = nfvars, data = design, center.only = !scale,
                       n.sd = n.sd)
      d <- design$variables
    }
  }

  out <- list(d = d, design = design, facvars = facvars, fvars = fvars)

  return(out)

}

