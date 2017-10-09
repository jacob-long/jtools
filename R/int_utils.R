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
                        standardize, n.sd) {

  # Just need to pick a helper function based on survey vs no survey
  if (survey == TRUE) {

    out <- center_vals_survey(d, weights, facvars, fvars, pred, resp, modx,
                              survey, design, mod2, wname, offname, centered,
                              standardize, n.sd)

  } else {

    out <- center_vals_non_survey(d, weights, facvars, fvars, pred, resp, modx,
                                  mod2, wname, offname, centered,
                                  standardize, n.sd)

  }

  return(out)


}

## If not svydesign, centering is fairly straightforward

center_vals_non_survey <- function(d, weights, facvars = NULL, fvars, pred,
                                   resp, modx, mod2, wname, offname, centered,
                                   standardize, n.sd) {

  omitvars <- c(pred, resp, modx, mod2, wname, offname)
  all_omitvars <- c(resp, wname, offname)

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (!is.null(centered) && centered != "all" && centered != "none") {

    d <- gscale(x = centered, data = d, center.only = !standardize,
                  weights = weights, n.sd = n.sd)

    for (v in fv2) {

      if (is.factor(d[,v]) && length(unique(d[,v])) == 2 && v %nin% centered) {

        facvars <- c(facvars, v)

      }

    }

  } else if (!is.null(centered) && centered == "all") {

    # saving all vars except response
    vars <- names(d)[names(d) %nin% all_omitvars]

    d <- gscale(x = vars, data = d, center.only = !standardize,
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

    d <- gscale(x = vars, data = d, center.only = !standardize, weights = wname,
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
                               standardize, n.sd) {

  omitvars <- c(pred, resp, modx, mod2, wname, offname)
  all_omitvars <- c(resp, wname, offname)

  # Dealing with two-level factors that aren't part of an interaction
  # /focal pred
  fv2 <- fvars[fvars %nin% omitvars]

  # Handling user-requested centered vars
  if (!is.null(centered) && centered != "all" && centered != "none") {

      design <- gscale(x = centered, data = design, center.only = !standardize,
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
      design <- gscale(x = ndfvars, data = design, center.only = !standardize,
                       n.sd = n.sd)
      d <- design$variables
    }

  } else {
    # Center all non-focal
    nfvars <- fvars[fvars %nin% omitvars]

    if (length(nfvars) > 0) {
      design <- gscale(x = nfvars, data = design, center.only = !standardize,
                       n.sd = n.sd)
      d <- design$variables
    }
  }

  out <- list(d = d, design = design, facvars = facvars, fvars = fvars)

  return(out)

}
