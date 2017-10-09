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
  if (is.null(modxvals) && is.factor(d[,modx])) {

    modxvals2 <- levels(d[,modx])
    if (is.null(modx.labels)) {

      modx.labels <- levels(d[,modx])

    }
    names(modxvals2) <- modx.labels

  } else if (is.factor(d[,modx]) &
             length(levels(d[,modx])) == 2 & sims == TRUE) {
      # We can work with a two-level factor

      names <- levels(d[,modx])
      condition <- suppressWarnings(all(is.na(as.numeric(levels(d[,modx])))))

      if (condition) {
        modxvals2 <- c(0,1)
      } else {
        modxvals2 <- sort(as.numeric(levels(d[,modx])), decreasing = FALSE)
      }

      names(modxvals2) <- names

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

  } else if (modxvals == "plus-minus") { # No mean

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
        sapply(as.character(modxvals2), FUN = function(x) {paste(modx, "=", x)})
      } else {
        names(modxvals2) <- modxvals2
      }

    }

  }

  return(modxvals2)

}



