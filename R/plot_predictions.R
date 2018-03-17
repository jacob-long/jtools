#'
#' @title Plot predicted effects from make_predictions
#'
#' @description The companion function to [make_predictions()]. This takes
#' data from [make_predictions()] (or elsewhere) and plots them like
#' [effect_plot()], [interact_plot()], and [cat_plot()]. Note that some
#' arguments will be ignored if the inputted predictions
#'
#' @param predictions Either the output from [make_predictions()] (an object
#'   of class "predictions") or a data frame of predicted values.
#'
#' @param vary.lty Should the resulting plot have different shapes for each
#'   line in addition to colors? Default is NULL, which will switch to FALSE
#'   if the `pred` is a factor and TRUE if `pred` is continuous.
#'
#' @param geom **For factor predictors only**: What type of plot should this be?
#'   There are several options
#'   here since the best way to visualize categorical interactions varies by
#'   context. Here are the options:
#'
#'   * `"dot"`: The default. Simply plot the point estimates. You may want to
#'      use
#'     `point.shape = TRUE` with this and you should also consider
#'     `interval = TRUE` to visualize uncertainty.
#'
#'   * `"line"`: This connects observations across levels of the `pred`
#'     variable with a line. This is a good option when the `pred` variable
#'     is ordinal (ordered). You may still consider `point.shape = TRUE` and
#'     `interval = TRUE` is still a good idea.
#'
#'   * `"bar"`: A bar chart. Some call this a "dynamite plot."
#'     Many applied researchers advise against this type of plot because it
#'     does not represent the distribution of the observed data or the
#'     uncertainty of the predictions very well. It is best to at least use the
#'     `interval = TRUE` argument with this geom.
#'
#'   * `"boxplot"`: This geom plots a dot and whisker plot. These can be useful
#'     for understanding the distribution of the observed data without having
#'     to plot all the observed points (especially helpful with larger data
#'     sets). **However**, it is important to note the boxplots are not based
#'     on the model whatsoever.
#'
#' @param resp What is the name of the response variable? Use a string.
#'
#' @param weights If the data are weighted, provide a vector of weights here.
#'   This is only used if `plot.points = TRUE` and `data` is not NULL.
#'
#' @param ... Ignored.
#'
#' @inheritParams interact_plot
#' @inheritParams cat_plot
#'
#' @details
#'
#' This is designed to offer more flexibility than the canned functions
#' ([effect_plot()], [interact_plot()], and [cat_plot()]), by letting you
#' generate your own predicted data and iteratively experiment with the
#' plotting options.
#'
#' Note: `predictions` objects from [make_predictions()] store information
#' about the arguments used to create the object. Unless you specify those
#' arguments manually to this function, as a convenience `plot_predictions`
#' will use the arguments stored in the `predictions` object. Those arguments
#' are:
#'
#' * `pred`, `modx`, and `mod2`
#' * `resp`
#' * `predvals`, `modxvals`, and `mod2vals`
#' * `pred.labels`, `modx.labels`, and `mod2.labels`
#' * `data`
#' * `interval`
#' * `linearity.check`
#' * `weights`
#'
#'
#' @family plotting tools
#' @export
#'
plot_predictions <- function(predictions, pred = NULL, modx = NULL, mod2 = NULL,
  resp = NULL, data = NULL, geom = c("dot","line","bar","boxplot"),
  plot.points = FALSE, interval = TRUE,
  predvals = NULL, modxvals = NULL, mod2vals = NULL, linearity.check = FALSE,
  x.label = NULL, y.label = NULL, pred.labels = NULL, modx.labels = NULL,
  mod2.labels = NULL, main.title = NULL, legend.main = NULL, color.class = NULL,
  line.thickness = 1.1, vary.lty = NULL, jitter = 0.1, weights = NULL,
  rug = FALSE, rug_sides = "b", ...) {

  # Capture user-specified arguments
  # I'm more interested in the names than the actual content
  args <- as.list(match.call())
  args <- args[-1]

  if (class(predictions) == "predictions") {

    pred_obj <- predictions
    predictions <- pred_obj$predicted
    args$predictions <- predictions
    data <- pred_obj$original
    args$data <- data

    atts <- attributes(pred_obj)
    atts <- atts[names(atts) %nin% c("class","names")]

    # Renaming args that need to be different in this function
    # It ain't elegant and I should probability fix the ones feeding these
    # incorrect names
    if ("modxvals2" %in% names(atts)) {
      atts$modxvals <- atts$modxvals2
      atts <- atts[names(atts) %nin% "modxvals2"]
    }
    if ("mod2vals2" %in% names(atts)) {
      atts$mod2vals <- atts$mod2vals2
      atts <- atts[names(atts) %nin% "mod2vals2"]
    }
    if ("weights" %in% names(atts)) {
      atts$wts <- atts$weights
      atts <- atts[names(atts) %nin% "weights"]
    }

    for (n in names(atts)) {
      # This conditional prevents overwriting of user-specified args
      if (n %nin% names(args)) {
        assign(n, atts[[n]])
      }
    }

  }

  # Renaming these objects for compatibility with the plotting functions
  modxvals2 <- modxvals
  mod2vals2 <- mod2vals

  if (is.factor(predictions[[pred]]) | is.character(predictions[[pred]])) {

    if (is.null(vary.lty)) {vary.lty <- FALSE}

    the_args <- formals("plot_cat")
    for (n in names(the_args)) {

      if (exists(n)) {
        the_args[[n]] <- get(n)
      }

    }

    the_args <- as.list(the_args)
    do.call("plot_cat", the_args)

  } else {

    if (is.null(vary.lty)) {vary.lty <- TRUE}

    if (is.null(modx)) { # effect_plot

      the_args <- formals("plot_effect_continuous")
      for (n in names(the_args)) {

        if (exists(n)) {
          the_args[[n]] <- get(n)
        }

      }

      the_args <- as.list(the_args)
      do.call("plot_effect_continuous", the_args)

    } else { # interact_plot

      the_args <- formals("plot_mod_continuous")
      for (n in names(the_args)) {

        if (exists(n)) {
          the_args[[n]] <- get(n)
        }

      }

      the_args <- as.list(the_args)
      do.call("plot_mod_continuous", the_args)

    }


  }


}


plot_mod_continuous <- function(predictions, pred, modx, resp, mod2 = NULL,
  data = NULL, plot.points = FALSE, interval = FALSE, linearity.check = FALSE,
  x.label = NULL, y.label = NULL, pred.labels = NULL, modx.labels = NULL,
  mod2.labels = NULL, main.title = NULL, legend.main = NULL, color.class = NULL,
  line.thickness = 1.1, vary.lty = TRUE, jitter = 0.1, modxvals2 = NULL,
  mod2vals2 = NULL, wts = NULL, rug = FALSE, rug_sides = "b") {

  d <- data
  pm <- predictions

  # Setting default for colors
  if (is.factor(d[[modx]])) {
    facmod <- TRUE
    if (is.null(color.class)) {
      color.class <- "Set2"
    }
  } else {
    facmod <- FALSE
    if (is.null(color.class)) {
      color.class <- "Blues"
    }
  }

  # If only 1 jitter arg, just duplicate it
  if (length(jitter) == 1) {jitter <- rep(jitter, 2)}

  # If no user-supplied legend title, set it to name of moderator
  if (is.null(legend.main)) {
    legend.main <- modx
  }

  if (is.null(x.label)) {
    x.label <- pred
  }

  if (is.null(y.label)) {
    y.label <- resp
  }

  if (is.null(modxvals2)) {
    modxvals2 <- unique(pm[[modx]])
  }

  if (!is.null(mod2) && is.null(mod2vals2)) {
    mod2vals2 <- unique(pm[[mod2]])
  }

  # Get palette from RColorBrewer myself so I can use darker values
  if (length(color.class) == 1) {
    if (facmod == FALSE) {
      colors <- RColorBrewer::brewer.pal((length(modxvals2) + 1), color.class)
      colors <- colors[-1]
    } else {
      if (length(modxvals2) == 2) {
        num_colors <- 3
      } else {
        num_colors <- length(modxvals2)
      }
      colors <- RColorBrewer::brewer.pal(num_colors, color.class)
      colors <- colors[seq_along(modxvals2)]
    }
  } else { # Allow manually defined colors
    colors <- color.class
    if (length(colors) != length(modxvals2)) {
      stop("Manually defined colors must be of same length as modxvals.")
    }
  }

  # Manually set linetypes
  types <- c("solid", "4242", "2222", "dotdash", "dotted", "twodash")
  ltypes <- types[seq_along(modxvals2)]

  if (is.null(mod2)) {
    colors <- rev(colors)
    pp_color <- first(colors) # Darkest color used for plotting points
  } else {
    ltypes <- rev(ltypes)
    pp_color <- last(colors)
  }

  names(colors) <- modx.labels
  names(ltypes) <- modx.labels

  # Defining linetype here
  if (vary.lty == TRUE) {
    p <- ggplot(pm, aes_string(x = pred, y = resp, colour = modx,
                               group = modx, linetype = modx))
  } else {
    p <- ggplot(pm, aes_string(x = pred, y = resp, colour = modx,
                               group = modx))
  }

  p <- p + geom_path(size = line.thickness)

  # Plot intervals if requested
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm, aes_string(x = pred,
                                               ymin = "ymin", ymax = "ymax",
                                               fill = modx, group = modx,
                                               colour = modx, linetype = NA),
                         alpha = 1/5, show.legend = FALSE,
                         inherit.aes = FALSE)

    p <- p + scale_fill_manual(values = colors, breaks = names(colors))
  }

  # If third mod, facet by third mod
  if (!is.null(mod2)) {
    facets <- facet_grid(paste(". ~ mod2_group"))
    p <- p + facets
  } else if (linearity.check == TRUE) {
    facets <- facet_grid(paste(". ~ modx_group"))
    modxgroup <- "modx_group"
    p <- p + facets + stat_smooth(data = d, aes_string(x = pred, y = resp,
                                                       group = modxgroup),
                                  method = "loess", size = 1,
                                  show.legend = FALSE, inherit.aes = FALSE,
                                  se = FALSE, span = 2, geom = "line",
                                  alpha = 0.5, color = "black")
  }

  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts)/sum(wts) # scaling constant
    const <- const * 2 # make the range of values larger
    wts <- const * wts
    # Append weights to data
    d[["the_weights"]] <- wts

    if (is.factor(d[[modx]])) {
      p <- p + geom_point(data = d, aes_string(x = pred, y = resp,
                                               colour = modx,
                                               size = "the_weights"),
                          position = position_jitter(width = jitter[1],
                                                     height = jitter[2]),
                          inherit.aes = TRUE, show.legend = FALSE)
    } else if (!is.factor(d[[modx]])) {
      # using alpha for same effect with continuous vars
      p <- p + geom_point(data = d,
                          aes_string(x = pred, y = resp, alpha = modx,
                                     size = "the_weights"),
                          colour = pp_color, inherit.aes = FALSE,
                          position = position_jitter(width = jitter[1],
                                                     height = jitter[2]),
                          show.legend = FALSE) +
        scale_alpha_continuous(range = c(0.25, 1), guide = "none")
    }

    # Add size aesthetic to avoid giant points
    p <- p + scale_size_identity()

  }

  # Rug plot for marginal distributions
  if (rug == TRUE) {
    if (is.factor(d[[modx]])) {
      p <- p + geom_rug(data = d,
                        aes_string(x = pred, y = resp, colour = modx),
                        alpha = 0.6,
                        position = position_jitter(width = jitter[1],
                                                   height = jitter[2]),
                        sides = rug_sides)
    } else {
      p <- p + geom_rug(data = d,
                        aes_string(x = pred, y = resp),
                        alpha = 0.6,
                        position = position_jitter(width = jitter[1],
                                                   height = jitter[2]),
                        sides = rug_sides, inherit.aes = FALSE)
    }
  }

  # Using theme_apa for theming...but using legend title and side positioning
  if (is.null(mod2)) {
    p <- p + theme_apa(legend.pos = "right", legend.use.title = TRUE,
                       legend.font.size = 11)
  } else {
    # make better use of space by putting legend on bottom for facet plots
    p <- p + theme_apa(legend.pos = "bottom", legend.use.title = TRUE,
                       facet.title.size = 10, legend.font.size = 11)
  }
  p <- p + labs(x = x.label, y = y.label) # better labels for axes

  # Getting rid of tick marks for factor predictor
  if (length(unique(d[[pred]])) == 2) {
    # Predictor has only two unique values
    # Make sure those values are in increasing order
    brks <- sort(unique(d[[pred]]), decreasing = F)
    if (is.null(pred.labels)) {
      p <- p + scale_x_continuous(breaks = brks)
    } else {
      if (length(pred.labels) == 2) { # Make sure pred.labels has right length
        p <- p + scale_x_continuous(breaks = brks, labels = pred.labels)
      } else {
        warning("pred.labels argument has the wrong length. It won't be used")
        p <- p + scale_x_continuous(breaks = brks)
      }
    }
  }

  # Get scale colors, provide better legend title
  p <- p + scale_colour_manual(name = legend.main, values = colors,
                               breaks = names(colors))

  if (vary.lty == TRUE) { # Add line-specific changes
    p <- p + scale_linetype_manual(name = legend.main, values = ltypes,
                                   breaks = names(ltypes),
                                   na.value = "blank")
    # Need some extra width to show the linetype pattern fully
    p <- p + theme(legend.key.width = grid::unit(2, "lines"))
  }

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }

  return(p)

}


plot_effect_continuous <- function(predictions, pred, plot.points = FALSE,
  interval = FALSE, data = NULL, x.label = NULL, y.label = NULL,
  pred.labels = NULL, main.title = NULL, color.class = NULL,
  line.thickness = 1.1, jitter = 0.1, resp = NULL, wts = NULL,
  rug = FALSE, rug_sides = "b") {

  pm <- predictions
  d <- data

  if (is.null(x.label)) {
    x.label <- pred
  }

  if (is.null(y.label)) {
    y.label <- resp
  }

  # If only 1 jitter arg, just duplicate it
  if (length(jitter) == 1) {jitter <- rep(jitter, 2)}

  # Starting plot object
  p <- ggplot(pm, aes_string(x = pred, y = resp))

  # Define line thickness
  p <- p + geom_path(size = line.thickness)

  # Plot intervals if requested
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm, aes_string(ymin = "ymin",
                                               ymax = "ymax"),
                         alpha = 1/5, show.legend = FALSE)
  }

  # Plot observed data
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts)/sum(wts) # scaling constant
    const <- const * 2 # make the range of values larger
    wts <- const * wts
    # Append weights to data
    d[["the_weights"]] <- wts
    p <- p + geom_point(data = d,
                        aes_string(x = pred, y = resp, size = "the_weights"),
                        position = position_jitter(width = jitter[1],
                                                   height = jitter[2]),
                        inherit.aes = FALSE, show.legend = FALSE)
    # Add size aesthetic to avoid giant points
    # p <- p + scale_size(range = c(0.3, 4))
    p <- p + scale_size_identity()

  }

  # Rug plot for marginal distributions
  if (rug == TRUE) {
    p <- p + geom_rug(data = d,
                      mapping = aes_string(x = pred, y = resp), alpha = 0.6,
                      position = position_jitter(width = jitter[1]),
                      sides = rug_sides, inherit.aes = FALSE)
  }

  # Using theme_apa for theming...but using legend title and side positioning
  p <- p + theme_apa(legend.pos = "right", legend.use.title = TRUE)

  p <- p + labs(x = x.label, y = y.label) # better labels for axes

  # Getting rid of tick marks for two-level predictor
  if (length(unique(d[[pred]])) == 2) { # Predictor has only two unique values
    # Make sure those values are in increasing order
    brks <- sort(unique(d[[pred]]), decreasing = F)
    if (is.null(pred.labels)) {
      p <- p + scale_x_continuous(breaks = brks)
    } else {
      if (length(pred.labels) == 2) { # Make sure pred.labels has right length
        p <- p + scale_x_continuous(breaks = brks, labels = pred.labels)
      } else {
        warning("pred.labels argument has the wrong length. It won't be used")
        p <- p + scale_x_continuous(breaks = brks)
      }
    }
  }

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }

  # Return the plot
  return(p)


}


plot_cat <- function(predictions, pred, modx = NULL, mod2 = NULL,
   data = NULL, geom = c("dot","line","bar","boxplot"), predvals = NULL,
   modxvals = NULL, mod2vals = NULL, interval = TRUE, plot.points = FALSE,
   point.shape = FALSE, vary.lty = FALSE,  pred.labels = NULL,
   modx.labels = NULL, mod2.labels = NULL, x.label = NULL, y.label = NULL,
   main.title = NULL, legend.main = NULL, color.class = "Set2", wts = NULL,
   resp = NULL, jitter = 0.1) {

  pm <- predictions
  d <- data

  geom <- geom[1]

  # If only 1 jitter arg, just duplicate it
  if (length(jitter) == 1) {jitter <- rep(jitter, 2)}

  # If no user-supplied legend title, set it to name of moderator
  if (is.null(legend.main)) {
    legend.main <- modx
  }

  if (is.null(x.label)) {
    x.label <- pred
  }

  if (is.null(y.label)) {
    y.label <- resp
  }

  # Sequential palettes get different treatment
  sequentials <-
    c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
      "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
      "YlOrBr", "YlOrRd")

  # Checking if user provided the colors his/herself
  if (length(color.class) == 1 |
      length(color.class) != length(levels(d[[pred]]))) {
    # Get palette from RColorBrewer myself so I can use darker values
    if (color.class %in% sequentials) {
      colors <- RColorBrewer::brewer.pal((length(unique(d[[pred]])) + 1),
                                         color.class)
      colors <- rev(colors)[-1]
    } else {
      suppressWarnings(colors <-
                         RColorBrewer::brewer.pal(length(unique(d[[pred]])),
                                                  color.class))
    }
  }

  names(colors) <- levels(d[[pred]])

  a_level <- 1
  if (plot.points == TRUE) {
    if (!is.null(modx)) {
      a_level <- 0
    } else {
      a_level <- 0.5
    }
  } else if (interval == TRUE) {
    a_level <- 0.5
  }

  if (!is.null(modx)) {
    if (point.shape == FALSE & vary.lty == FALSE) {
      p <- ggplot(pm, aes_string(x = pred, y = resp, group = modx,
                                 colour = modx, fill = modx))
    } else if (point.shape == TRUE & vary.lty == FALSE) {
      p <- ggplot(pm, aes_string(x = pred, y = resp, group = modx,
                                 colour = modx, fill = modx,
                                 shape = modx))
    } else if (point.shape == FALSE & vary.lty == TRUE) {
      p <- ggplot(pm, aes_string(x = pred, y = resp, group = modx,
                                 colour = modx, fill = modx,
                                 linetype = modx))
    } else if (point.shape == TRUE & vary.lty == TRUE) {
      p <- ggplot(pm, aes_string(x = pred, y = resp, group = modx,
                                 colour = modx, fill = modx,
                                 shape = modx, linetype = modx))
    }
  } else {
    p <- ggplot(pm, aes_string(x = pred, y = resp, group = 1))
  }

  if (geom == "bar") {
    p <- p + geom_bar(stat = "identity", position = "dodge", alpha = a_level)
  } else if (geom == "boxplot") {
    if (!is.null(modx)) {
      p <- ggplot(d, aes_string(x = pred, y = resp,
                                colour = modx)) +
        geom_boxplot(position = position_dodge(0.9))
    } else {
      p <- ggplot(d, aes_string(x = pred, y = resp)) +
        geom_boxplot(position = position_dodge(0.9))
    }
  } else if (geom == "line") {
    p <- p + geom_path() + geom_point(size = 4)
  } else if (geom == "dot") {
    p <- p + geom_point(size = 3, position = position_dodge(0.9))
  }

  # Plot intervals if requested
  if (interval == TRUE & geom %in% c("bar", "dot")) {

    p <- p + geom_errorbar(aes_string(ymin = "ymin", ymax = "ymax"),
                           alpha = 1, show.legend = FALSE,
                           position = position_dodge(0.9), width = 0.8)

  } else if (interval == TRUE & geom %in% c("line")) {

    p <- p + geom_errorbar(aes_string(ymin = "ymin", ymax = "ymax"),
                           alpha = 0.8, show.legend = FALSE, width = 0.5)

  }

  # If third mod, facet by third mod
  if (!is.null(mod2)) {
    facets <- facet_grid(paste(". ~", mod2))
    p <- p + facets
  }

  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts)/sum(wts) # scaling constant
    const <- const * 2 # make the range of values larger
    wts <- const * wts
    # Append weights to data
    d[,"the_weights"] <- wts

    if (point.shape == TRUE & !is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred, y = resp,
                                               colour = modx,
                                               size = "the_weights",
                                               shape = modx),
                          position = position_jitterdodge(dodge.width = 0.9,
                                                    jitter.width = jitter[1],
                                                    jitter.height = jitter[2]),
                          inherit.aes = FALSE,
                          show.legend = FALSE,
                          alpha = 0.6)
    } else if (point.shape == FALSE & !is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred, y = resp,
                                               colour = modx,
                                               size = "the_weights"),
                          position = position_jitterdodge(dodge.width = 0.9,
                                                    jitter.width = jitter[1],
                                                    jitter.height = jitter[2]),
                          inherit.aes = FALSE,
                          show.legend = FALSE,
                          alpha = 0.6)
    } else if (point.shape == TRUE & is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred, y = resp,
                                               colour = pred,
                                               size = "the_weights",
                                               shape = modx),
                          position = position_jitterdodge(dodge.width = 0.9,
                                                    jitter.width = jitter[1],
                                                    jitter.height = jitter[2]),
                          inherit.aes = FALSE,
                          show.legend = FALSE,
                          alpha = 0.6)
    } else if (point.shape == FALSE & is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred, y = resp,
                                               size = "the_weights"),
                          position = position_jitterdodge(dodge.width = 0.9,
                                                    jitter.width = jitter[1],
                                                    jitter.height = jitter[2]),
                          inherit.aes = FALSE,
                          show.legend = FALSE,
                          alpha = 0.6)
    }


    # Add size aesthetic to avoid giant points
    p <- p + scale_size_identity()

  }

  # Using theme_apa for theming...but using legend title and side positioning
  if (is.null(mod2)) {
    p <- p + theme_apa(legend.pos = "right", legend.use.title = TRUE)
  } else {
    # make better use of space by putting legend on bottom for facet plots
    p <- p + theme_apa(legend.pos = "bottom", legend.use.title = TRUE,
                       facet.title.size = 10)
  }
  p <- p + labs(x = x.label, y = y.label) # better labels for axes

  # Get scale colors, provide better legend title
  p <- p + scale_colour_brewer(name = legend.main, palette = color.class)
  p <- p + scale_fill_brewer(name = legend.main, palette = color.class)

  # Need some extra width to show the linetype pattern fully
  p <- p + theme(legend.key.width = grid::unit(2, "lines"))

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }

  # Return the plot
  return(p)


}
