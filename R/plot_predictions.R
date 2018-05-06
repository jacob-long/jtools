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
#'   * `"point"`: The default. Simply plot the point estimates. You may want to
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
  resp = NULL, data = NULL, geom = c("point", "line", "bar", "boxplot"),
  plot.points = FALSE, interval = TRUE,
  predvals = NULL, modxvals = NULL, mod2vals = NULL, linearity.check = FALSE,
  facet.modx = FALSE, x.label = NULL, y.label = NULL, pred.labels = NULL,
  modx.labels = NULL, mod2.labels = NULL, main.title = NULL, legend.main = NULL,
  color.class = NULL, line.thickness = 1.1, vary.lty = NULL, jitter = 0.1,
  weights = NULL, rug = FALSE, rug.sides = "b", force.cat = FALSE,
  point.shape = FALSE, geom.alpha = NULL, dodge.width = NULL,
  errorbar.width = NULL, interval.geom = c("errorbar", "linerange"),
  pred.point.size = 3.5, point.size = 1, ...) {

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

  if (is.factor(predictions[[pred]]) | is.character(predictions[[pred]]) |
      force.cat == TRUE) {

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
  mod2vals2 = NULL, wts = NULL, rug = FALSE, rug.sides = "b",
  point.shape = FALSE, point.size = 1, facet.modx = FALSE) {

  d <- data
  pm <- predictions

  # Setting default for colors
  if (is.null(color.class) && (facet.modx == TRUE | linearity.check == TRUE)) {
    color.class <- rep("black", times = length(modxvals2))
    vary.lty <- FALSE
    point.shape <- FALSE
  }
  if (is.factor(d[[modx]])) {
    facmod <- TRUE
    if (is.null(color.class)) {
      color.class <- "CUD Bright"
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

  # Get manually defined colors from whichever source requested
  if (length(color.class) == 1 | length(color.class) >= length(modxvals2)) {
    colors <- suppressWarnings(get_colors(color.class, length(modxvals2)))
  } else { # Allow manually defined colors
    stop("Manually defined colors must be of same length as modxvals.")
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

  # Deal with non-syntactic names
  if (make.names(pred) !=  pred) {
    pred_g <- paste0("`", pred, "`")
  } else {
    pred_g <- pred
  }
  if (make.names(modx) !=  modx) {
    modx_g <- paste0("`", modx, "`")
  } else {
    modx_g <- modx
  }
  if (!is.null(mod2) && make.names(mod2) !=  mod2) {
    mod2_g <- paste0("`", mod2, "`")
  } else if (!is.null(mod2)) {
    mod2_g <- mod2
  }
  if (make.names(resp) !=  resp) {
    resp_g <- paste0("`", resp, "`")
  } else {
    resp_g <- resp
  }

  # Defining linetype here
  if (vary.lty == TRUE) {
    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, colour = modx_g,
                               group = modx_g, linetype = modx_g))
  } else {
    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, colour = modx_g,
                               group = modx_g))
  }

  p <- p + geom_path(size = line.thickness, show.legend = !facet.modx)

  # Plot intervals if requested
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm, aes_string(x = pred_g,
                                               ymin = "ymin", ymax = "ymax",
                                               fill = modx_g, group = modx_g,
                                               colour = modx_g, linetype = NA),
                         alpha = 1/5, show.legend = FALSE,
                         inherit.aes = FALSE)

    p <- p + scale_fill_manual(values = colors, breaks = names(colors))
  }

  # If third mod, facet by third mod
  facet_form <- "~"
  modgroup <- NULL
  if (!is.null(mod2) || linearity.check == TRUE || facet.modx == TRUE) {
    do_facets <- TRUE
    # p <- p + facets
  } else {do_facets <- FALSE}

  if (linearity.check == TRUE | facet.modx == TRUE) {
    facet_form <- paste(facet_form, "modx_group")
    modgroup <- "modx_group"
  }

  if (!is.null(mod2)) {
    facet_form <- paste(facet_form,
                        ifelse(facet_form == "~", yes = "mod2_group",
                               no = "+ mod2_group"))
    if (!is.null(modgroup)) {
      modgroup <- "modgroup"
    } else {
      modgroup <- "mod2group"
    }
  }

  if (do_facets == TRUE) {
    if (!is.null(mod2) & (linearity.check == TRUE | facet.modx == TRUE)) {
      num_unique <- nrow(unique(pm[c("modx_group", "mod2_group")]))
      if (num_unique %in% c(3, 6, 9)) {
        # 1 x 3, 2 x 3, or (most commonly) 3 x 3
        num_cols <- 3
      } else if (num_unique %in% c(4)) {
        # 2 x 2
        num_cols <- 2
      } else { # let ggplot2 decide
        num_cols <- NULL
      }
    } else {num_cols <- NULL}
    p <- p + facet_wrap(as.formula(facet_form), ncol = num_cols)
  }

  if (linearity.check == TRUE) {
    p <- p + stat_smooth(data = d,
                        aes_string(x = pred_g, y = resp_g,
                                   group = "modx_group"),
                        method = "loess", size = 1,
                        show.legend = FALSE, inherit.aes = FALSE,
                        se = FALSE, span = 2, geom = "line",
                        alpha = 0.6, color = "red")
  }

  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts)/sum(wts) # scaling constant
    # make the range of values larger, but only if there are non-1 weights
    const <- const * ((1 * !all(wts == 1)) + point.size)
    wts <- const * wts
    # Append weights to data
    d[["the_weights"]] <- wts

    if (is.factor(d[[modx]])) {
      # Create shape aesthetic argument
      shape_arg <- if (point.shape == TRUE) {modx_g} else {NULL}
      shape_guide <- if (point.shape == TRUE) {TRUE} else {FALSE}

      p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                               colour = modx_g,
                                               size = "the_weights",
                                               shape = shape_arg),
                          position = position_jitter(width = jitter[1],
                                                     height = jitter[2]),
                          inherit.aes = TRUE, show.legend = shape_guide,
                          size = point.size) +
        scale_shape_discrete(name = legend.main, breaks = names(colors),
                             na.value = "blank")
    } else if (!is.factor(d[[modx]])) {
      # using alpha for same effect with continuous vars
      # set alpha argument dependent on whether this is a plot facetted by
      # the moderator
      alpha_arg <- ifelse(facet.modx, yes = c(1, 1), no = c(0.25, 1))
      p <- p + geom_point(data = d,
                          aes_string(x = pred_g, y = resp_g, alpha = modx_g,
                                     size = "the_weights"),
                          colour = pp_color, inherit.aes = FALSE,
                          position = position_jitter(width = jitter[1],
                                                     height = jitter[2]),
                          show.legend = FALSE) +
        scale_alpha_continuous(range = alpha_arg, guide = "none")
    }

    # Add size aesthetic to avoid giant points
    p <- p + scale_size_continuous(range = c(0.5, 5), guide = "none")

  }

  # Rug plot for marginal distributions
  if (rug == TRUE) {
    if (is.factor(d[[modx]])) {
      p <- p + geom_rug(data = d,
                        aes_string(x = pred_g, y = resp_g, colour = modx_g),
                        alpha = 0.6,
                        position = position_jitter(width = jitter[1],
                                                   height = jitter[2]),
                        sides = rug.sides)
    } else {
      p <- p + geom_rug(data = d,
                        aes_string(x = pred_g, y = resp_g),
                        alpha = 0.6,
                        position = position_jitter(width = jitter[1],
                                                   height = jitter[2]),
                        sides = rug.sides, inherit.aes = FALSE)
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
  rug = FALSE, rug.sides = "b", point.size = 1) {

  pm <- predictions
  d <- data

  if (is.null(x.label)) {
    x.label <- pred
  }

  if (is.null(y.label)) {
    y.label <- resp
  }

  # Deal with non-syntactic names
  if (make.names(pred) !=  pred) {
    pred_g <- paste0("`", pred, "`")
  } else {
    pred_g <- pred
  }
  if (make.names(resp) !=  resp) {
    resp_g <- paste0("`", resp, "`")
  } else {
    resp_g <- resp
  }

  # If only 1 jitter arg, just duplicate it
  if (length(jitter) == 1) {jitter <- rep(jitter, 2)}

  # Starting plot object
  p <- ggplot(pm, aes_string(x = pred_g, y = resp_g))

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
    # make the range of values larger, but only if there are non-1 weights
    const <- const * (1 * all(wts == 1) + point.size)
    wts <- const * wts
    # Append weights to data
    d[["the_weights"]] <- wts
    p <- p + geom_point(data = d,
                        aes_string(x = pred_g, y = resp_g,
                         size = "the_weights"),
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
                      mapping = aes_string(x = pred_g, y = resp_g), alpha = 0.6,
                      position = position_jitter(width = jitter[1]),
                      sides = rug.sides, inherit.aes = FALSE)
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
   data = NULL, geom = c("point", "line", "bar", "boxplot"), predvals = NULL,
   modxvals = NULL, mod2vals = NULL, interval = TRUE, plot.points = FALSE,
   point.shape = FALSE, vary.lty = FALSE,  pred.labels = NULL,
   modx.labels = NULL, mod2.labels = NULL, x.label = NULL, y.label = NULL,
   main.title = NULL, legend.main = NULL, color.class = "CUD Bright",
   wts = NULL, resp = NULL, jitter = 0.1, geom.alpha = NULL, dodge.width = NULL,
   errorbar.width = NULL, interval.geom = c("errorbar", "linerange"),
   line.thickness = 1.1, point.size = 1, pred.point.size = 3.5) {

  pm <- predictions
  d <- data

  geom <- geom[1]
  if (geom == "dot") {geom <- "point"}

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

  # Deal with non-syntactic names
  if (make.names(pred) !=  pred) {
    pred_g <- paste0("`", pred, "`")
  } else {
    pred_g <- pred
  }
  if (!is.null(modx) && make.names(modx) !=  modx) {
    modx_g <- paste0("`", modx, "`")
  } else if (!is.null(modx)) {
    modx_g <- modx
  }
  if (!is.null(mod2) && make.names(mod2) !=  mod2) {
    mod2_g <- paste0("`", mod2, "`")
  } else if (!is.null(mod2)) {
    mod2_g <- mod2
  }
  if (make.names(resp) !=  resp) {
    resp_g <- paste0("`", resp, "`")
  } else {
    resp_g <- resp
  }

  # Deal with numeric predictors coerced into factors
  if (is.numeric(pm[[pred]])) {
    pred.levels <- if (!is.null(predvals)) {predvals} else {
      unique(pm[[pred]])
    }
    pred.labels <- if (!is.null(pred.labels)) {pred.labels} else {
      unique(pm[[pred]])
    }
    pm[[pred]] <- factor(pm[[pred]], levels = pred.levels,
                         labels = pred.labels)

    # Make sure only observations of requested levels of predictor are included
    d <- d[d[[pred]] %in% pred.levels,]
    d[[pred]] <- factor(d[[pred]], levels = pred.levels, labels = pred.labels)
  }

  # Checking if user provided the colors his/herself
  if (length(color.class) == 1 | length(color.class) != length(modx.labels)) {
    colors <- suppressWarnings(get_colors(color.class, length(modx.labels)))
  } else {
    colors <- color.class
  }

  # Manually set linetypes
  types <- c("solid", "4242", "2222", "dotdash", "dotted", "twodash")
  ltypes <- types[seq_along(modx.labels)]

  names(ltypes) <- modx.labels
  names(colors) <- modx.labels

  if (is.null(geom.alpha)) {
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
  } else {a_level <- geom.alpha}

  if (is.null(dodge.width)) {
    dodge.width <- if (geom %in% c("bar", "point", "boxplot")) {0.9} else {0}
  }
  if (is.null(errorbar.width)) {
    errorbar.width <- if (geom %in% c("bar", "point")) {0.9} else {0.5}
  }

  if (!is.null(modx)) {
    shape_arg <- if (point.shape == TRUE) {modx_g} else {NULL}
    lty_arg <- if (vary.lty == TRUE) {modx_g} else {NULL}

    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, group = modx_g,
                                 colour = modx_g, fill = modx_g,
                                 shape = shape_arg, linetype = lty_arg))
  } else {
    p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, group = 1))
  }

  if (geom == "bar") {
    p <- p + geom_bar(stat = "identity", position = "dodge", alpha = a_level)
  } else if (geom == "boxplot") {
    if (!is.null(modx)) {
      p <- ggplot(d, aes_string(x = pred_g, y = resp_g,
                                colour = modx_g)) +
        geom_boxplot(position = position_dodge(dodge.width))
    } else {
      p <- ggplot(d, aes_string(x = pred_g, y = resp_g)) +
        geom_boxplot(position = position_dodge(dodge.width))
    }
  } else if (geom %in% c("point", "line")) {
    p <- p + geom_point(size = pred.point.size,
                        position = position_dodge(dodge.width))
  }

  if (geom == "line") {
    p <- p + geom_path(position = position_dodge(dodge.width),
                       size = line.thickness)
  }

  # Plot intervals if requested
  if (interval == TRUE && geom != "boxplot" && interval.geom == "errorbar") {
    p <- p + geom_errorbar(aes_string(ymin = "ymin", ymax = "ymax"),
                           alpha = 1, show.legend = FALSE,
                           position = position_dodge(dodge.width),
                           width = errorbar.width,
                           size = line.thickness)
  } else if (interval == TRUE && geom != "boxplot" && interval.geom %in%
                                                    c("line", "linerange")) {
    p <- p + geom_linerange(aes_string(ymin = "ymin", ymax = "ymax"),
                           alpha = 0.8, show.legend = FALSE,
                           position = position_dodge(dodge.width),
                           size = line.thickness)
  }

  # If third mod, facet by third mod
  if (!is.null(mod2)) {
    facets <- facet_grid(paste(". ~", mod2_g))
    p <- p + facets
  }

  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts) / sum(wts) # scaling constant
    # make the range of values larger, but only if there are non-1 weights
    const <- const * (1 * all(wts == 1) + point.size)
    wts <- const * wts
    # Append weights to data
    d[,"the_weights"] <- wts

    if (!is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                               colour = modx_g,
                                               size = "the_weights",
                                               shape = shape_arg),
                          position =
                            position_jitterdodge(dodge.width = dodge.width,
                                                 jitter.width = jitter[1],
                                                 jitter.height = jitter[2]),
                          inherit.aes = FALSE,
                          show.legend = FALSE,
                          alpha = 0.6)
    } else if (is.null(modx)) {
      p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                               size = "the_weights",
                                               shape = pred_g),
                          position =
                            position_jitterdodge(dodge.width = dodge.width,
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
  p <- p + scale_colour_manual(name = legend.main,
                               values = colors,
                               breaks = names(colors))
  p <- p + scale_fill_manual(name = legend.main,
                             values = colors,
                             breaks = names(colors))
  p <- p + scale_shape(name = legend.main)

  if (vary.lty == TRUE) { # Add line-specific changes
    p <- p + scale_linetype_manual(name = legend.main, values = ltypes,
                                   breaks = names(ltypes),
                                   na.value = "blank")
    # Need some extra width to show the linetype pattern fully
    p <- p + theme(legend.key.width = grid::unit(3, "lines"))
  }

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }

  # Return the plot
  return(p)


}
