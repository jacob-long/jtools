#' Plot varying effects in multilevel regression models
#'
#' \code{plot_varying_effects()} plots regression paths for different levels of
#'  grouping variables in multilevel models.
#'  
#' @param model A regression model. The function is tested with
#'    \code{\link[lme4]{merMod}}, \code{\link[brms]{brmsfit}},
#'    \code{stanreg} models.
#'   Models from other classes may work as well but are not officially
#'   supported. The model should include the interaction of interest.
#'   
#' @inheritParams effect_plot
#' @inheritParams summ.lm
#' @inheritParams make_predictions
#'
#' @details 
#'   This function provides output comparable to [effect_plot()], but with the
#'   added ability to plot effects that vary by group in multilevel regression 
#'   models. If you do not want to visualize multiple groups, then you should
#'   use [effect_plot()].
#'
#' @return The functions returns a \code{ggplot} object, which can be treated
#'   like a user-created plot and expanded upon as such.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @seealso [effect_plot()]
#'
#' @examples
#' # With lme4
#' \dontrun{
#' library(lme4)
#' data(VerbAgg)
#' mv <- glmer(r2 ~ Anger + mode + (1 | item), data = VerbAgg,
#'             family = binomial,
#'             control = glmerControl("bobyqa"))
#' plot_varying_effects(mv, pred = Anger, )
#' }
#'
#' @noRd

plot_varying_effects <- function(model, pred, group_var = NULL,
  group_levels = NULL, pred.values = NULL, centered = "all",
  plot.points = FALSE, interval = FALSE, data = NULL, at = NULL,
  facet = FALSE, vary.lty = FALSE, re.form = NULL,
  int.type = c("confidence","prediction"), int.width = .95, 
  outcome.scale = "response",  set.offset = 1, x.label = NULL, y.label = NULL,
  pred.labels = NULL, main.title = NULL, colors = NULL,
  line.thickness = 1.1,  point.size = 1.5, point.alpha = 0.6,
  point.color = "black", jitter = 0, rug = FALSE, rug.sides = "lb",
  force.cat = FALSE, cat.geom = c("point", "line", "bar"), 
  cat.interval.geom = c("errorbar", "linerange"), cat.pred.point.size = 3.5,  
  partial.residuals = FALSE, ...) {
  
  # Evaluate the pred and group_var args
  pred <- quo_name(enexpr(pred))
  group_var <- quo_name(enexpr(group_var))
  if (group_var == "NULL") {
    group_var <- names(ranef(model))[1]
    msg_wrap("group_var unspecified, using ", group_var)
  }
  
  # Have a sensible interval default for categorical predictors
  if ("interval" %nin% names(match.call())[-1] &
      !(is.numeric(get_data(model, warn = FALSE)[[pred]]) &
        force.cat == FALSE)) {
    interval <- TRUE
  }
  
  if (force.cat == TRUE & is.null(pred.values)) {
    if (is.null(data)) {data <- get_data(model)}
    pred.values <- sort(unique(suppressMessages(data[[pred]])))
  }
  
  if (!is.null(group_levels)) {
    if (is.null(at)) {
      at <- list()
    }
    at[[group_var]] <- group_levels
    if (is.null(colors)) {
      if (length(group_levels) < 8) {
        colors <- "CUD Bright"
      } else {
        colors <- "Set3"
      }
    }
  } else {
    d <- get_data(model, warn = FALSE)
    if (is.null(group_levels)) {
      n_levels <- length(unique(d[[group_var]]))
      if (n_levels > 7) {
        if (n_levels > 7 & is.null(colors)) {colors <- "Set3"}
        if (n_levels > 10) {
          group_levels <- sample(unique(d[[group_var]]), 10,
                                 prob = prop.table(table(d[[group_var]])))
          msg_wrap("Because of the large number of groups, 10 were randomly 
               selected. Specify them manually using group_levels if you
               would like to include more or a different set of them.")
        }
      } else {
        if (is.null(colors)) {colors <- "CUD Bright"}
        group_levels <- unique(d[[group_var]])
      }
    } else {
      n_levels <- length(group_levels)
      if (is.null(colors)) {
        if (n_levels > 7) {colors <- "Set3"} else {"CUD Bright"}
      }
    }
    
    if (is.null(at)) {
      at <- list()
    }
    at[[group_var]] <- group_levels
  }
  
  pred_out <- make_predictions(model, pred = pred, pred.values = pred.values,
                               at = at, center = centered,
                               interval = interval, int.type = int.type, 
                               outcome.scale = outcome.scale, robust = FALSE,
                               cluster = NULL, vcov = NULL,
                               set.offset = set.offset, return.orig.data = TRUE,
                               partial.residuals = partial.residuals, 
                               data = data, re.form = re.form, ...)
  
  # Putting these outputs into separate objects
  pm <- pred_out[[1]]
  d <- pred_out[[2]]
  
  if (length(names(group_levels)) == 0) {names(group_levels) <- group_levels}
  
  if (is.numeric(d[[pred]]) & force.cat == FALSE) {
    plot_varying_continuous(predictions = pm, pred = pred,
                           plot.points = plot.points | partial.residuals,
                           interval = interval,
                           data = d, x.label = x.label, y.label = y.label,
                           pred.labels = pred.labels, main.title = main.title,
                           colors = colors,
                           line.thickness = line.thickness, jitter = jitter,
                           resp = get_response_name(model),
                           weights = get_weights(model, d)$weights_name,
                           rug = rug, rug.sides = rug.sides,
                           point.size = point.size, point.alpha = point.alpha,
                           group_var = group_var,
                           group_levels = group_levels, vary.lty = vary.lty,
                           facet = facet)
  } else {
    plot_varying_cat(predictions = pm, pred = pred, data = d,  
             geom = cat.geom, pred.values = pred.values,
             interval = interval, plot.points = plot.points | partial.residuals,
             pred.labels = pred.labels, x.label = x.label,
             y.label = y.label, main.title = main.title,
             colors = colors, weights = get_weights(model, d)$weights_name,
             resp = get_response_name(model), jitter = jitter, 
             interval.geom = cat.interval.geom, line.thickness = line.thickness,
             point.size = point.size, pred.point.size = cat.pred.point.size,
             point.alpha = point.alpha, point.color = point.color, 
             group_var = group_var, group_levels = group_levels, 
             vary.lty = vary.lty, facet = facet)
  }
  
}

plot_varying_continuous <- function(predictions, pred, resp, group_var,
 data = NULL, plot.points = FALSE, interval = FALSE, 
 x.label = NULL, y.label = NULL, pred.labels = NULL, group_levels = NULL,
 main.title = NULL, legend.main = NULL, colors = NULL,
 line.thickness = 1.1, vary.lty = TRUE, jitter = 0, weights = NULL, 
 rug = FALSE, rug.sides = "b", point.shape = FALSE, point.size = 2, 
 facet = FALSE, point.alpha = 0.6) {
  
  d <- data
  pm <- predictions
  
  # If only 1 jitter arg, just duplicate it
  if (length(jitter) == 1) {jitter <- rep(jitter, 2)}
  
  # If no user-supplied legend title, set it to name of moderator
  if (is.null(legend.main)) {
    legend.main <- group_var
  }
  
  if (is.null(x.label)) {
    x.label <- pred
  }
  
  if (is.null(y.label)) {
    y.label <- resp
  }
  
  # Checking if user provided the colors his/herself
  if (!facet) {
    colors <- suppressWarnings(get_colors(colors, length(group_levels)))
  } else {
    colors <- try(suppressWarnings(get_colors(colors, length(group_levels))))
    if (inherits(colors, "try-error")) {
      msg_wrap("Not enough colors in color palette for the number of groups.
               Using black instead")
      colors <- rep("black", length(group_levels))
    }
  }
  
  # Manually set linetypes
  types <- c("solid", "4242", "2222", "dotdash", "dotted", "twodash",
             "12223242", "F282", "F4448444", "224282F2", "F1")
  ltypes <- types[seq_along(group_levels)]
  
  names(colors) <- group_levels
  
  # Prepare names for tidy evaluation
  pred <- sym(pred)
  resp <- sym(resp)
  grp <- sym(group_var)
  if (!is.null(weights)) {weights <- sym(weights)}
  
  lty <- if (vary.lty) sym(group_var) else NULL
  
  
  p <- ggplot(pm, aes(x = !! pred, y = !! resp, colour = !! grp,
                      group = !! grp, linetype = !! lty))
  
  p <- p + geom_path(size = line.thickness, show.legend = !facet)
  
  # Plot intervals if requested
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm,
                         aes(x = !! pred, ymin = !! sym("ymin"),
                             ymax = !! sym("ymax"), fill = !! grp,
                             group = !! grp, colour = !! grp, linetype = NA),
                         alpha = 1/5, show.legend = FALSE,
                         inherit.aes = TRUE)
  }
  
  if (facet == TRUE) {
    if (facet == TRUE) {
      num_unique <- nrow(unique(pm[group_var]))
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
    p <- p + facet_wrap(as.formula(paste0("~", group_var)), ncol = num_cols)
  }
  
  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    
    if (point.shape == TRUE) {
      shape_arg <- grp
      # Only show legend if there's a shape aesthetic
      show_legend <- TRUE
    } else {
      shape_arg <- NULL
      show_legend <- FALSE
    }
    constants <- list(alpha = point.alpha)
    if (is.null(weights)) {
      # Only use constant size if weights are not used
      constants$size <- point.size
    }
    # Need to use layer function to programmatically define constant aesthetics
    p <- p + layer(geom = "point", data = d, stat = "identity",
                   inherit.aes = TRUE, show.legend = show_legend,
                   mapping = aes(x = !! pred, y = !! resp, size = !! weights,
                                 group = !! grp, colour = !! grp,
                                 shape = !! shape_arg),
                   position = position_jitter(width = jitter[1],
                                              height = jitter[2]),
                   params = constants) +
      scale_size(range = c(1 * point.size, 5 * point.size), guide = "none")
    
  }
  
  # Rug plot for marginal distributions
  if (rug == TRUE) {
    p <- p + geom_rug(data = d, aes(linetype = NULL),
                      alpha = 0.6,
                      position = position_jitter(width = jitter[1],
                                                 height = jitter[2]),
                      sides = rug.sides, inherit.aes = TRUE)
  }
  
  # Using theme_nice for theming...but using legend title and side positioning
  if (!facet) {
    p <- p + theme_nice(legend.pos = "right")
  } else {
    # make better use of space by putting legend on bottom for facet plots
    p <- p + theme_nice(legend.pos = "bottom")
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
                               breaks = names(colors), 
                               labels = names(group_levels),
                               aesthetics = c("colour", "fill"))
  
  if (vary.lty == TRUE) { # Add line-specific changes
    p <- p + scale_linetype_manual(name = legend.main, values = ltypes,
                                   breaks = names(ltypes),
                                   labels = names(group_levels),
                                   na.value = "blank")
    # Need some extra width to show the linetype pattern fully
    p <- p + theme(legend.key.width = grid::unit(3, "lines"))
  }
  
  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }
  
  return(p)
  
}

plot_varying_cat <- function(predictions, pred, data = NULL,
  geom = c("point", "line", "bar", "boxplot"), pred.values = NULL, 
  interval = TRUE, plot.points = FALSE, pred.labels = NULL, x.label = NULL,
  y.label = NULL, main.title = NULL, colors = "black", weights = NULL,
  resp = NULL, jitter = 0.1, geom.alpha = NULL, dodge.width = NULL,
  errorbar.width = NULL, interval.geom = c("errorbar", "linerange"),
  line.thickness = 1.1, point.size = 1, pred.point.size = 3.5, 
  point.alpha = 0.6, point.color = "black") {
  
  pm <- predictions
  d <- data
  
  geom <- geom[1]
  if (geom == "dot") {geom <- "point"}
  
  # If only 1 jitter arg, just duplicate it
  if (length(jitter) == 1) {jitter <- rep(jitter, 2)}
  
  if (is.null(x.label)) {
    x.label <- pred
  }
  
  if (is.null(y.label)) {
    y.label <- resp
  }
  
  # Deal with numeric predictors coerced into factors
  if (is.numeric(pm[[pred]])) {
    pred.levels <- if (!is.null(pred.values)) {pred.values} else {
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
  
  # Convert strings to symbols for tidy evaluation
  pred <- sym(pred)
  resp <- sym(resp)
  if (!is.null(weights)) {weights <- sym(weights)}
  
  # Checking if user provided the colors his/herself
  colors <- suppressWarnings(get_colors(colors, 1))
  
  if (is.null(geom.alpha)) {
    a_level <- 1
    if (plot.points == TRUE) {
      a_level <- 0.5
    } else if (interval == TRUE) {
      a_level <- 0.5
    }
  } else {a_level <- geom.alpha}
  
  if (is.null(dodge.width)) {
    dodge.width <- if (geom %in% c("bar", "point")) {0.9} else {0}
  }
  if (is.null(errorbar.width)) {
    errorbar.width <- if (geom == "point") {
      0.75
    } else if (geom == "bar") {
      0.75
    } else {0.5}
  }
  
  
  p <- ggplot(pm, aes(x = !! pred, y = !! resp, group = 1))
  
  if (geom == "bar") {
    p <- p + geom_bar(stat = "identity", position = "dodge", alpha = a_level,
                      show.legend = FALSE, color = colors)
  } else if (geom %in% c("point", "line")) {
    p <- p + geom_point(size = pred.point.size,
                        position = position_dodge(dodge.width),
                        show.legend = FALSE, color = colors)
  }
  
  if (geom == "line") {
    p <- p + geom_path(position = position_dodge(dodge.width),
                       size = line.thickness, show.legend = FALSE, 
                       color = colors)
  }
  
  # Plot intervals if requested
  if (interval == TRUE & interval.geom[1] == "errorbar") {
    p <- p + geom_errorbar(aes(ymin = !! sym("ymin"), ymax = !! sym("ymax")),
                           alpha = 1, show.legend = FALSE,
                           position = position_dodge(dodge.width),
                           width = errorbar.width,
                           size = line.thickness, color = colors)
  } else if (interval == TRUE & interval.geom[1] %in% c("line", "linerange")) {
    p <- p + geom_linerange(aes(ymin = !! sym("ymin"), ymax = !! sym("ymax")),
                            alpha = 0.8, show.legend = FALSE,
                            position = position_dodge(dodge.width),
                            size = line.thickness, color = colors)
  }
  
  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    
    constants <- list(alpha = point.alpha, colour = point.color)
    if (is.null(weights)) {
      # Only use constant size if weights are not used
      constants$size <- point.size
    } 
    # Need to use layer function to programmatically define constant aesthetics
    p <- p + layer(geom = "point", data = d, stat = "identity",
                   inherit.aes = FALSE, show.legend = FALSE,
                   mapping = aes(x = !! pred, y = !! resp, size = !! weights),
                   position = position_jitter(width = jitter[1], 
                                              height = jitter[2]),
                   params = constants) +
      scale_size(range = c(1 * point.size, 5 * point.size))
    
  }
  
  # Using theme_apa for theming...but using legend title and side positioning
  p <- p + theme_nice() + drop_x_gridlines() + 
    labs(x = x.label, y = y.label) # better labels for axes
  
  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }
  
  # Return the plot
  return(p)
  
  
}
