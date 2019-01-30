
#### Effect plot ##############################################################

### So closely related to interact_plot that I'll keep them in one file

#' Plot simple effects in regression models
#'
#' \code{effect_plot()} plots regression paths. The plotting is done with
#'  \code{ggplot2} rather than base graphics, which some similar functions use.
#'
#' @param model A regression model. The function is tested with \code{lm},
#'   \code{glm}, \code{\link[survey]{svyglm}}, \code{\link[lme4]{merMod}},
#'   \code{\link[quantreg]{rq}}, \code{\link[brms]{brmsfit}},
#'   \code{stanreg} models.
#'   Models from other classes may work as well but are not officially
#'   supported. The model should include the interaction of interest.
#'
#' @param pred The name of the predictor variable involved
#'  in the interaction. This can be a bare name or string. Note that it
#'  is evaluated using `rlang`, so programmers can use the `!!` syntax
#'  to pass variables instead of the verbatim names.
#'
#' @param centered A vector of quoted variable names that are to be
#'   mean-centered. If `"all"`, all non-focal predictors are centered. You
#'   may instead pass a character vector of variables to center. User can
#'   also use "none" to base all predictions on variables set at 0.
#'   The response variable, `pred`, weights, and offset variables are never
#'   centered.
#'
#' @param data Optional, default is NULL. You may provide the data used to
#'   fit the model. This can be a better way to get mean values for centering
#'   and can be crucial for models with variable transformations in the formula
#'   (e.g., `log(x)`) or polynomial terms (e.g., `poly(x, 2)`). You will
#'   see a warning if the function detects problems that would likely be
#'   solved by providing the data with this argument and the function will
#'   attempt to retrieve the original data from the global environment.
#'
#' @param plot.points Logical. If \code{TRUE}, plots the actual data points as
#'   a scatterplot on top of the interaction lines. The color of the dots will
#'   be based on their moderator value.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction
#'   intervals around the line using \code{\link[ggplot2]{geom_ribbon}}.
#'
#' @param int.type Type of interval to plot. Options are "confidence" or
#'  "prediction". Default is confidence interval.
#'
#' @param int.width How large should the interval be, relative to the standard
#'   error? The default, .95, corresponds to roughly 1.96 standard errors and
#'   a .05 alpha level for values outside the range. In other words, for a
#'   confidence interval, .95 is analogous to a 95\% confidence interval.
#'
#' @param outcome.scale For nonlinear models (i.e., GLMs), should the outcome
#'   variable be plotted on the link scale (e.g., log odds for logit models) or
#'   the original scale (e.g., predicted probabilities for logit models)? The
#'   default is \code{"response"}, which is the original scale. For the link
#'   scale, which will show straight lines rather than curves, use
#'   \code{"link"}.
#'
#' @inheritParams summ.lm
#' @inheritParams make_predictions
#'
#' @param vcov Optional. You may supply the variance-covariance matrix of the
#'  coefficients yourself. This is useful if you are using some method for
#'  robust standard error calculation not supported by the \pkg{sandwich}
#'  package.
#'
#' @param set.offset For models with an offset (e.g., Poisson models), sets an
#'   offset for the predicted values. All predicted values will have the same
#'   offset. By default, this is set to 1, which makes the predicted values a
#'   proportion. See details for more about offset support.
#'
#' @param x.label A character object specifying the desired x-axis label. If
#'   \code{NULL}, the variable name is used.
#'
#' @param y.label A character object specifying the desired x-axis label. If
#'   \code{NULL}, the variable name is used.
#'
#' @param pred.labels A character vector of labels for categorical predicotrs.
#'  If \code{NULL}, the default, the factor labels are used.
#'
#' @param main.title A character object that will be used as an overall title
#'   for the plot. If \code{NULL}, no main title is used.
#'
#' @param colors See [jtools_colors] for details on the types of arguments
#'    accepted. Default is "black". 
#' 
#' @param color.class Deprecated. Now known as `colors`.
#'
#' @param line.thickness How thick should the plotted lines be? Default is 1.1;
#'   ggplot's default is 1.
#'
#' @param jitter How much should `plot.points` observed values be "jittered"
#'    via [ggplot2::position_jitter()]? When there are many points near each
#'    other, jittering moves them a small amount to keep them from
#'    totally overlapping. In some cases, though, it can add confusion since
#'    it may make points appear to be outside the boundaries of observed
#'    values or cause other visual issues. Default is 0, but try various
#'    small values (e.g., 0.1) and increase as needed if your points are
#'    overlapping too much. If the argument is a vector with two values,
#'    then the first is assumed to be the jitter for width and the second
#'    for the height.
#'
#' @param rug Show a rug plot in the margins? This uses [ggplot2::geom_rug()]
#'    to show the distribution of the predictor (top/bottom) and/or
#'    response variable (left/right) in the original data. Default is
#'    FALSE.
#'
#' @param rug.sides On which sides should rug plots appear? Default is "b",
#'    meaning bottom. "t" and/or "b" show the distribution of the predictor
#'    while "l" and/or "r" show the distribution of the response. "bl" is
#'    a good option to show both the predictor and response.
#'
#' @param point.size What size should be used for observed data when
#'   `plot.points` is TRUE? Default is 1.
#'
#' @param robust Should robust standard errors be used to find confidence
#'   intervals for supported models? Default is FALSE, but you should specify
#'   the type of sandwich standard errors if you'd like to use them (i.e.,
#'   `"HC0"`, `"HC1"`, and so on). If `TRUE`, defaults to `"HC3"` standard
#'   errors.
#'
#' @param cluster For clustered standard errors, provide the column name of
#'   the cluster variable in the input data frame (as a string). Alternately,
#'   provide a vector of clusters.
#'
#' @param ... extra arguments passed to `make_predictions`
#' 
#' @param pred.values Values of `pred` to use instead of the equi-spaced 
#'   series by default (for continuous variables) or all unique values (for
#'   non-continuous variables).
#' @param force.cat Force the continuous `pred` to be treated as categorical?
#'   default is FALSE, but this can be useful for things like dummy 0/1 
#'   variables.
#' @param cat.geom If `pred` is categorical (or `force.cat` is TRUE), 
#'   what type of plot should this be? There are several options
#'   here since the best way to visualize categorical interactions varies by
#'   context. Here are the options:
#'
#'   * `"point"`: The default. Simply plot the point estimates. You may want to
#'      use `point.shape = TRUE` with this and you should also consider
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
#' @param cat.interval.geom For categorical by categorical interactions.
#'   One of "errorbar" or "linerange". If the former,
#'   [ggplot2::geom_errorbar()] is used. If the latter,
#'   [ggplot2::geom_linerange()] is used.
#' @param cat.pred.point.size (for categorical `pred`)
#'  If TRUE and `geom` is `"point"` or `"line"`,
#'  sets the size of the predicted points. Default is 3.5.
#'  Note the distinction from `point.size`, which refers to the
#'  observed data points.
#' @param point.alpha What should the `alpha` aesthetic for plotted points of 
#'  observed data be? Default is 0.6, and it can range from 0 (transparent) to 
#'  1 (opaque).
#' @param point.color What should the `colour` aesthetic for plotted points of 
#'  observed data be? Default is "black".
#' @param partial.residuals Instead of plotting the observed data, you may plot
#'  the partial residuals (controlling for the effects of variables besides 
#'  `pred`). 
#'
#' @details This function provides a means for plotting effects for the
#'   purpose of exploring regression estimates. You must have the
#'   package \code{ggplot2} installed to benefit from these plotting functions.
#'
#'   By default, all numeric predictors other than the one specified in the
#'   \code{pred} argument are mean-centered, which usually produces more
#'   intuitive plots. This only affects the y-axis in linear models, but
#'   may be especially important/influential in non-linear/generalized linear
#'   models.
#'
#'   This function supports nonlinear and generalized linear models and by
#'   default will plot them on
#'   their original scale (\code{outcome.scale = "response"}).
#'
#'   While mixed effects models from \code{lme4} are supported, only the fixed
#'   effects are plotted. \code{lme4} does not provide confidence intervals,
#'   so they are not supported with this function either.
#'
#'   Note: to use transformed predictors, e.g., `log(x)`, or polynomials,
#'   e.g., `poly(x, 2)`, provide the raw variable name (`x`) to the `pred =`
#'   argument. You will need to input the data frame used to fit the model with
#'   the `data =` argument.
#'
#' @return The functions returns a \code{ggplot} object, which can be treated
#'   like
#'   a user-created plot and expanded upon as such.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @seealso \code{\link[interactions]{interact_plot}} plots interaction effects,
#'   producing plots like this function but with separate lines for different
#'   levels of a moderator. \code{\link[interactions]{cat_plot}} does the same
#'   for categorical by categorical interactions.
#'
#' @examples
#' # Using a fitted lm model
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder,
#'   data = states)
#' effect_plot(model = fit, pred = Murder)
#'
#' # Using polynomial predictor, plus intervals
#' fit <- lm(accel ~ poly(mag,3) + dist, data = attenu)
#' effect_plot(fit, pred = mag, interval = TRUE,
#'   int.type = "confidence", int.width = .8, data = attenu) # note data arg.
#'
#' # With svyglm
#' if (requireNamespace("survey")) {
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell + meals, design = dstrat)
#' effect_plot(regmodel, pred = ell, interval = TRUE)
#' }
#'
#' # With lme4
#' \dontrun{
#' library(lme4)
#' data(VerbAgg)
#' mv <- glmer(r2 ~ Anger + mode + (1 | item), data = VerbAgg,
#'             family = binomial,
#'             control = glmerControl("bobyqa"))
#' effect_plot(mv, pred = Anger)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median weights
#' @import ggplot2
#' @import rlang
#' @export effect_plot

effect_plot <- function(model, pred, pred.values = NULL, centered = "all",
  plot.points = FALSE, interval = FALSE, data = NULL, at = NULL,
  int.type = c("confidence","prediction"), int.width = .95,
  outcome.scale = "response", robust = FALSE, cluster = NULL, vcov = NULL, 
  set.offset = 1, x.label = NULL, y.label = NULL, pred.labels = NULL,
  main.title = NULL, colors = "black", line.thickness = 1.1, 
  point.size = 1, point.alpha = 0.6, point.color = "black", jitter = 0,
  rug = FALSE, rug.sides = "b", 
  force.cat = FALSE, cat.geom = c("point", "line", "bar", "boxplot"), 
  cat.interval.geom = c("errorbar", "linerange"), cat.pred.point.size = 3.5, 
  partial.residuals = FALSE, color.class = colors, ...) {
  
  # Evaluate the pred arg
  pred <- quo_name(enexpr(pred))
  
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
  
  # Deal with legacy color argument
  if (!all(color.class == colors)) colors <- color.class
  
  pred_out <- make_predictions(model, pred = pred, pred.values = pred.values,
                               at = NULL, center = centered,
                               interval = interval, int.type = int.type, 
                               outcome.scale = outcome.scale, robust = robust,
                               cluster = cluster, vcov = vcov,
                               set.offset = set.offset, return.orig.data = TRUE,
                               partial.residuals = partial.residuals, ...)
  
  # Putting these outputs into separate objects
  pm <- pred_out[[1]]
  d <- pred_out[[2]]
  
  # # Check for factor predictor
  # if (is.factor(d[[pred]])) {
  #   # I could assume the factor is properly ordered, but that's too risky
  #   stop("Focal predictor (\"pred\") cannot be a factor. Either",
  #        " use it as modx, convert it to a numeric dummy variable,",
  #        " or use the cat_plot function for factor by factor interaction",
  #        " plots.")
  # }
  
  if (is.numeric(d[[pred]]) & force.cat == FALSE) {
    plot_effect_continuous(predictions = pm, pred = pred,
                           plot.points = plot.points | partial.residuals,
                           interval = interval,
                           data = d, x.label = x.label, y.label = y.label,
                           pred.labels = pred.labels, main.title = main.title,
                           colors = colors,
                           line.thickness = line.thickness, jitter = jitter,
                           resp = get_response_name(model),
                           wts = get_weights(model, d)$weights,
                           rug = rug, rug.sides = rug.sides,
                           point.size = point.size, point.alpha = point.alpha,
                           point.color = point.color)
  } else {
    plot_cat(predictions = pm, pred = pred, data = d,  
             geom = cat.geom, pred.values = pred.values,
             interval = interval, plot.points = plot.points | partial.residuals,
             pred.labels = pred.labels, x.label = x.label,
             y.label = y.label, main.title = main.title,
             colors = colors, wts = get_weights(model, d)$weights,
             resp = get_response_name(model), jitter = jitter, 
             interval.geom = cat.interval.geom, line.thickness = line.thickness,
             point.size = point.size, pred.point.size = cat.pred.point.size,
             point.alpha = point.alpha, point.color = point.color)
  }
  
}

plot_effect_continuous <- 
  function(predictions, pred, plot.points = FALSE, interval = FALSE, 
           data = NULL, x.label = NULL, y.label = NULL, pred.labels = NULL,
           main.title = NULL, colors = NULL, line.thickness = 1.1,
           jitter = 0.1, resp = NULL, wts = NULL, rug = FALSE, rug.sides = "b",
           point.size = 1, point.alpha = 0.6, point.color = "black") {
  
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

  # Plot observed data — do this first to plot the line over the points
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts)/sum(wts) # scaling constant
    # make the range of values larger, but only if there are non-1 weights
    const <- const * (1 * all(wts == 1) + point.size)
    wts <- const * wts
    # Append weights to data
    d[["the_weights"]] <- wts
    point.size <- wts
    
    p <- p + geom_point(data = d,
                        aes_string(x = pred_g, y = resp_g,
                                   size = "the_weights"),
                        position = position_jitter(width = jitter[1],
                                                   height = jitter[2]),
                        inherit.aes = FALSE, show.legend = FALSE,
                        size = point.size, alpha = point.alpha,
                        colour = point.color)
    # Add size aesthetic to avoid giant points
    # p <- p + scale_size(range = c(0.3, 4))
    # p <- p + scale_size_identity()
    
  }
  
  # Define line thickness
  p <- p + geom_path(size = line.thickness)
  
  # Plot intervals if requested
  if (interval == TRUE) {
    p <- p + geom_ribbon(data = pm, aes_string(ymin = "ymin",
                                               ymax = "ymax"),
                         alpha = 1/5, show.legend = FALSE)
  }
  
  # Rug plot for marginal distributions
  if (rug == TRUE) {
    p <- p + geom_rug(data = d,
                      mapping = aes_string(x = pred_g, y = resp_g), alpha = 0.6,
                      position = position_jitter(width = jitter[1]),
                      sides = rug.sides, inherit.aes = TRUE)
  }
  
  # Using theme_apa for theming...but using legend title and side positioning
  p <- p + theme_nice(legend.pos = "right")
  
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

plot_cat <- function(predictions, pred, data = NULL, 
 geom = c("point", "line", "bar", "boxplot"), pred.values = NULL,
 interval = TRUE, plot.points = FALSE, pred.labels = NULL, x.label = NULL,
 y.label = NULL, main.title = NULL, colors = "black", wts = NULL,
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
    dodge.width <- if (geom %in% c("bar", "point", "boxplot")) {0.9} else {0}
  }
  if (is.null(errorbar.width)) {
    errorbar.width <- if (geom == "point") {
      0.75
    } else if (geom == "bar") {
      0.75
    } else {0.5}
  }
  

  p <- ggplot(pm, aes_string(x = pred_g, y = resp_g, group = 1))
  
  if (geom == "bar") {
    p <- p + geom_bar(stat = "identity", position = "dodge", alpha = a_level,
                      show.legend = FALSE, color = colors)
  } else if (geom == "boxplot") {
    p <- ggplot(d, aes_string(x = pred_g, y = resp_g), color = colors) +
      geom_boxplot(position = position_dodge(dodge.width), show.legend = FALSE)
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
  if (interval == TRUE && geom != "boxplot" && interval.geom == "errorbar") {
    p <- p + geom_errorbar(aes_string(ymin = "ymin", ymax = "ymax"),
                           alpha = 1, show.legend = FALSE,
                           position = position_dodge(dodge.width),
                           width = errorbar.width,
                           size = line.thickness, color = colors)
  } else if (interval == TRUE && geom != "boxplot" && interval.geom %in%
             c("line", "linerange")) {
    p <- p + geom_linerange(aes_string(ymin = "ymin", ymax = "ymax"),
                            alpha = 0.8, show.legend = FALSE,
                            position = position_dodge(dodge.width),
                            size = line.thickness, color = colors)
  }
  
  # For factor vars, plotting the observed points
  # and coloring them by factor looks great
  if (plot.points == TRUE) {
    # Transform weights so they have mean = 1
    const <- length(wts) / sum(wts) # scaling constant
    # make the range of values larger, but only if there are non-1 weights
    const <- const * (1 * all(wts == 1) * point.size)
    wts <- const * wts
    # Append weights to data
    d[,"the_weights"] <- wts

    p <- p + geom_point(data = d, aes_string(x = pred_g, y = resp_g,
                                             size = "the_weights"),
                        position =
                          position_jitterdodge(dodge.width = dodge.width,
                                               jitter.width = jitter[1],
                                               jitter.height = jitter[2]),
                        inherit.aes = FALSE,
                        show.legend = FALSE,
                        alpha = point.alpha, colour = point.color)
    
    # Add size aesthetic to avoid giant points
    p <- p + scale_size_identity()
    
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