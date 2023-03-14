#' @title Plot Regression Summaries
#' @description `plot_summs` and `plot_coefs` create regression coefficient
#'   plots with ggplot2.
#' @param ... regression model(s). You may also include arguments to be passed
#'   to `tidy()`.
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
#' @param colors See [jtools_colors] for more on your color options.
#'   Default: 'CUD Bright'
#' @param color.class Deprecated. Now known as `colors`.
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
#'   model from the others? Default is TRUE. You may also pass a vector of
#'   shapes to specify shapes yourself.
#' @param point.size Change the size of the points. Default is 3.
#' @param line.size Change the thickness of the error bar lines. 
#'    Default is `c(0.8, 2)`. The first number is the size for the full 
#'    width of the interval, the second number is used for the thicker 
#'    inner interval when `inner.ci` is `TRUE`.
#' @param legend.title What should the title for the legend be? Default is
#'   "Model", but you can specify it here since it is rather difficult to
#'   change later via `ggplot2`'s typical methods.
#' @param groups If you would like to have facets (i.e., separate panes) for
#'   different groups of coefficients, you can specify those groups with a
#'   list here. See details for more on how to do this.
#' @param facet.rows The number of rows in the facet grid (the `nrow` argument
#'   to [ggplot2::facet_wrap()]).
#' @param facet.cols The number of columns in the facet grid (the `nrow`
#'   argument to [ggplot2::facet_wrap()]).
#' @param facet.label.pos Where to put the facet labels. One of "top" (the
#'   default), "bottom", "left", or "right".
#' @param resp For any models that are `brmsfit` and have multiple response
#'   variables, specify them with a vector here. If the model list includes
#'   other types of models, you do not need to enter `resp` for those models.
#'   For instance, if I want to plot a `lm` object and two `brmsfit` objects,
#'   you only need to provide a vector of length 2 for `resp`.
#' @param dpar For any models that are `brmsfit` and have a distributional 
#'   dependent variable, that can be specified here. If NULL, it is assumed you 
#'   want coefficients for the location/mean parameter, not the distributional
#'   parameter(s).
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
#'   To use the `groups` argument, provide a (preferably named) list of
#'   character vectors. If I want separate panes with "Frost" and "Illiteracy"
#'   in one and "Population" and "Area" in the other, I'd make a list like
#'   this:
#'
#'   `list(pane_1 = c("Frost", "Illiteracy"), pane_2 = c("Population", "Area"))`
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
                       colors = "CUD Bright", plot.distributions = FALSE,
                       rescale.distributions = FALSE, exp = FALSE,
                       point.shape = TRUE, point.size = 5, 
                       line.size = c(0.8, 2), legend.title = "Model",
                       groups = NULL, facet.rows = NULL, facet.cols = NULL,
                       facet.label.pos = "top", color.class = colors, 
                       resp = NULL, dpar = NULL) {
  
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
                    inner_ci_level = inner_ci_level, colors = list(colors),
                    plot.distributions = plot.distributions,
                    rescale.distributions = rescale.distributions, exp = exp,
                    point.shape = list(point.shape), point.size = point.size, 
                    line.size = list(line.size), legend.title = legend.title,
                    groups = list(groups), facet.rows = facet.rows,
                    facet.cols = facet.cols, facet.label.pos = facet.label.pos, 
                    color.class = color.class, resp = resp, dpar = dpar,
                    ex_args))
  
  do.call("plot_coefs", args = args)
  
}

#' @export
#' @rdname plot_summs
#' @importFrom ggplot2 ggplot aes_string geom_vline scale_colour_brewer theme
#' @importFrom ggplot2 element_blank element_text ylab

plot_coefs <- function(..., ci_level = .95, inner_ci_level = NULL,
                       model.names = NULL, coefs = NULL,
                       omit.coefs = c("(Intercept)", "Intercept"),
                       colors = "CUD Bright", plot.distributions = FALSE,
                       rescale.distributions = FALSE,
                       exp = FALSE, point.shape = TRUE, point.size = 5,
                       line.size = c(0.8, 2), legend.title = "Model", 
                       groups = NULL, facet.rows = NULL, facet.cols = NULL,
                       facet.label.pos = "top", color.class = colors,
                       resp = NULL, dpar = NULL) {
  
  if (!requireNamespace("broom", quietly = TRUE)) {
    stop_wrap("Install the broom package to use the plot_coefs function.")
  }
  
  if (!is.numeric(line.size[1])) stop_wrap("line.size must be a number (or two
    numbers in a vector).")

  if (!all(color.class == colors)) colors <- color.class
  
  # Nasty workaround for R CMD Check warnings for global variable bindings
  model <- term <- estimate <- conf.low <- conf.high <- conf.low.inner <-
    conf.high.inner <- curve <- est <- NULL
  
  # Capture arguments
  dots <- list(...)
  
  # If first element of list is a list, assume the list is a list of models
  if (inherits(dots[[1]], 'list')) {
    mods <- dots[[1]]
    if (is.null(model.names) && !is.null(names(mods))) {
      if (is.null(model.names)) model.names <- names(mods)
    }
    if (length(dots) > 1) {
      ex_args <- dots[-1]
    } else {ex_args <- NULL}
  } else if (!is.null(names(dots))) {
    if (all(nchar(names(dots))) > 0) {
      # if all args named, need to figure out which are models
      models <- !is.na(sapply(dots, function(x) {
        out <- find_S3_class("tidy", x, package = "generics")
        if (out %in% c("list", "character", "logical", "numeric", "default")) {
          out <- NA
        }
      }))
      mods <- dots[models]
      if (is.null(model.names)) model.names <- names(dots)[models]
      if (!all(models)) {
        ex_args <- dots[models]
      } else {
        ex_args <- NULL
      }
    } else {
      mods <- dots[names(dots) == ""]
      ex_args <- dots[names(dots) != ""]
    }
  } else {
    mods <- dots
    ex_args <- NULL
  }
  
  if (!is.null(omit.coefs) && !is.null(coefs)) {
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
                        coefs = coefs, resp = resp, dpar = dpar)
  
  n_models <- length(unique(tidies$model))
  
  # Create more data for the inner interval
  if (!is.null(inner_ci_level)) {
    if (plot.distributions == FALSE || n_models == 1) {
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
  
  # Put a factor in the model for facetting purposes
  if (!is.null(groups)) {
    tidies$group <- NA
    for (g in seq_len(length(groups))) {
      if (is.null(names(groups)) || names(groups)[g] == "") {
        tidies$group[tidies$term %in% groups[[g]]] <- as.character(g)
      } else {
        tidies$group[tidies$term %in% groups[[g]]] <- names(groups)[g]
      }
    }
    if (plot.distributions == TRUE) {
      warn_wrap("Distributions cannot be plotted when groups are used.")
    }
  }
  
  p <- ggplot(data = tidies, aes(y = term, x = estimate, xmin = conf.low,
                                 xmax = conf.high, colour = model))
  
  if (!is.null(groups)) {
    if (is.null(facet.rows) && is.null(facet.cols)) {
      facet.cols <- 1
    }
    p <- p + facet_wrap(group ~ ., nrow = facet.rows, ncol = facet.cols,
                        scales = "free_y", strip.position = facet.label.pos)
  }
  
  # Checking if user provided the colors his/herself
  if (length(colors) == 1 || length(colors) != n_models) {
    colors <- get_colors(colors, n_models)
  } else {
    colors <- colors
  }
  
  # "dodge height" determined by presence of distribution plots
  dh <- as.numeric(!plot.distributions) * 0.5
  # Plot distribution layer first so it is beneath the pointrange
  if (plot.distributions == TRUE) {
    # Helper function to generate data for geom_polygon
    dist_curves <- get_dist_curves(tidies, order = levels(tidies$term),
                                   models = levels(tidies$model),
                                   rescale.distributions = rescale.distributions)
    # Draw the distributions
    p <- p + geom_polygon(data = dist_curves,
                          aes(x = est, y = curve,
                              group = interaction(term, model), fill = model),
                          alpha = 0.7, show.legend = FALSE,
                          inherit.aes = FALSE) +
      scale_fill_manual(name = legend.title,
                        values = get_colors(colors, n_models),
                        breaks = rev(levels(tidies$model)),
                        limits = rev(levels(tidies$model)))
  }
  
  # Plot the inner CI using linerange first, so the point can overlap it
  if (!is.null(inner_ci_level)) {
    if (length(line.size) < 2) {
      msg_wrap("No line.size specified for inner CI level. Defaulting to 
                line.size * 2.")
      line.size <- c(line.size, line.size * 2)
    }
    p <- p + ggplot2::geom_linerange(
      aes(y = term, xmin = conf.low.inner, xmax = conf.high.inner,
          colour = model), position = ggplot2::position_dodge(width = dh),
          # orientation = "x",
      linewidth = line.size[2], show.legend = length(mods) > 1)
  }
  
  # If there are overlapping distributions, the overlapping pointranges
  # are confusing and look bad. If plotting distributions with more than one
  # model, just plot the points with no ranges.
  if (plot.distributions == FALSE || n_models == 1) {
    # Plot the pointranges
    p <- p + ggplot2::geom_pointrange(
      aes(y = term, x = estimate, xmin = conf.low,
          xmax = conf.high, colour = model, shape = model),
      position = ggplot2::position_dodge(width = dh),
      fill = "white", fatten = point.size, linewidth = line.size[1],
      # orientation = "x",
      show.legend = length(mods) > 1) # omit legend if just a single model
  } else {
    p <- p + geom_point(
      aes(y = term, x = estimate, colour = model, shape = model),
      fill = "white", size = point.size, stroke = 1, show.legend = TRUE)
  }
  
  # To set the shape aesthetic, I prefer the points that can be filled. But
  # there are only 6 such shapes, so I need to check how many models there are.
  if (length(point.shape) == 1 && point.shape == TRUE) {
    oshapes <- c(21:25, 15:18, 3, 4, 8)
    shapes <- oshapes[seq_len(n_models)]
  } else if (length(point.shape) == 1 && is.logical(point.shape[1]) &&
             point.shape[1] == FALSE) {
    shapes <- rep(21, times = n_models)
  } else {
    if (length(point.shape) != n_models && length(point.shape) != 1) {
      stop_wrap("You must provide the same number of point shapes as the
                number of models.")
    } else if (length(point.shape) == 1) {
      shapes <- rep(point.shape, times = n_models)
    } else {
      shapes <- point.shape
    }
  }
  
  p <- p +
    geom_vline(xintercept = 1 - !exp, linetype = 2, linewidth = .25) +
    scale_colour_manual(values = colors,
                        limits = rev(levels(tidies$model)),
                        breaks = rev(levels(tidies$model)),
                        labels = rev(levels(tidies$model)),
                        name = legend.title) +
    scale_shape_manual(limits = rev(levels(tidies$model)),
                       values = shapes, name = legend.title) +
    theme_nice(legend.pos = "right") +
    drop_y_gridlines() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 10),
          panel.grid.major.x = element_line(linetype = "solid")) +
    xlab(ifelse(exp, no = "Estimate", yes = "exp(Estimate)"))
  
  # Plotting distributions often causes distributions to poke out above top
  # of plotting area so I need to set manually
  if (plot.distributions == TRUE) {
    
    p <- p + scale_y_discrete(limits = levels(tidies$term),
                              name = legend.title)
    
    yrange <- ggplot_build(p)$layout$panel_params[[1]]$y.range
    xrange <- ggplot_build(p)$layout$panel_params[[1]]$x.range
    if (is.null(yrange) && is.null(xrange)) { # ggplot 2.2.x compatibility
      yrange <- ggplot_build(p)$layout$panel_ranges[[1]]$y.range
      xrange <- ggplot_build(p)$layout$panel_ranges[[1]]$x.range
    }
    
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
                        coefs, resp = NULL, dpar = NULL) {
  
  # Need to handle complexities of resp and dpar arguments
  dpars <- NULL
  dpar_fits <- FALSE
  resps <- NULL
  if ("brmsfit" %in% sapply(mods, class)) {
    
    if (!requireNamespace("broom.mixed")) {
      stop_wrap("Please install the broom.mixed package to process `brmsfit`
                objects.")
    }
    
    mv_fits <- sapply(mods, function(x) "mvbrmsformula" %in% class(formula(x)))
    if (any(mv_fits)) {
      if (!is.null(resp) && length(resp) %nin% c(sum(mv_fits), 1)) {
        stop_wrap("The length of the `resp` argument must be either equal to
                  the number of multivariate `brmsfit` objects or 1.")
      } else if (is.null(resp)) { # Need to retrieve first DV
        resp <- lapply(mods[mv_fits], function(x) names(formula(x)[["forms"]])[1])
      }
      # Create new vector that includes the non-multivariate models
      resps <- as.list(rep(NA, length(mods)))
      # Now put resp into that vector
      resps[mv_fits] <- resp
    } 
    
    # Need to detect which models have distributional parameters
    dpar_fits <- sapply(mods, function(x) {
      if ("brmsfit" %nin% class(x)) return(FALSE)
      if ("mvbrmsformula" %in% class(formula(x))) {
        any(sapply(
          brms::brmsterms(formula(x))$terms, function(x) length(x$dpars)
        ) > 1)
      } else {
        length(brms::brmsterms(formula(x))$dpars) > 1
      }
    })
    if (!is.null(dpar)) {
      if (length(dpar) %nin% c(sum(dpar_fits), 1)) {
        stop_wrap("The length of the `dpar` argument must be either equal to
                  the number of `brmsfit` objects with a distributional model or
                  1.")
      }
      dpars <- as.list(rep(NA, length(mods)))
      dpars[dpar_fits] <- dpar 
    } 
  }
  
  # Create empty list to hold tidy frames
  tidies <- as.list(rep(NA, times = length(mods)))
  
  for (i in seq_along(mods)) {
    
    # Major kludge for methods clash between broom and broom.mixed
    # Making namespace environment with broom.mixed before generics 
    # to try to put those methods in the search path
    # Will drop after update to broom 0.7.0
    if (requireNamespace("broom.mixed")) {
      nse <- as.environment(unlist(sapply(c(asNamespace("broom.mixed"), 
                                            asNamespace("generics")),
                                          as.list)))
    } else {
      nse <- asNamespace("generics")
    }
    method_stub <- find_S3_class("tidy", mods[[i]], package = "generics")
    if (getRversion() < 3.5) {
      # getS3method() only available in R >= 3.3
      the_method <- get(paste0("tidy.", method_stub), nse,
                        mode = "function")
    } else {
      the_method <- utils::getS3method("tidy", method_stub, envir = nse)
    }
    if (!is.null(ex_args)) {
      method_args <- formals(the_method)
      
      method_args <-
        method_args[names(method_args) %nin% c("intervals", "prob")]
      
      if (method_stub == "brmsfit" && "par_type" %nin% ex_args) {
        ex_args <- c(ex_args, par_type = "non-varying", effects = "fixed")
      } 
      
      extra_args <- ex_args[names(ex_args) %in% names(method_args)]
      
    } else if (method_stub == "brmsfit" && is.null(ex_args)) {
      extra_args <- list(effects = "fixed")
    } else {
      extra_args <- NULL
    }
    
    all_args <- as.list(c(x = list(mods[[i]]), conf.int = TRUE,
                          conf.level = ci_level, extra_args))
    
    tidies[[i]] <- do.call(generics::tidy, args = all_args)
    if (!is.null(names(mods)) && any(names(mods) != "")) {
      tidies[[i]]$model <- names(mods)[i]
    } else {
      modname <- paste("Model", i)
      tidies[[i]]$model <- modname
    }
    # Deal with glht with no `term` column
    if ("term" %nin% names(tidies[[i]]) && "lhs" %in% names(tidies[[i]])) {
      tidies[[i]]$term <- tidies[[i]]$lhs
    }
    if ("brmsfit" %in% class(mods[[i]]) && (!is.null(resps) || !is.null(dpars))) {
      # See if we're selecting a DV in a multivariate model
      if (!is.null(resps) && !is.na(resps[[i]])) {
        # Now see if we're also dealing with a distributional outcome
        if (is.null(dpars) || is.na(dpars[[i]])) {
          # If not, then just select the terms referring to this DV
          tidies[[i]] <- tidies[[i]][tidies[[i]]$response == resps[[i]], ]
        } else {
          # Otherwise, select those referring to this distributional DV
          tidies[[i]] <- tidies[[i]][tidies[[i]]$response == dpars[[i]], ]
          this_dv <- grepl(paste0("^", resps[[i]], "_"), tidies[[i]]$term)
          tidies[[i]] <- tidies[[i]][this_dv, ]
          # Also want to manicure those term names because they're confusing
          tidies[[i]]$term <- 
            gsub(paste0("^", resps[[i]], "_"), "", tidies[[i]]$term)
        }
      } else if (!is.null(dpars) && !is.na(dpars[[i]])) {
        # Everything's different for non-multivariate models -__-
        # Prefixed with, e.g., sigma_
        this_dv <- grepl(paste0("^", dpars[[i]], "_"), tidies[[i]]$term)
        tidies[[i]] <- tidies[[i]][this_dv, ]
        # Drop the prefix now
        tidies[[i]]$term <- 
          gsub(paste0("^", dpars[[i]], "_"), "", tidies[[i]]$term)
      }
    } else if ("brmsfit" %in% class(mods[[i]]) && 
               any(dpar_fits) && dpar_fits[[i]]) {
      # Need to drop dpar parameters...first, need to identify them
      the_dpars <- names(brms::brmsterms(formula(mods[[i]]))$dpars)
      # Loop through them, other than the first (location) parameter
      for (the_dpar in the_dpars[-1]) {
        tidies[[i]] <-
          tidies[[i]][!grepl(paste0("^", the_dpar, "_"), tidies[[i]]$term),]
      }
    }
  }
  
  # Keep only columns common to all models
  # TODO: replicate dplyr::bind_rows behavior of keeping all columns and 
  # filling empty rows with NA
  tidies <- lapply(tidies, function(x) {
    x[Reduce(intersect, lapply(tidies, names))]
  })
  
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
  tidies$term <- factor(tidies$term, levels = rev(coefs),
                        labels = rev(names(coefs)))
  
  if (all(c("upper", "lower") %in% names(tidies)) &&
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
    # I need to know where to put this on the y-axis *numerically* since
    # geom_polygon isn't going to know about the terms on the y-axis.
    y_pos <- which(order == term_names[i])
    this_curve <- cfs[[i]]$curve
    this_curve <- (this_curve * multiplier) + y_pos
    cfs[[i]]$curve <- this_curve
    
  }
  
  out <- do.call("rbind", cfs)
  return(out)
  
}

