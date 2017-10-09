#' Plot simple effects in regression models
#'
#' \code{effect_plot()} plots regression paths. The plotting is done with
#'  \code{ggplot2} rather than base graphics, which some similar functions use.
#'
#' @param model A regression model of type \code{lm}, \code{glm},
#'   \code{\link[survey]{svyglm}}, or \code{\link[lme4]{merMod}}. Models from
#'   other classes may work as well but are not officially supported.
#'
#' @param pred The name of the predictor variable you want on the x-axis.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered.
#'   If \code{NULL}, all non-focal predictors are centered. If not \code{NULL},
#'   only the user-specified predictors are centered. User can also use "none"
#'   or "all" arguments. The response variable is not centered unless specified
#'   directly.
#'
#' @param standardize Logical. Would you like to standardize the variables
#'   that are centered? Default is \code{FALSE}, but if \code{TRUE} it will
#'   standardize variables specified by the \code{centered} argument. Note that
#'   non-focal predictors are centered when \code{centered = NULL}, its default.
#'
#' @param n.sd How many standard deviations should be used if \code{standardize
#'   = TRUE}? Default is 1, but some prefer 2.
#'
#' @param plot.points Logical. If \code{TRUE}, plots the actual data points as a
#'   scatterplot on top of the interaction lines.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction intervals
#'   the line using \code{\link[ggplot2]{geom_ribbon}}. Not supported for
#'   \code{merMod} models.
#'
#' @param int.type Type of interval to plot. Options are "confidence" or
#'  "prediction". Default is confidence interval.
#'
#' @param int.width How large should the interval be, relative to the standard error?
#'   The default, .95, corresponds to roughly 1.96 standard errors and a .05 alpha
#'   level for values outside the range. In other words, for a confidence interval,
#'   .95 is analogous to a 95\% confidence interval.
#'
#' @param outcome.scale For nonlinear models (i.e., GLMs), should the outcome
#'   variable be plotted on the link scale (e.g., log odds for logit models) or
#'   the original scale (e.g., predicted probabilities for logit models)? The
#'   default is \code{"response"}, which is the original scale. For the link
#'   scale, which will show straight lines rather than curves, use
#'   \code{"link"}.
#'
#' @param set.offset For models with an offset (e.g., Poisson models), sets a
#'   offset for the predicted values. All predicted values will have the same
#'   offset. By default, this is set to 1, which makes the predicted values a
#'   proportion.
#'
#' @param x.label A character object specifying the desired x-axis label.
#'   If \code{NULL}, the variable name is used.
#'
#' @param y.label A character object specifying the desired x-axis label.
#'   If \code{NULL}, the variable name is used.
#'
#' @param pred.labels A character vector of 2 labels for the predictor if it is
#'   a 2-level factor or a continuous variable with only 2 values. If \code{NULL},
#'   the default, the factor labels are used.
#'
#' @param main.title A character object that will be used as an overall title for the
#'   plot. If \code{NULL}, no main title is used.
#'
#' @param color.class Any palette argument accepted by
#'   \code{\link[ggplot2]{scale_colour_brewer}}.
#'
#' @param line.thickness How thick should the plotted lines be? Default is 1.1;
#'   ggplot's default is 1.
#'
#'
#' @details This function provides a means for plotting effects for the
#'   purpose of exploring regression estimates. You must have the
#'   package \code{ggplot2} installed to benefit from these plotting functions.
#'
#'   By default, all numeric predictors other than the one specified in the
#'   \code{pred} argument are mean-centered, which usually produces more
#'   intuitive plots. This only affects the y-axis in linear models, but
#'   maybe especially important/influential in non-linear/generalized linear
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
#'   Note: to use transformed predictors, e.g., \code{log(variable)},
#'   put its name in quotes or backticks in the argument.
#'
#' @return The functions returns a \code{ggplot} object, which can be treated like
#'   a user-created plot and expanded upon as such.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @seealso \code{\link[jtools]{interact_plot}} plots interaction effects,
#'   producing plots like this function but with separate lines for different
#'   levels of a moderator.
#'
#' @examples
#' # Using a fitted lm model
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder,
#'   data = states)
#' effect_plot(model = fit, pred = Murder)
#'
#' # Using interval feature
#' fit <- lm(accel ~ mag + dist, data=attenu)
#' effect_plot(fit, pred = mag, interval = TRUE,
#'   int.type = "confidence", int.width = .8)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell + meals, design = dstrat)
#' effect_plot(regmodel, pred = ell, interval = TRUE)
#'
#' # With lme4
#' \dontrun{
#' library(lme4)
#' data(VerbAgg)
#' mv <- glmer(r2 ~ Anger + mode + (1 | item), data = VerbAgg, family = binomial,
#'             control = glmerControl("bobyqa"))
#' effect_plot(mv, pred = Anger)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median
#' @export effect_plot

effect_plot <- function(model, pred, centered = NULL, standardize = FALSE,
                          n.sd = 1, plot.points = FALSE, interval = FALSE,
                          int.type = c("confidence","prediction"),
                          int.width = .95, outcome.scale = "response",
                          set.offset = 1,
                          x.label = NULL, y.label = NULL,
                          pred.labels = NULL, main.title = NULL,
                          color.class = NULL, line.thickness = 1.1) {

  # Evaluate the modx, mod2, pred args
  pred <- as.character(substitute(pred))

  # Duplicating the dataframe so it can be manipulated as needed
  d <- model.frame(model)

  # Is it a svyglm?
  if (class(model)[1] == "svyglm" || class(model)[1] == "svrepglm") {
    survey <- TRUE
    mixed <- FALSE
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
    if (class(model)[1] %in% c("glmerMod","lmerMod","nlmerMod")) {

      mixed <- TRUE
      if (interval == TRUE) {
        warning("Confidence intervals cannot be provided for random effects models.")
        interval <- FALSE
      }

    } else {
      mixed <- FALSE
    }

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

  # weights?
  if (survey == FALSE && ("(weights)" %in% names(d) |
                          !is.null(getCall(model)$weights))) {
    weights <- TRUE
    wname <- as.character(getCall(model)["weights"])
    if (any(colnames(d) == "(weights)")) {
      colnames(d)[which(colnames(d) == "(weights)")] <- wname
    }
    wts <- d[,wname]

  } else {

    weights <- FALSE
    wname <- NULL
    wts <- rep(1, times = nrow(d))

  }

  # offset?
  if (!is.null(model.offset(model.frame(model)))) {

    off <- TRUE
    # subset gives bare varname
    offname <- as.character(getCall(model)$offset[-1])

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

        d[,offname] <- exp(d[,offname])

      }

    }

    # Exponentiate offset if it was logged
    if ("log" %in% as.character(getCall(model)$offset)) {
      d[,offname] <- exp(d[,offname])
    }

  } else {

      off <- FALSE
      offname <- NULL

  }

  # For setting dimensions correctly later
  nc <- ncol(d)

  # Get the formula from lm object if given
  formula <- formula(model)
  formula <- paste(formula[2], formula[1], formula[3])

  # Pulling the name of the response variable for labeling
  resp <- sub("(.*)(?=~).*", x = formula, perl = TRUE, replacement = "\\1")
  resp <- trimws(resp)

### Centering ##################################################################

  # Update facvars by pulling out all non-focals
  facvars <-
    facvars[facvars %nin% c(pred, resp, wname, offname)]

  # Use utility function shared by all interaction functions
  c_out <- center_vals(d = d, weights = wts, facvars = facvars,
                       fvars = fvars, pred = pred,
                       resp = resp, modx = NULL, survey = survey,
                       design = design, mod2 = NULL, wname = wname,
                       offname = offname, centered = centered,
                       standardize = standardize, n.sd = n.sd)

  design <- c_out$design
  d <- c_out$d
  fvars <- c_out$fvars
  facvars <- c_out$facvars

  # Support for factor input for pred term, but only if it has two levels
  if (is.factor(d[,pred]) & length(unique(d[,pred] == 2))) {
    # Getting the labels from the factor
    predlabs <- levels(d[,pred])
    # Now convert it to a numeric variable, subtracting 1 to make it 0/1 rather
    # than 1/2
    d[,pred] <- as.numeric(d[,pred]) - 1
    # Set this indicator so it is treated properly later
    predfac <- TRUE
  } else if (is.factor(d[,pred]) & length(unique(d[,pred] != 2))) {
    # I could assume the factor is properly ordered, but that's too risky
    stop("Focal predictor (\"pred\") cannot have more than two levels. Either use it as modx or convert it to a continuous or single dummy variable.")
  } else {
    predfac <- FALSE
  }


  # Creating a set of dummy values of the focal predictor for use in predict()
  xpreds <- seq(from = range(d[!is.na(d[,pred]),pred])[1],
                to = range(d[!is.na(d[,pred]),pred])[2],
                length.out = 1000)

  # Creating matrix for use in predict()
  if (interval == TRUE) { # Only create SE columns if intervals needed

    pm <- matrix(rep(0, 1000*(nc + 2)), ncol = (nc + 2))

  } else {

    pm <- matrix(rep(0, 1000*(nc)), ncol = (nc))

  }

  # Change name of offset column to its original
  if (off == TRUE) {
    # Avoiding the variable not found stuff
    colnames(d)[colnames(d) %in% "(offset)"] <- offname
    d[,offname] <- exp(d[,offname])
  }

  # Naming columns
  if (interval == TRUE) { # if intervals, name the SE columns
    colnames(pm) <- c(colnames(d),"ymax","ymin")
  } else {
    colnames(pm) <- colnames(d)
  }
  # Convert to dataframe
  pm <- as.data.frame(pm)

  # Add values of focal predictor to df
  pm[,pred] <- xpreds

  # Set factor predictors arbitrarily to their first level
  if (length(facvars) > 0) {
    for (v in facvars) {
      pm[,v] <- levels(d[,v])[1]
    }
  }

  if (off == TRUE) {
    if (is.null(set.offset)) {
      offset.num <- median(d[,offname])
    } else {
      offset.num <- set.offset
    }

    pm[,offname] <- offset.num
    msg <- paste("Count is based on a total of", offset.num, "exposures")
    message(msg)
  }

  # Back-ticking variable names in formula to prevent problems with transformed preds
  formc <- as.character(deparse(formula(model)))
  for (var in all.vars) {

    regex_pattern <- paste("(?<=(~|\\s|\\*|\\+))", escapeRegex(var),
                           "(?=($|~|\\s|\\*|\\+))", sep = "")

    bt_name <- paste("`", var, "`", sep = "")
    formc <- gsub(regex_pattern, bt_name, formc, perl = TRUE)

  }
  form <- as.formula(formc)

  # Create predicted values based on specified levels of the focal predictor

  ## Don't update model if no vars were centered
  if (!is.null(centered) && centered == "none") {
    modelu <- model
  } else {
    if (survey == FALSE) {
      if (mixed == FALSE) {
        modelu <- update(model, formula = form, data = d)
      } else {
        optimiz <- model@optinfo$optimizer
        if (class(model) == "glmerMod") {
          modelu <- update(model, formula = form, data = d,
                           control = lme4::glmerControl(optimizer = optimiz,
                                                        calc.derivs = FALSE))
        } else {
          modelu <- update(model, formula = form, data = d,
                           control = lme4::lmerControl(optimizer = optimiz,
                                                       calc.derivs = FALSE))
        }
      }
    } else {
      # Have to do all this to avoid adding survey to dependencies
      call <- getCall(model)
      call$design <- design
      call$formula <- form
      call[[1]] <- survey::svyglm
      modelu <- eval(call)
      interval <- TRUE # predict.svyglm doesn't like se.fit = FALSE
    }
  }

  if (mixed == TRUE) {
    predicted <- as.data.frame(predict(modelu, newdata = pm,
                                       type = outcome.scale,
                                       allow.new.levels = F,
                                       re.form = ~0))
  } else {
    predicted <- as.data.frame(predict(modelu, newdata = pm,
                                       se.fit = interval,
                                       interval = int.type[1],
                                       type = outcome.scale))
  }
  pm[,resp] <- predicted[,1] # this is the actual values

  ## Convert the confidence percentile to a number of S.E. to multiply by
  intw <- 1 - ((1 - int.width)/2)
  ses <- qnorm(intw, 0, 1)

  # See minimum and maximum values for plotting intervals
  if (interval == TRUE) { # only create SE columns if intervals are needed
    if (mixed == TRUE) {
      # No SEs
      warning("Standard errors cannot be calculated for mixed effect models.")
    } else if (survey == FALSE) {
      pm[,"ymax"] <- pm[,resp] + (predicted[,"se.fit"])*ses
      pm[,"ymin"] <- pm[,resp] - (predicted[,"se.fit"])*ses
    } else if (survey == TRUE) {
      pm[,"ymax"] <- pm[,resp] + (predicted[,"SE"])*ses
      pm[,"ymin"] <- pm[,resp] - (predicted[,"SE"])*ses
    }
  } else {
    # Do nothing
  }

  # Saving x-axis label
  if (is.null(x.label)){
    x.label <- pred
  }

  # Saving y-axis label
  if (is.null(y.label)){
    y.label <- resp
  }

  # Starting plot object
  p <- ggplot2::ggplot(pm, ggplot2::aes(x=pm[,pred], y=pm[,resp]))

  # Define line thickness
  p <- p + ggplot2::geom_path(size = line.thickness)

  # Plot intervals if requested
  if (interval==TRUE) {
    p <- p + ggplot2::geom_ribbon(data=pm, ggplot2::aes(ymin=pm[,"ymin"],
                                                        ymax=pm[,"ymax"]),
                                  alpha=1/5, show.legend = FALSE)
  }

  # Plotting the observed points
  if (plot.points == TRUE) {
    p <- p + ggplot2::geom_point(data=d, ggplot2::aes(x=d[,pred], y=d[,resp]),
                                 inherit.aes = F, position = "jitter")
  }

  # Using theme_apa for theming...but using legend title and side positioning
  p <- p + theme_apa(legend.pos = "right", legend.use.title = TRUE)

  p <- p + ggplot2::labs(x = x.label, y = y.label) # better labels for axes

  # Getting rid of tick marks for factor predictor
  if (predfac == TRUE) {
    if (is.null(pred.labels)) { # Let pred.labels override factor labels
      p <- p + ggplot2::scale_x_continuous(breaks = c(0,1), labels = predlabs)
    } else { # Use the factor labels
      p <- p + ggplot2::scale_x_continuous(breaks = c(0,1), labels = pred.labels)
    }
  } else if (length(unique(d[,pred])) == 2) { # Predictor has only two unique values
    # Make sure those values are in increasing order
    brks <- sort(unique(d[,pred]), decreasing = F)
    if (is.null(pred.labels)) {
      p <- p + ggplot2::scale_x_continuous(breaks = brks)
    } else {
      if (length(pred.labels) == 2) { # Make sure pred.labels has right length
        p <- p + ggplot2::scale_x_continuous(breaks = brks, labels = pred.labels)
      } else {
        warning("pred.labels argument has the wrong length. It won't be used")
        p <- p + ggplot2::scale_x_continuous(breaks = brks)
      }
    }
  }

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggplot2::ggtitle(main.title)
  }

  # Return the plot
  return(p)

}

#' @export

print.effect_plot <- function(x, ...) {
  print(x)
}
