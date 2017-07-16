#' Plot interaction effects in regression models
#'
#' \code{interact_plot()} plots regression lines at user-specified levels of a moderator
#' variable to explore interactions. The plotting is done with \code{ggplot2} rather than
#' base graphics, which some similar functions use.
#'
#' @param model A regression model of type \code{lm}, \code{glm},
#'   \code{\link[survey]{svyglm}}, or \code{\link[lme4]{merMod}}. It should
#'   contain the interaction of interest. Models from other classes may work as
#'   well but are not officially supported.
#'
#' @param pred The name of the predictor variable involved
#'  in the interaction.
#'
#' @param modx The name of the moderator variable involved
#'  in the interaction.
#'
#' @param mod2 Optional. The name of the second moderator
#'  variable involved in the interaction.
#'
#' @param modxvals For which values of the moderator should lines be plotted?
#'   Default is \code{NULL}. If \code{NULL}, then the customary +/- 1 standard
#'   deviation from the mean as well as the mean itself are used for continuous
#'   moderators. If the moderator is a factor variable and \code{modxvals} is
#'   \code{NULL}, each level of the factor is included. If
#'   \code{"plus-minus"}, plots lines when the moderator is at +/- 1 standard
#'   deviation without the mean.
#'
#' @param mod2vals For which values of the second moderator should the plot be
#'   facetted by? That is, there will be a separate plot for each level of this
#'   moderator. Defaults are the same as \code{modxvals}.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered. If
#'   \code{NULL}, all non-focal predictors are centered. If not \code{NULL}, only
#'   the user-specified predictors are centered. User can also use "none" or "all"
#'   arguments. The response variable is not centered unless specified directly.
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
#'   scatterplot on top of the interaction lines. If moderator is a factor, the dots
#'   will be the same color as their parent factor.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction intervals
#'   the line using \code{\link[ggplot2]{geom_ribbon}}. Not supported for
#'   \code{merMod} models.
#'
#' @param int.type Type of interval to plot. Options are "confidence" or "prediction".
#'   Default is confidence interval.
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
#' @param x.label A character object specifying the desired x-axis label. If \code{NULL},
#'   the variable name is used.
#'
#' @param y.label A character object specifying the desired x-axis label. If \code{NULL},
#'   the variable name is used.
#'
#' @param pred.labels A character vector of 2 labels for the predictor if it is
#'   a 2-level factor or a continuous variable with only 2 values. If \code{NULL},
#'   the default, the factor labels are used.
#'
#' @param modx.labels A character vector of labels for each level of the moderator values,
#'   provided in the same order as the \code{modxvals} argument. If \code{NULL},
#'   the values themselves are used as labels unless \code{modxvals} is also \code{NULL}.
#'   In that case, "+1 SD" and "-1 SD" are used.
#'
#' @param mod2.labels A character vector of labels for each level of the 2nd moderator
#'   values, provided in the same order as the \code{mod2vals} argument. If \code{NULL},
#'   the values themselves are used as labels unless \code{mod2vals} is also \code{NULL}.
#'   In that case, "+1 SD" and "-1 SD" are used.
#'
#' @param main.title A character object that will be used as an overall title for the
#'   plot. If \code{NULL}, no main title is used.
#'
#' @param legend.main A character object that will be used as the title that appears
#'   above the legend. If \code{NULL}, the name of the moderating variable is used.
#'
#' @param color.class Any palette argument accepted by
#'   \code{\link[ggplot2]{scale_colour_brewer}}. Default is "Set2" for factor moderators,
#'   "Blues" for +/- SD and user-specified \code{modxvals} values.
#'
#' @param line.thickness How thick should the plotted lines be? Default is 1.1;
#'   ggplot's default is 1.
#'
#' @param vary.lty Should the resulting plot have different shapes for each line
#'   in addition to colors? Defaults to \code{TRUE}.
#'
#'
#' @details This function provides a means for plotting conditional effects for the
#'   purpose of exploring interactions in the context of regression. You must have the
#'   package \code{ggplot2} installed to benefit from these plotting functions.
#'
#'   The function is designed for two and three-way interactions. For
#'    additional terms, the
#'   \code{\link[effects]{effects}} package may be better suited to the task.
#'
#'   This function supports nonlinear and generalized linear models and by
#'   default will plot them on
#'   their original scale (\code{outcome.scale = "response"}).
#'
#'   While mixed effects models from \code{lme4} are supported, only the fixed
#'   effects are plotted. \code{lme4} does not provide confidence intervals,
#'   so they are not supported with this function either.
#'
#' @return The functions returns a \code{ggplot} object, which can be treated like
#'   a user-created plot and expanded upon as such.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @family interaction tools
#'
#' @seealso \code{\link[rockchalk]{plotSlopes}} from \pkg{rockchalk} performs a
#'   similar function, but
#'   with R's base graphics---this function is meant, in part, to emulate
#'   its features.
#'
#'   \code{\link{sim_slopes}} performs a simple slopes analysis with a similar
#'   argument syntax to this function.
#'
#' @references
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and multilevel
#'  regression: Inferential and graphical techniques. \emph{Multivariate Behavioral
#'  Research}, \emph{40}(3), 373-400.
#'  \url{http://dx.doi.org/10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied multiple
#' regression/correlation analyses for the behavioral sciences} (3rd ed.).
#' Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' @examples
#' # Using a fitted lm model
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder*Illiteracy,
#'   data = states)
#' interact_plot(model = fit, pred = Murder,
#'   modx = Illiteracy)
#'
#' # Using interval feature
#' fit <- lm(accel ~ mag*dist, data=attenu)
#' interact_plot(fit, pred = mag, modx = dist, interval = TRUE,
#'   int.type = "confidence", int.width = .8)
#'
#' # Using second moderator
#' fit <- lm(Income ~ HSGrad*Murder*Illiteracy,
#'   data = states)
#' interact_plot(model = fit, pred = Murder,
#'   modx = Illiteracy, mod2 = HSGrad)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell*meals,design=dstrat)
#' interact_plot(regmodel, pred = ell, modx = meals)
#'
#' # With lme4
#' \dontrun{
#' library(lme4)
#' data(VerbAgg)
#' mv <- glmer(r2 ~ Anger * mode + (1 | item), data = VerbAgg, family = binomial,
#'             control = glmerControl("bobyqa"))
#' interact_plot(mv, pred = Anger, modx = mode)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median
#' @export interact_plot

interact_plot <- function(model, pred, modx, modxvals = NULL, mod2 = NULL,
                          mod2vals = NULL, centered = NULL, standardize = FALSE,
                          n.sd = 1, plot.points = FALSE, interval = FALSE,
                          int.type = c("confidence","prediction"),
                          int.width = .95, outcome.scale = "response",
                          set.offset = 1,
                          x.label = NULL, y.label = NULL,
                          pred.labels = NULL, modx.labels = NULL,
                          mod2.labels = NULL, main.title = NULL,
                          legend.main = NULL, color.class = NULL,
                          line.thickness = 1.1, vary.lty = TRUE) {

  # Evaluate the modx, mod2, pred args
  pred <- as.character(substitute(pred))
  modx <- as.character(substitute(modx))
  mod2 <- as.character(substitute(mod2))
  # To avoid unexpected behavior, need to un-un-parse mod2 when it is NULL
  if (length(mod2) == 0) {
    mod2 <- NULL
  }

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

    facvars <- c()
    for (v in fvars) {
      if (is.factor((d[,v])) && length(unique(d[,v])) > 2) {
        facvars <- c(facvars, v)
      }
    }

  }

  # weights?
  if (survey == FALSE && "(weights)" %in% names(d)) {
    weights <- TRUE
    wname <- as.character(model$call["weights"])
    colnames(d)[which(colnames(d) == "(weights)")] <- wname
  } else {
    weights <- FALSE
    wname <- NULL
  }

  # offset?
  if (!is.null(model.offset(model.frame(model)))) {
    off <- TRUE
    offname <- as.character(model$call$offset[-1])
  } else {
    off <- FALSE
    offname <- NULL
  }

  # Setting default for colors
  if (is.factor(d[,modx])) {
    facmod <- TRUE
    if (is.null(color.class)) {
      color.class <- "Set2"
    }
    # Unrelated, but good place to throw a warning
    if (!is.null(modxvals)) {
      warning("All levels of factor must be used. Ignoring modxvals argument...")
      modxvals <- NULL
    }
  } else {
    facmod <- FALSE
    if (is.null(color.class)) {
      color.class <- "Blues"
    }
  }

  # For setting dimensions correctly later
  nc <- ncol(d)

  # Get the formula from lm object if given
  formula <- formula(model)
  formula <- paste(formula[2],formula[1],formula[3])

  # Pulling the name of the response variable for labeling
  resp <- sub("(.*)(?=~).*", x=formula, perl=T, replacement="\\1")
  resp <- trimws(resp)

  # Update facvars by pulling out all non-focals
  facvars <- facvars[!(facvars %in% c(pred, resp, modx, mod2, wname, "(offset)"))]

  # Handling user-requested centered vars
  if (!is.null(centered) && centered != "all" && centered != "none") {
    if (survey == FALSE) {
      if (weights == FALSE) {
        d <- gscale(x = centered, data = d, center.only = !standardize,
                    n.sd = n.sd)
      } else {
        d <- gscale(x = centered, data = d, center.only = !standardize,
                    weights = wname, n.sd = n.sd)
      }
    } else if (survey == TRUE) {
      design <- gscale(x = centered, data = design, center.only = !standardize,
                       n.sd = n.sd)
      d <- design$variables
    }
  } else if (!is.null(centered) && centered == "all") {
    # Need to handle surveys differently within this condition
    vars <- names(d)[!(names(d) %in% c(resp, wname, "(offset)"))] # saving all vars expect response
    if (survey == FALSE) {
      if (weights == FALSE) {
        d <- gscale(x = vars, data = d, center.only = !standardize,
                    n.sd = n.sd)
      } else {
        d <- gscale(x = vars, data = d, center.only = !standardize,
                    weights = wname, n.sd = n.sd)
      }
    } else {
      # Need a different strategy for not iterating over unimportant vars for
      # survey designs
      ndfvars <- fvars[!(fvars %in% c(resp, wname, "(offset)"))]
      if (length(ndfvars) > 0) {
        design <- gscale(x = ndfvars, data = design, center.only = !standardize,
                         n.sd = n.sd)
        d <- design$variables
      }
    }
  } else if (!is.null(centered) && centered == "none") {

  } else { # Center all non-focal
    # Centering the non-focal variables to make the slopes more interpretable
    vars <- names(d)[!(names(d) %in% c(pred, resp, modx, mod2, wname, "(offset)"))]
    # Need to handle surveys differently within this condition
    if (survey == FALSE) {
      if (weights == FALSE) {
        d <- gscale(x = vars, data = d, center.only = !standardize,
                    n.sd = n.sd)
      } else {
        d <- gscale(x = vars, data = d, center.only = !standardize,
                    weights = wname, n.sd = n.sd)
      }
    } else {
      # Need a different strategy for not iterating over unimportant vars for
      # survey designs
      nfvars <- fvars[!(fvars %in% c(pred, resp, modx, mod2, wname, "(offset)"))]
      if (length(nfvars) > 0) {
        design <- gscale(x = nfvars, data = design, center.only = !standardize,
                         n.sd = n.sd)
        d <- design$variables
      }
    }
  }

  # Fixes a data type error with predict() later
  d <- as.data.frame(d)

  # Default to +/- 1 SD unless modx is factor
  if (is.null(modxvals) && !is.factor(d[,modx]) &&
      length(unique(d[,modx])) > 2) {

    if (survey == FALSE) {

      if (weights == FALSE) { # can have weights w/o svyglm

        modsd <- sd(d[,modx]) # save the SD
        # Now save a vector of those focal values
        modxvalssd <- c(mean(d[,modx]) + modsd,
                        mean(d[,modx]),
                        mean(d[,modx]) - modsd)
        # Name the vector for better labeling of the plot
        names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")

      } else { # Same thing, but with weights

        modsd <- wtd.sd(d[,modx], d[,wname])
        modxvalssd <- c(weighted.mean(d[,modx], d[,wname]) + modsd,
                        weighted.mean(d[,modx], d[,wname]),
                        weighted.mean(d[,modx], d[,wname]) - modsd)
        names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")

      }

      modxvals2 <- modxvalssd # Saving the values to a new object for use later

    } else if (survey == TRUE) {

      modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
      # Have to construct the formula this way since the syntax for svymean
      # differs from mean
      modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")),
                                 design = design)
      modxvalssd <- c(modmean + modsd,
                      modmean,
                      modmean - modsd)
      names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")

      modxvals2 <- modxvalssd

    }

  } else if (!is.factor(d[,modx]) && class(modxvals) == "character" &&
                        modxvals == "plus-minus") { # No mean

    if (survey == FALSE) {

      if (weights == FALSE) {

        modsd <- sd(d[,modx])
        modxvalssd <- c(mean(d[,modx]) + modsd,
                        mean(d[,modx]) - modsd)
        names(modxvalssd) <- c("+1 SD", "-1 SD")
        modxvals2 <- modxvalssd

      } else {

        modsd <- wtd.sd(d[,modx], d[,wname])
        modxvalssd <- c(weighted.mean(d[,modx], d[,wname]) + modsd,
                        weighted.mean(d[,modx], d[,wname]) - modsd)
        names(modxvalssd) <- c("+1 SD", "-1 SD")
        modxvals2 <- modxvalssd

      }

    } else if (survey == TRUE) {

      modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
      modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")), design = design)
      modxvalssd <- c(modmean+modsd, modmean-modsd)
      names(modxvalssd) <- c("+1 SD", "-1 SD")
      modxvals2 <- modxvalssd

    }

  } else if (is.null(modxvals) && !is.factor(d[,modx]) &&
             length(unique(d[,modx])) == 2) {

    modxvals2 <- as.numeric(levels(factor(d[,modx])))

  } else if (is.null(modxvals) && is.factor(d[,modx])){

    modxvals2 <- levels(d[,modx])

  } else { # Use user-supplied values otherwise

    modxvals2 <- sort(modxvals, decreasing = T)

  }

  # Same process for second moderator
  if (!is.null(mod2)) {

    if (is.null(mod2vals) && !is.factor(d[,mod2]) &&
        length(unique(d[,mod2])) > 2) {

      if (survey == FALSE) {

        if (weights == FALSE) {
          mod2sd <- sd(d[,mod2])
          mod2valssd <- c(mean(d[,mod2]) + mod2sd,
                          mean(d[,mod2]),
                          mean(d[,mod2]) - mod2sd)
          names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                                 paste("Mean of", mod2),
                                 paste("Mean of", mod2, "-1 SD"))

        } else {

          mod2sd <- wtd.sd(d[,mod2], d[,wname])
          mod2valssd <- c(weighted.mean(d[,mod2], d[,wname]) + mod2sd,
                          weighted.mean(d[,mod2], d[,wname]),
                          weighted.mean(d[,mod2], d[,wname]) - mod2sd)
          names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                                 paste("Mean of", mod2),
                                 paste("Mean of", mod2, "-1 SD"))

        }

        mod2vals2 <- mod2valssd
        mod2vals2 <- sort(mod2vals2, decreasing = F)

      } else if (survey == TRUE) {

        mod2sd <- svysd(as.formula(paste("~", mod2, sep = "")), design = design)
        mod2mean <- survey::svymean(as.formula(paste("~", mod2, sep = "")), design = design)
        mod2valssd <- c(mod2mean + mod2sd, mod2mean, mod2mean - mod2sd)
        names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                               paste("Mean of", mod2),
                               paste("Mean of", mod2, "-1 SD"))
        mod2vals2 <- mod2valssd
        mod2vals2 <- sort(mod2vals2, decreasing = F)

      }

    } else if (!is.factor(d[,mod2]) && class(mod2vals) == "character" &&
               mod2vals == "plus-minus") {

      if (survey == FALSE) {

        if (weights == FALSE) {

          mod2sd <- sd(d[,mod2])
          mod2valssd <- c(mean(d[,mod2]) + mod2sd,
                          mean(d[,mod2]) - mod2sd)
          names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                                 paste("Mean of", mod2, "-1 SD"))

        } else {

          mod2sd <- wtd.sd(d[,mod2])
          mod2valssd <- c(weighted.mean(d[,mod2], d[,wname]) + mod2sd,
                          weighted.mean(d[,mod2], d[,wname]) - mod2sd)
          names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                                 paste("Mean of", mod2, "-1 SD"))

        }

          mod2vals2 <- mod2valssd
          mod2vals2 <- sort(mod2vals2, decreasing = F)

      } else if (survey == TRUE) {

        mod2sd <- svysd(as.formula(paste("~", mod2, sep = "")), design = design)
        mod2mean <- survey::svymean(as.formula(paste("~", mod2, sep = "")), design = design)
        mod2valssd <- c(mod2mean+mod2sd, mod2mean-mod2sd)
        names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                               paste("Mean of", mod2, "-1 SD"))
        mod2vals2 <- mod2valssd
        mod2vals2 <- sort(mod2vals2, decreasing = F)

      }

    } else if (is.null(mod2vals) && !is.factor(d[,mod2]) &&
               length(unique(d[,mod2])) == 2) {

      mod2vals2 <- as.numeric(levels(factor(d[,mod2])))

    } else if (is.null(mod2vals) && is.factor(d[,mod2])) {

      mod2vals2 <- levels(d[,mod2])

    } else { # Use user-supplied values otherwise

      mod2vals2 <- sort(mod2vals, decreasing = F)

    }

  }

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
  xpreds <- seq(from=range(d[!is.na(d[,pred]),pred])[1],
                to=range(d[!is.na(d[,pred]),pred])[2],
                length.out=100)
  xpreds <- rep(xpreds, length(modxvals2))

  # Create values of moderator for use in predict()
  facs <- rep(modxvals2[1],100)

  # Looping here allows for a theoretically limitless amount of moderator values
  for (i in 2:length(modxvals2)){
    facs <- c(facs, rep(modxvals2[i], 100))
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
      pm <- matrix(rep(0, 100*(nc+2)*length(modxvals2)), ncol=(nc+2))
    } else {
      pm <- matrix(rep(0, (nc+2)*length(facs)), ncol=(nc+2))
    }
  } else {
    if (is.null(mod2)) {
      pm <- matrix(rep(0, 100*(nc)*length(modxvals2)), ncol=(nc))
    } else {
      pm <- matrix(rep(0, (nc)*length(facs)), ncol=(nc))
    }
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
  # Add values of moderator to df
  pm[,modx] <- facs
  if (!is.null(mod2)){ # if second moderator
    pm[,mod2] <- facs2
  }

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

  # Create predicted values based on specified levels of the moderator, focal predictor

  ## Don't update model if no vars were centered
  if (!is.null(centered) && centered == "none") {
    modelu <- model
  } else {
    if (survey == FALSE) {
      if (mixed == FALSE) {
        modelu <- update(model, data = d)
      } else {
        optimiz <- model@optinfo$optimizer
        if (class(model) == "glmerMod") {
          modelu <- update(model, data = d,
                           control = lme4::glmerControl(optimizer = optimiz,
                                                        calc.derivs = FALSE))
        } else {
          modelu <- update(model, data = d,
                           control = lme4::lmerControl(optimizer = optimiz,
                                                        calc.derivs = FALSE))
        }
      }
    } else {
      # Have to do all this to avoid adding survey to dependencies
      call <- getCall(model)
      call$design <- design
      call[[1]] <- survey::svyglm
      modelu <- eval(call)
    }
  }

  if (mixed == TRUE) {
    predicted <- as.data.frame(predict(modelu, pm,
                                       type = outcome.scale,
                                       allow.new.levels = F,
                                       re.form = ~0))
  } else {
    predicted <- as.data.frame(predict(modelu, pm, se.fit=T,
                                       interval=int.type[1],
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

  # For plotting, it's good to have moderator as factor...
  # but not until after predict() is used
  pm[,modx] <- as.factor(pm[,modx])
  if (!is.null(mod2)) {
    pm[,mod2] <- as.factor(pm[,mod2])
  }

  # Saving x-axis label
  if (is.null(x.label)){
    x.label <- pred
  }

  # Saving y-axis label
  if (is.null(y.label)){
    y.label <- resp
  }

  # Labels for values of moderator
  if (is.null(modx.labels)) {
    if (exists("modxvalssd") && length(modxvalssd)==2){
      pm[,modx] <- factor(pm[,modx], labels=names(sort(modxvals2, decreasing = F)))
    } else if (exists("modxvalssd") && length(modxvalssd)==3){
      pm[,modx] <- factor(pm[,modx], labels=names(sort(modxvals2, decreasing = F)))
    } else if (!is.factor(d[,modx])) {
      labs <- as.character(modxvals2)
      names(modxvals2) <- labs
      pm[,modx] <- factor(pm[,modx], labels = names(sort(modxvals2, decreasing = F)))
    } else if (is.factor(d[,modx])) {
      pm[,modx] <- factor(pm[,modx], levels = modxvals2)
    }
  } else if (length(modx.labels) == length(modxvals2)) {
    if (!is.factor(d[,modx])) {
      pm[,modx] <- factor(pm[,modx], labels = modx.labels)
    } else if (is.factor(d[,modx])) {
      pm[,modx] <- factor(pm[,modx], levels = modxvals2, labels = modx.labels)
    }
  } else {warning("modx.labels argument was not the same length as modxvals. Ignoring...")}


  # Setting labels for second moderator
  if (!is.null(mod2)) {
    if (is.null(mod2.labels)) {
      if (exists("mod2valssd") && length(mod2valssd)==2){
        pm[,mod2] <- factor(pm[,mod2], labels=names(sort(mod2vals2, decreasing = F)))
      } else if (exists("mod2valssd") && length(mod2valssd)==3){
        pm[,mod2] <- factor(pm[,mod2], labels=names(sort(mod2vals2, decreasing = F)))
      } else if (!is.factor(d[,mod2])) {
        labs <- as.character(mod2vals2)
        names(mod2vals2) <- labs
        pm[,mod2] <- factor(pm[,mod2], labels = names(sort(mod2vals2, decreasing = F)))
      } else if (is.factor(d[,mod2])) {
        pm[,mod2] <- factor(pm[,mod2], levels = mod2vals2)
      }
    } else if (length(mod2.labels) == length(mod2vals2)) {
      if (!is.factor(d[,mod2])) {
        pm[,mod2] <- factor(pm[,mod2], labels = mod2.labels)
      } else if (is.factor(d[,mod2])) {
        pm[,mod2] <- factor(pm[,mod2], levels = mod2vals2, labels = mod2.labels)
      }
    } else {warning("mod2.labels argument was not the same length as modxvals. Ignoring...")}
  }

  # If no user-supplied legend title, set it to name of moderator
  if (is.null(legend.main)){
    legend.main <- modx
  }

  # Get palette from RColorBrewer myself so I can use darker values
  colors <- RColorBrewer::brewer.pal((length(modxvals2)+1), color.class)
  colors <- rev(colors)
  names(colors) <- c(names(modxvals2))

  # Defining linetype here
  if (vary.lty == TRUE) {
    p <- ggplot2::ggplot(pm, ggplot2::aes(x=pm[,pred], y=pm[,resp], colour=pm[,modx],
                                          group=pm[,modx], linetype = pm[,modx]))
  } else {
    p <- ggplot2::ggplot(pm, ggplot2::aes(x=pm[,pred], y=pm[,resp], colour=pm[,modx],
                                          group=pm[,modx]))
  }

  # Define line thickness
  p <- p + ggplot2::geom_path(size = line.thickness)

  # Plot intervals if requested
  if (interval==TRUE) {
    p <- p + ggplot2::geom_ribbon(data=pm, ggplot2::aes(ymin=pm[,"ymin"],
                                                        ymax=pm[,"ymax"],
                                                        fill = pm[,modx],
                                                        colour = NA),
                                  alpha=1/5, show.legend = FALSE)
    if (facmod == TRUE) {
      p <- p + ggplot2::scale_fill_brewer(palette = color.class)
    } else {
      p <- p + ggplot2::scale_fill_manual(values = colors, breaks = levels(pm[,modx]))
    }
  }

  # If third mod, facet by third mod
  if (!is.null(mod2)) {
    p <- p + ggplot2::facet_grid(. ~ pm[,mod2])
  }

  # For factor vars, plotting the observed points and coloring them by factor looks great
  if (plot.points==TRUE && is.factor(d[,modx])) {
    p <- p + ggplot2::geom_point(data=d, ggplot2::aes(x=d[,pred],y=d[,resp],
                                                      colour=d[,modx]),
                                 position = "jitter", inherit.aes = F, show.legend = F)
  } else if (plot.points == TRUE && !is.factor(d[,modx])) { # otherwise just black points
    p <- p + ggplot2::geom_point(data=d, ggplot2::aes(x=d[,pred], y=d[,resp]),
                                 inherit.aes = F, position = "jitter")
  }

  # Using theme_apa for theming...but using legend title and side positioning
  if (is.null(mod2)) {
    p <- p + theme_apa(legend.pos = "right", legend.use.title = TRUE)
  } else { # make better use of space by putting legend on bottom for facet plots
    p <- p + theme_apa(legend.pos = "bottom", legend.use.title = TRUE,
                       facet.title.size = 8)
  }
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

  # Get scale colors, provide better legend title
  if (facmod == TRUE) {
    p <- p + ggplot2::scale_colour_brewer(name = legend.main, palette = color.class)
  } else {
    p <- p + ggplot2::scale_colour_manual(name = legend.main, values = colors, breaks = pm[,modx])
  }

  if (vary.lty == TRUE) {# Add line-specific changes
    if (facmod == FALSE) {
      p <- p + ggplot2::scale_linetype_discrete(name = legend.main, breaks = pm[,modx])
    } else {
      p <- p + ggplot2::scale_linetype_discrete(name = legend.main)
    }
    # Need some extra width to show the linetype pattern fully
    p <- p + ggplot2::theme(legend.key.width = grid::unit(2, "lines"))
  }

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)){
    p <- p + ggplot2::ggtitle(main.title)
  }

  # Return the plot
  return(p)

}

#' @export

print.interact_plot <- function(x, ...) {
  print(x)
}
