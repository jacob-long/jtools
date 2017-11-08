#' Plot interaction effects in regression models
#'
#' \code{interact_plot()} plots regression lines at user-specified levels of a
#'  moderator variable to explore interactions. The plotting is done with
#'  \code{ggplot2} rather than base graphics, which some similar functions use.
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
#'   deviation without the mean. You may also choose `"terciles"` to split
#'   the data into equally-sized groups and choose the point at the mean of
#'   each of those groups.
#'
#' @param mod2vals For which values of the second moderator should the plot be
#'   facetted by? That is, there will be a separate plot for each level of this
#'   moderator. Defaults are the same as \code{modxvals}.
#'
#' @param centered A vector of quoted variable names that are to be
#'   mean-centered. If \code{NULL}, all non-focal predictors are centered. If
#'   not \code{NULL}, only the user-specified predictors are centered. User can
#'   also use "none" or "all" arguments. The response variable is not centered
#'   unless specified directly.
#'
#' @param scale Logical. Would you like to standardize the variables
#'   that are centered? Default is \code{FALSE}, but if \code{TRUE} it will
#'   standardize variables specified by the \code{centered} argument. Note that
#'   non-focal predictors are centered when \code{centered = NULL}, its
#'   default.
#'
#' @param n.sd How many standard deviations should be used if \code{scale
#'   = TRUE}? Default is 1, but some prefer 2.
#'
#' @param plot.points Logical. If \code{TRUE}, plots the actual data points as a
#'   scatterplot on top of the interaction lines. The color of the dots will be
#'   based on their moderator value.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction
#'   intervals around the line using \code{\link[ggplot2]{geom_ribbon}}. Not
#'   supported for \code{merMod} models.
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
#' @param linearity.check For two-way interactions only. If `TRUE`, plots a
#'   pane for each level of the moderator and superimposes a loess smoothed
#'   line (in gray) over the plot. This enables you to see if the effect is
#'   linear through the span of the moderator. See Hainmuller et al. (2016) in
#'   the references for more details on the intuition behind this. It is
#'   recommended that you also set `plot.points = TRUE` and use
#'   `modxvals = "terciles"` with this option.
#'
#' @param set.offset For models with an offset (e.g., Poisson models), sets a
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
#' @param pred.labels A character vector of 2 labels for the predictor if it is
#'   a 2-level factor or a continuous variable with only 2 values. If
#'   \code{NULL}, the default, the factor labels are used.
#'
#' @param modx.labels A character vector of labels for each level of the
#'   moderator values, provided in the same order as the \code{modxvals}
#'   argument. If \code{NULL}, the values themselves are used as labels unless
#'   \code{modxvals} is also \code{NULL}. In that case, "+1 SD" and "-1 SD"
#'   are used.
#'
#' @param mod2.labels A character vector of labels for each level of the 2nd
#'   moderator values, provided in the same order as the \code{mod2vals}
#'   argument. If \code{NULL}, the values themselves are used as labels unless
#'   \code{mod2vals} is also \code{NULL}. In that case, "+1 SD" and "-1 SD"
#'   are used.
#'
#' @param main.title A character object that will be used as an overall title
#'   for the plot. If \code{NULL}, no main title is used.
#'
#' @param legend.main A character object that will be used as the title that
#'   appears above the legend. If \code{NULL}, the name of the moderating
#'   variable is used.
#'
#' @param color.class Any palette argument accepted by
#'   \code{\link[ggplot2]{scale_colour_brewer}}. Default is "Set2" for factor
#'    moderators, "Blues" for +/- SD and user-specified \code{modxvals} values.
#'
#' @param line.thickness How thick should the plotted lines be? Default is 1.1;
#'   ggplot's default is 1.
#'
#' @param vary.lty Should the resulting plot have different shapes for each
#'   line in addition to colors? Defaults to \code{TRUE}.
#'
#' @param standardize Deprecated. Equivalent to `scale`. Please change your
#'  scripts to use `scale` instead as this argument will be removed in the
#'  future.
#'
#' @details This function provides a means for plotting conditional effects
#'   for the purpose of exploring interactions in the context of regression.
#'   You must have the
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
#'   Note: to use transformed predictors, e.g., \code{log(variable)},
#'   put its name in quotes or backticks in the argument.
#'
#'   \emph{Details on how observed data are split in multi-pane plots}:
#'
#'   If you set `plot.points = TRUE` and request a multi-pane (facetted) plot
#'   either with a second moderator or `linearity.check = TRUE`, the observed
#'   data are split into as many groups as there  arepanes and plotted
#'   separately. If the moderator is a factor, then the way this happens will
#'   be very intuitive since it's obvious which values go in which pane. The
#'   rest of this section will address the case of continuous moderators.
#'
#'   My recommendation is that you use `modxvals = "terciles"`` or
#'   `mod2vals = "terciles"` when you want to plot observed data on multi-pane
#'   plots. When you do, the data are split into three approximately
#'   equal-sized groups with the lowest third, middle third, and highest third
#'   of the data split accordingly. You can replicate this procedure using
#'   [Hmisc::cut2()] with `g = 3` from the `Hmisc` package. Sometimes, the
#'   groups will not be equal in size because the number of observations is
#'   not divisible by 3 and/or there are multiple observations with the same
#'   value at one of the cut points.
#'
#'   Otherwise, a more ad hoc procedure is used to split the data. Quantiles
#'   are found for each `mod2vals` or `modxvals` value. These are not the
#'   quantiles used to split the data, however, since we want the plotted lines
#'   to represent the slope at a typical value in the group. The next step,
#'   then, is to take the mean of each pair of neighboring quantiles and use
#'   these as the cut points.
#'
#'   For example, if the `mod2vals` are at the 25th, 50th, and 75th percentiles
#'   of the distribution of the moderator, the data will be split at the
#'   37.5th and and 62.5th percentiles. When the variable is
#'   normally distributed, this will correspond fairly closely to using
#'   terciles.
#'
#'   \emph{Info about offsets:}
#'
#'   Offsets are partially supported by this function with important
#'   limitations. First of all, only a single offset per model is supported.
#'   Second, it is best in general to specify offsets with the offset argument
#'   of the model fitting function rather than in the formula. If it is
#'   specified in the formula with a svyglm, this function will stop with an
#'   error message.
#'
#'   It is also advised not to do any transformations to the offset other than
#'   the common log transformation. If you apply a log transform, this function
#'   will deal with it sensibly. So if your offset is a logged count, the
#'   exposure you set will be the non-logged version, which is much easeir to
#'   wrap one's head around. For any other transformation you may apply, or
#'   if you apply no transformation at all, the exposures used will be the
#'   post-tranformation number (which is by default 1).
#'
#' @return The functions returns a \code{ggplot} object, which can be treated
#'   like a user-created plot and expanded upon as such.
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
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#'  multilevel regression: Inferential and graphical techniques.
#'  \emph{Multivariate Behavioral
#'  Research}, \emph{40}(3), 373-400.
#'  \url{http://dx.doi.org/10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple
#' regression/correlation analyses for the behavioral sciences} (3rd ed.).
#' Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' Hainmueller, J., Mummolo, J., & Xu, Y. (2016). How much should we trust
#'   estimates from multiplicative interaction models? Simple tools to improve
#'   empirical practice. SSRN Electronic Journal.
#'   \url{https://doi.org/10.2139/ssrn.2739221}

#'
#' @examples
#' # Using a fitted lm model
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder * Illiteracy,
#'   data = states)
#' interact_plot(model = fit, pred = Murder,
#'   modx = Illiteracy)
#'
#' # Using interval feature
#' fit <- lm(accel ~ mag * dist, data = attenu)
#' interact_plot(fit, pred = mag, modx = dist, interval = TRUE,
#'   int.type = "confidence", int.width = .8)
#'
#' # Using second moderator
#' fit <- lm(Income ~ HSGrad * Murder * Illiteracy,
#'   data = states)
#' interact_plot(model = fit, pred = Murder,
#'   modx = Illiteracy, mod2 = HSGrad)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#' interact_plot(regmodel, pred = ell, modx = meals)
#'
#' # With lme4
#' \dontrun{
#' library(lme4)
#' data(VerbAgg)
#' mv <- glmer(r2 ~ Anger * mode + (1 | item), data = VerbAgg,
#'             family = binomial,
#'             control = glmerControl("bobyqa"))
#' interact_plot(mv, pred = Anger, modx = mode)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median ecdf quantile
#' @import ggplot2
#' @export interact_plot

interact_plot <- function(model, pred, modx, modxvals = NULL, mod2 = NULL,
                          mod2vals = NULL, centered = NULL, scale = FALSE,
                          n.sd = 1, plot.points = FALSE, interval = FALSE,
                          int.type = c("confidence","prediction"),
                          int.width = .95, outcome.scale = "response",
                          linearity.check = FALSE,
                          set.offset = 1,
                          x.label = NULL, y.label = NULL,
                          pred.labels = NULL, modx.labels = NULL,
                          mod2.labels = NULL, main.title = NULL,
                          legend.main = NULL, color.class = NULL,
                          line.thickness = 1.1, vary.lty = TRUE,
                          standardize = NULL) {

  # Evaluate the modx, mod2, pred args
  pred <- as.character(substitute(pred))
  modx <- as.character(substitute(modx))
  mod2 <- as.character(substitute(mod2))
  # To avoid unexpected behavior, need to un-un-parse mod2 when it is NULL
  if (length(mod2) == 0) {
    mod2 <- NULL
  }

  # Check for deprecated argument
  if (!is.null(standardize)) {
    warning("The standardize argument is deprecated. Please use 'scale'",
      " instead.")
    scale <- standardize
  }

  # Duplicating the dataframe so it can be manipulated as needed
  d <- model.frame(model)

  # Is it a svyglm?
  if (class(model)[1] == "svyglm" || class(model)[1] == "svrepglm") {
    survey <- TRUE
    mixed <- FALSE
    design <- model$survey.design
    d <- design$variables

    wts <- weights(design) # for use with points.plot aesthetic

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
        warning("Confidence intervals cannot be provided for random effects",
                " models.")
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
    if (survey == FALSE) {wts <- rep(1, times = nrow(d))}

  }

  # offset?
  if (!is.null(model.offset(model.frame(model)))) {

    off <- TRUE
    offname <- as.character(getCall(model)$offset[-1]) # subset gives bare name

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

  # Setting default for colors
  if (is.factor(d[,modx])) {
    facmod <- TRUE
    if (is.null(color.class)) {
      color.class <- "Set2"
    }
    # Unrelated, but good place to throw a warning
    if (!is.null(modxvals)) {
      warning("All levels of factor must be used. Ignoring modxvals",
              " argument...")
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
  formula <- paste(formula[2], formula[1], formula[3])

  # Pulling the name of the response variable for labeling
  resp <- sub("(.*)(?=~).*", x = formula, perl = TRUE, replacement = "\\1")
  resp <- trimws(resp)

### Centering ##################################################################

  # Update facvars by pulling out all non-focals
  facvars <-
    facvars[facvars %nin% c(pred, resp, modx, mod2, wname, offname)]

  # Use utility function shared by all interaction functions
  c_out <- center_vals(d = d, weights = wts, facvars = facvars,
                       fvars = fvars, pred = pred,
                       resp = resp, modx = modx, survey = survey,
                       design = design, mod2 = mod2, wname = wname,
                       offname = offname, centered = centered,
                       scale = scale, n.sd = n.sd)

  design <- c_out$design
  d <- c_out$d
  fvars <- c_out$fvars
  facvars <- c_out$facvars

### Getting moderator values ##################################################

  modxvals2 <- mod_vals(d, modx, modxvals, survey, wts, design,
                        modx.labels, any.mod2 = !is.null(mod2))
  modx.labels <- names(modxvals2)

  if (!is.null(mod2)) {

    mod2vals2 <- mod_vals(d, mod2, mod2vals, survey, wts, design,
                          mod2.labels, any.mod2 = !is.null(mod2),
                          is.mod2 = TRUE)
    mod2.labels <- names(mod2vals2)

  }

### 2-level factor as predictor ###############################################

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
    stop("Focal predictor (\"pred\") cannot have more than two levels. Either",
         " use it as modx or convert it to a continuous or single dummy",
         " variable.")
  } else {
    predfac <- FALSE
  }

### Prep original data for splitting into groups ##############################

  # Only do this if going to plot points
  if ((!is.null(mod2) | linearity.check == TRUE) & !is.factor(d[[modx]]) &
      plot.points == TRUE) {

    # Use ecdf function to get quantile of the modxvals
    mod_val_qs <- ecdf(d[[modx]])(sort(modxvals2))

    # Now I am going to split the data in a way that roughly puts each modxval
    # in the middle of each group. mod_val_qs is a vector of quantiles for each
    # modxval, so I will now build a vector of the midpoint between each
    # neighboring pair of quantiles — they will become the cutpoints for
    # splitting the data into groups that roughly correspond to the modxvals
    cut_points <- c() # empty vector
    # Iterate to allow this to work regardless of number of modxvals
    for (i in 1:(length(modxvals2) - 1)) {

      cut_points <- c(cut_points, mean(mod_val_qs[i:(i + 1)]))

    }

    # Add Inf to both ends to encompass all values outside the cut points
    cut_points <- c(-Inf, quantile(d[[modx]], cut_points), Inf)

    # Create variable storing this info as a factor
    d$modx_group <- cut(d[[modx]], cut_points, labels = names(sort(modxvals2)))

    if (!is.null(modxvals) && modxvals == "terciles") {
      d$modx_group <- factor(cut2(d[[modx]], g = 3, levels.mean = TRUE),
                             labels = c("Lower tercile", "Middle tercile",
                                        "Upper tercile"))
    }

  }

  if (!is.null(mod2) && !is.factor(d[[mod2]]) & plot.points == TRUE) {

    mod_val_qs <- ecdf(d[[mod2]])(sort(mod2vals2))


    cut_points2 <- c()
    for (i in 1:(length(mod2vals2) - 1)) {

      cut_points2 <- c(cut_points2, mean(mod_val_qs[i:(i + 1)]))

    }

    cut_points2 <- c(-Inf, quantile(d[[mod2]], cut_points2), Inf)

    d$mod2_group <- cut(d[[mod2]], cut_points2, labels = names(sort(mod2vals2)))

    if (!is.null(mod2vals) && mod2vals == "terciles") {
      d$mod2_group <- factor(cut2(d[[mod2]], g = 3, levels.mean = TRUE),
                             labels = c(paste("Lower tercile of", mod2),
                                        paste("Middle tercile of", mod2),
                                        paste("Upper tercile of", mod2)))
    }

  }

#### Creating predicted frame #################################################

  # Creating a set of dummy values of the focal predictor for use in predict()
  xpreds <- seq(from = range(d[!is.na(d[,pred]),pred])[1],
                to = range(d[!is.na(d[,pred]),pred])[2],
                length.out = 100)
  xpreds <- rep(xpreds, length(modxvals2))

  # Create values of moderator for use in predict()
  facs <- rep(modxvals2[1],100)

  # Looping here allows for a theoretically limitless amount of
  # moderator values
  for (i in 2:length(modxvals2)) {
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
      pm <- matrix(rep(0, 100 * (nc + 2) * length(modxvals2)), ncol = (nc + 2))
    } else {
      pm <- matrix(rep(0, (nc + 2) * length(facs)), ncol = (nc + 2))
    }
  } else {
    if (is.null(mod2)) {
      pm <- matrix(rep(0, 100 * nc * length(modxvals2)), ncol = nc)
    } else {
      pm <- matrix(rep(0, nc * length(facs)), ncol = nc)
    }
  }

  # Naming columns
  if (interval == TRUE) { # if intervals, name the SE columns
    colnames(pm) <- c(colnames(d)[colnames(d) %nin%
                                    c("modx_group","mod2_group")],
                      "ymax", "ymin")
  } else {
    colnames(pm) <- colnames(d)[colnames(d) %nin% c("modx_group","mod2_group")]
  }
  # Convert to dataframe
  pm <- as.data.frame(pm)
  # Add values of moderator to df
  pm[,modx] <- facs
  if (!is.null(mod2)) { # if second moderator
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

#### Predicting with update models ############################################

  # Create predicted values based on specified levels of the moderator,
  # focal predictor

  ## This is passed to predict(), but for svyglm needs to be TRUE always
  interval_arg <- interval

  # Back-ticking variable names in formula to prevent problems with
  # transformed preds
  formc <- as.character(deparse(formula(model)))
  for (var in all.vars) {

    regex_pattern <- paste("(?<=(~|\\s|\\*|\\+))", escapeRegex(var),
                             "(?=($|~|\\s|\\*|\\+))", sep = "")

    bt_name <- paste("`", var, "`", sep = "")
    formc <- gsub(regex_pattern, bt_name, formc, perl = TRUE)

  }
  form <- as.formula(formc)

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
      interval_arg <- TRUE
      call <- getCall(model)
      call$design <- design
      call$formula <- form
      call[[1]] <- survey::svyglm
      modelu <- eval(call)
    }
  }

  if (mixed == TRUE) {
    predicted <- as.data.frame(predict(modelu, newdata = pm,
                                       type = outcome.scale,
                                       allow.new.levels = F,
                                       re.form = ~0))
  } else {
    predicted <- as.data.frame(predict(modelu, newdata = pm,
                                       se.fit = interval_arg,
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
  if (is.null(x.label)) {
    x.label <- pred
  }

  # Saving y-axis label
  if (is.null(y.label)) {
    y.label <- resp
  }

  # Labels for values of moderator
  pm[,modx] <- factor(pm[,modx], levels = modxvals2, labels = modx.labels)
  pm$modx_group <- pm[,modx]

  # Setting labels for second moderator
  if (!is.null(mod2)) {

    pm[,mod2] <- factor(pm[,mod2], levels = mod2vals2, labels = mod2.labels)

    # Setting mod2 in OG data to the factor if plot.points == TRUE
    if (plot.points == TRUE) {

      d[[mod2]] <- d$mod2_group

    }

  }


#### Plotting #################################################################

  # If no user-supplied legend title, set it to name of moderator
  if (is.null(legend.main)) {
    legend.main <- modx
  }

  # Get palette from RColorBrewer myself so I can use darker values
  colors <- RColorBrewer::brewer.pal((length(modxvals2) + 1), color.class)
  colors <- colors[-1]

  if (is.null(mod2)) {
    colors <- rev(colors)
    pp_color <- first(colors) # Darkest color used for plotting points
  } else {
    pp_color <- last(colors)
  }

  names(colors) <- modx.labels

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
    p <- p + geom_ribbon(aes_string(ymin = "ymin", ymax = "ymax",
                                    fill = modx, group = modx,
                                    colour = modx, linetype = NA),
                                  alpha = 1/5, show.legend = FALSE)
    if (facmod == TRUE) {
      p <- p + scale_fill_brewer(palette = color.class)
    } else {
      p <- p + scale_fill_manual(values = colors,
                                 breaks = levels(pm[,modx]))
    }
  }

  # If third mod, facet by third mod
  if (!is.null(mod2)) {
    facets <- facet_grid(paste(". ~", mod2))
    p <- p + facets
  } else if (linearity.check == TRUE) {
    facets <- facet_grid(paste(". ~ modx_group"))
    p <- p + facets + stat_smooth(data = d, aes_string(x = pred, y = resp),
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
    d[,"the_weights"] <- wts

    if (is.factor(d[,modx])) {
      p <- p + geom_point(data = d, aes_string(x = pred, y = resp,
                          colour = modx, size = "the_weights"),
               position = "jitter", inherit.aes = FALSE, show.legend = FALSE)
    } else if (!is.factor(d[,modx])) {
      # using alpha for same effect with continuous vars
      p <- p + geom_point(data = d,
                          aes_string(x = pred, y = resp, alpha = modx,
                                    size = "the_weights"),
                          colour = pp_color, inherit.aes = FALSE,
                          position = "jitter", show.legend = FALSE) +
          scale_alpha_continuous(range = c(0.25, 1), guide = "none")
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

  # Getting rid of tick marks for factor predictor
  if (predfac == TRUE) {
    if (is.null(pred.labels)) { # Let pred.labels override factor labels
      p <- p + scale_x_continuous(breaks = c(0,1), labels = predlabs)
    } else { # Use the factor labels
      p <- p + scale_x_continuous(breaks = c(0,1), labels = pred.labels)
    }
  } else if (length(unique(d[,pred])) == 2) {
    # Predictor has only two unique values
    # Make sure those values are in increasing order
    brks <- sort(unique(d[,pred]), decreasing = F)
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
  if (facmod == TRUE) {
    p <- p + scale_colour_brewer(name = legend.main, palette = color.class)
  } else {
    p <- p + scale_colour_manual(name = legend.main, values = colors,
                                 breaks = pm[,modx])
  }

  if (vary.lty == TRUE) { # Add line-specific changes
    if (facmod == FALSE) {
      p <- p + scale_linetype_discrete(name = legend.main, breaks = pm[,modx])
    } else {
      p <- p + scale_linetype_discrete(name = legend.main)
    }
    # Need some extra width to show the linetype pattern fully
    p <- p + theme(legend.key.width = grid::unit(2, "lines"))
  }

  # Give the plot the user-specified title if there is one
  if (!is.null(main.title)) {
    p <- p + ggtitle(main.title)
  }

  # Return the plot
  return(p)

}

#' @export

print.interact_plot <- function(x, ...) {
  print(x)
}

#### Effect plot ##############################################################

### So closely related to interact_plot that I'll keep them in one file

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
#' @param centered A vector of quoted variable names that are to be
#'   mean-centered.
#'   If \code{NULL}, all non-focal predictors are centered. If not \code{NULL},
#'   only the user-specified predictors are centered. User can also use "none"
#'   or "all" arguments. The response variable is not centered unless specified
#'   directly.
#'
#' @param scale Logical. Would you like to standardize the variables
#'   that are centered? Default is \code{FALSE}, but if \code{TRUE} it will
#'   standardize variables specified by the \code{centered} argument. Note that
#'   non-focal predictors are centered when \code{centered = NULL}, its
#'   default.
#'
#' @param n.sd How many standard deviations should be used if \code{scale
#'   = TRUE}? Default is 1, but some prefer 2.
#'
#' @param plot.points Logical. If \code{TRUE}, plots the actual data points as a
#'   scatterplot on top of the interaction lines.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction
#'   intervals
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
#' @param standardize Deprecated. Equivalent to `scale`. Please change your
#'  scripts to use `scale` instead as this argument will be removed in the
#'  future.
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
#' @return The functions returns a \code{ggplot} object, which can be treated
#'   like
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
#' mv <- glmer(r2 ~ Anger + mode + (1 | item), data = VerbAgg,
#'             family = binomial,
#'             control = glmerControl("bobyqa"))
#' effect_plot(mv, pred = Anger)
#' }
#'
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median weights
#' @import ggplot2
#' @export effect_plot

effect_plot <- function(model, pred, centered = NULL, scale = FALSE,
                        n.sd = 1, plot.points = FALSE, interval = FALSE,
                        int.type = c("confidence","prediction"),
                        int.width = .95, outcome.scale = "response",
                        set.offset = 1,
                        x.label = NULL, y.label = NULL,
                        pred.labels = NULL, main.title = NULL,
                        color.class = NULL, line.thickness = 1.1,
                        standardize = NULL) {

  # Check for deprecated argument
  if (!is.null(standardize)) {
    warning("The standardize argument is deprecated. Please use 'scale'",
      " instead.")
    scale <- standardize
  }

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

    wts <- weights(design) # for use with points.plot aesthetic

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
    if (survey == FALSE) {wts <- rep(1, times = nrow(d))}

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
                       scale = scale, n.sd = n.sd)

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

  # Back-ticking variable names in formula to prevent problems with
  # transformed preds
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
    d[,"the_weights"] <- wts
      p <- p + geom_point(data = d,
                          aes_string(x = pred, y = resp, size = "the_weights"),
               position = "jitter", inherit.aes = FALSE, show.legend = FALSE)
    # Add size aesthetic to avoid giant points
    # p <- p + scale_size(range = c(0.3, 4))
    p <- p + scale_size_identity()
  }

  # Using theme_apa for theming...but using legend title and side positioning
  p <- p + theme_apa(legend.pos = "right", legend.use.title = TRUE)

  p <- p + labs(x = x.label, y = y.label) # better labels for axes

  # Getting rid of tick marks for factor predictor
  if (predfac == TRUE) {
    if (is.null(pred.labels)) { # Let pred.labels override factor labels
      p <- p + scale_x_continuous(breaks = c(0,1), labels = predlabs)
    } else { # Use the factor labels
      p <- p + scale_x_continuous(breaks = c(0,1), labels = pred.labels)
    }
  } else if (length(unique(d[,pred])) == 2) { # Predictor has only two unique values
    # Make sure those values are in increasing order
    brks <- sort(unique(d[,pred]), decreasing = F)
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

#' @export

print.effect_plot <- function(x, ...) {
  print(x)
}
