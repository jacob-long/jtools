#' Scale variables in fitted regression models
#'
#' \code{scale_lm()} takes fitted regression models and scales all predictors
#' by dividing each by 1 or 2 standard deviations (as chosen by the user).
#'
#' @param model A regression model of type \code{lm}, \code{glm}, or
#' \code{\link[survey]{svyglm}}.
#'
#' @param binary.inputs Options for binary variables. Default is \code{"0/1"};
#'   \code{"0/1"} keeps original scale; \code{"-0.5,0.5"} rescales 0 as -0.5 and
#'    1 as 0.5; \code{center} substracts the mean; and \code{full} treats them
#'   like other continuous variables.
#'
#' @param n.sd How many standard deviations should you divide by for
#'   standardization? Default is 1, though some prefer 2.
#'
#' @param center Default is \code{FALSE}. If \code{TRUE}, the predictors are also
#'   mean-centered. For binary predictors, the \code{binary.inputs} argument
#'   supersedes this one.
#'
#' @details This function will scale all continuous variables in a regression
#'   model for ease of interpretation, especially for those models that have
#'   interaction terms. It can also mean-center all of them as well, if requested.
#'
#'   The scaling happens on the input data, not the terms themselves. That means
#'   interaction terms are still properly calculated because they are the product
#'   of standardized predictors, not a standardized product of predictors.
#'
#'   This function re-estimates the model, so for large models one should expect
#'   a runtime equal to the first run.
#'
#' @return The functions returns a \code{lm} or \code{glm} object, inheriting from
#'   whichever class was supplied.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @seealso
#'
#'   \code{\link{gscale}} does the centering behind the scenes.
#'
#'   \code{\link{center_lm}} is a near duplicate, but will only center the
#'   predictors.
#'
#'   \code{\link{sim_slopes}} performs a simple slopes analysis.
#'
#'   \code{\link{interact_plot}} creates attractive, user-configurable plots of
#'   interaction models.
#'
#' @references
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and multilevel
#'  regression: Inferential and graphical techniques. \emph{Multivariate Behavioral
#'  Research}, \emph{40}(3), 373-400.
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied multiple
#' regression/correlation analyses for the behavioral sciences} (3rd ed.).
#' Mahwah, NJ: Lawerence Erlbaum Associates, Inc.
#'
#' @examples
#'
#' fit <- lm(formula = Murder ~ Income * Illiteracy, data = as.data.frame(state.x77))
#' fit_scale <- scale_lm(fit)
#' fit_scale <- scale_lm(fit, center = TRUE)
#'
#' # With weights
#' fitw <- lm(formula = Murder ~ Income * Illiteracy,
#'            data = as.data.frame(state.x77),
#'            weights = Population)
#' fitw_scale <- scale_lm(fitw)
#' fitw_scale <- scale_lm(fitw, center = TRUE, binary.input = "0/1")
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell*meals,design=dstrat)
#' regmodel_scale <- scale_lm(regmodel)
#' regmodel_scale <- scale_lm(regmodel, binary.input = "0/1")
#'
#' @importFrom stats weighted.mean as.formula
#' @export scale_lm
#'

scale_lm <- function(model, binary.inputs = "0/1", n.sd = 1, center = FALSE) {
  # Save data
  d <- model.frame(model)

  # svyglm?
  if (class(model)[1]=="svyglm" || class(model)[1]=="svrepglm") {survey <- TRUE} else {survey <- FALSE}

  # things are different for these svyglm objects...
  if (survey == TRUE) {

    # Otherwise update() won't work
    requireNamespace(survey)

    # Get the survey design object
    design <- model$survey.design

    # Now we need to know the variables of interest
    vars <- attributes(model$terms)$variables
    vars <- as.character(vars)[2:length(vars)]

    # Call gscale()
    if (center == FALSE) {
      design <- gscale(x = vars, data = design, scale.only = TRUE, n.sd = n.sd)
    } else if (center == TRUE) {
      design <- gscale(x = vars, data = design, n.sd = n.sd)
    }

    # Update the model
    new <- update(model, design = design)
  }


  # weights?
  if (survey == FALSE && "(weights)" %in% names(d)) {
    weights <- TRUE
    theweights <- d$`(weights)`
    wname <- sub("()", model$call["weights"], "")
    colnames(d)[which(colnames(d) == "(weights)")] <- wname
  } else {
    weights <- FALSE
  }

  # calling gscale(), incorporating the weights
  if (weights == TRUE) {
    varnames <- names(d)[!(names(d) %in% wname)]
    d <- gscale(x = varnames, data = d, binary.inputs = binary.inputs,
                scale.only = !center, weights = theweights, n.sd = n.sd)
  } else {
    d <- gscale(data = d, binary.inputs = binary.inputs,
                scale.only = !center, n.sd = n.sd)
  }


  if (survey == FALSE) {
    # Update model
    new <- update(model, data = d)
  }
  return(new)
}

