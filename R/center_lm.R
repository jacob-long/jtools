#' Center variables in fitted regression models
#'
#' \code{center_lm()} takes fitted regression models and mean-centers the continous
#'   variables in the model to aid interpretation, especially in the case of models
#'   with interactions.
#'
#' @param model A regression model of type \code{lm}, \code{glm}, or
#' \code{\link[survey]{svyglm}}. It should contain the interaction(s) of interest,
#' if any.
#'
#' @details This function will mean-center all continuous variables in a regression
#'   model for ease of interpretation, especially for those models that have
#'   interaction terms.
#'
#' @return The functions returns a \code{lm} or \code{glm} object, inheriting from
#'   whichever class was supplied.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @seealso
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
#' fit_center <- center_lm(fit)
#'
#' # With weights
#' fitw <- lm(formula = Murder ~ Income * Illiteracy, data = as.data.frame(state.x77),
#'    weights = Population)
#' fitw_center <- center_lm(fitw)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell*meals,design=dstrat)
#' regmodel_center <- center_lm(regmodel)
#'
#' @importFrom stats weighted.mean
#' @export center_lm
#'

center_lm <- function(model) {
  # Save data
  d <- model.frame(model)

  # svyglm?
  if (class(model)[1]=="svyglm" || class(model)[1]=="svrepglm") {survey <- TRUE} else {survey <- FALSE}

  # things are different for these svyglm objects...
  if (survey == TRUE) {

    # Get the survey design object
    design <- model$survey.design
    # Get the DF from it
    d <- design$variables

    # Now we need to know the variables of interest
    vars <- attributes(model$terms)$variables
    vars <- as.character(vars)

    # Subtract means
    for (i in 1:ncol(d)) {
      if (names(d)[i] %in% vars && !is.factor(d[,i])) {
        vmean <- survey::svymean(as.formula(paste("~", names(d)[i])), design = design)
        d[,i] <- d[,i] - vmean
      }
    }

    # Update the values with mean-centered version
    design$variables <- d

    new <- update(model, design = design)
  }


  # weights?
  if (survey == FALSE && "(weights)" %in% names(d)) {
    weights <- TRUE
    wname <- sub("()", model$call["weights"], "")
    colnames(d)[which(colnames(d) == "(weights)")] <- wname
  } else {
    weights <- FALSE
  }

  # mean center
  if (survey == FALSE && weights == FALSE) {
    for (i in 1:ncol(d)) {
      if (!is.factor(d[,i]) && colnames(d)[i] != "(weights)") {
        d[,i] <- d[,i] - mean(d[,i])
      }
    }
  } else if (weights == TRUE) {
    for (i in 1:ncol(d)) {
      if (!is.factor(d[,i]) && colnames(d)[i] != wname) {
        d[,i] <- d[,i] - weighted.mean(d[,i], d[,wname])
      }
    }
  }

  if (survey == FALSE) {
    new <- update(model, data)
  }
  return(new)
}

