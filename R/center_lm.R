#' Center variables in fitted regression models
#'
#' \code{center_lm()} takes fitted regression models and mean-centers the continuous
#'   variables in the model to aid interpretation, especially in the case of models
#'   with interactions.
#'
#' @param model A regression model of type \code{lm}, \code{glm}, or
#' \code{\link[survey]{svyglm}}. It should contain the interaction(s) of interest,
#' if any.
#'
#' @param binary.inputs Options for binary variables. Default is \code{0/1};
#'   \code{0/1} keeps original scale; \code{-0.5,0.5} rescales 0 as -0.5 and 1 as 0.5;
#'   \code{center} subtracts the mean; and \code{full} treats them like other
#'   continuous variables.
#'
#' @param center.response Should the response variable also be centered? Default
#'   is \code{FALSE}.
#'
#' @details This function will mean-center all continuous variables in a regression
#'   model for ease of interpretation, especially for those models that have
#'   interaction terms. The mean for \code{svyglm} objects is calculated using
#'   \code{svymean}, so reflects the survey-weighted mean. The weight variables
#'   in \code{svyglm} are not centered, nor are they in other \code{lm} family
#'   models.
#'
#'   This function re-estimates the model, so for large models one should expect
#'   a runtime equal to the first run.
#'
#' @return The functions returns a \code{lm} or \code{glm} object, inheriting from
#'   whichever class was supplied.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @family standardization, scaling, and centering tools
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
#' Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' @examples
#'
#' fit <- lm(formula = Murder ~ Income * Illiteracy, data = as.data.frame(state.x77))
#' fit_center <- center_lm(fit)
#'
#' # With weights
#' fitw <- lm(formula = Murder ~ Income * Illiteracy,
#'            data = as.data.frame(state.x77),
#'            weights = Population)
#' fitw_center <- center_lm(fitw)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell*meals,design=dstrat)
#' regmodel_center <- center_lm(regmodel)
#'
#' @importFrom stats weighted.mean as.formula getCall formula
#' @importFrom stats model.matrix model.weights
#'
#' @export center_lm
#'

center_lm <- function(model, binary.inputs = "0/1", center.response = FALSE) {

  # Save data --- using the call to access the data to avoid problems w/
  # transformed data
  call <- getCall(model)

  mm <- model.matrix(model)
  mf <- model.frame(model)
  form <- formula(model)
  formc <- as.character(deparse(formula(model)))

  # Detect presence of offset, save the vector
  if (!is.null(model.offset(mf))) {

    offset <- TRUE
    the_offset <- model.offset(mf)

  } else {

    offset <- FALSE
    the_offset <- NULL

  }

  # Save response variable
  resp <- as.character(formula(model)[2])

  # Save list of terms
  vars <- attributes(terms(model))$variables
  vars <- as.character(vars)[2:length(vars)] # Use 2:length bc 1 is "list"

  # If offset supplied in the formula, detect it, delete it
  if (!is.null(attr(terms(form), "offset"))) {

    off_pos <- attr(terms(form), "offset") # Get index of offset in terms list
    offname <- attr(terms(form), "variables")[off_pos] # Get its name in list
    formc <- gsub(offname, "", formc)

  }

  # Now I'm quoting all the names so there's no choking on vars with functions
  # applied. I save the backticked names
  for (var in vars) {

    regex_pattern <- paste("(?<=(~|\\s|\\*|\\+))", escapeRegex(var),
                           "(?=($|~|\\s|\\*|\\+))", sep = "")
    backtick_name <- paste("`", var, "`", sep = "")
    formc <- gsub(regex_pattern, backtick_name, formc, perl = T)

  }

  # svyglm?
  if (class(model)[1] == "svyglm" || class(model)[1] == "svrepglm") {
    survey <- TRUE
  } else {
    survey <- FALSE
  }

  if (!is.null(model.weights(mf)) && survey == FALSE) {

    weights <- TRUE
    the_weights <- model.weights(mf)

  } else {

    weights <- FALSE
    the_weights <- NULL

  }

  # things are different for these svyglm objects...
  if (survey == TRUE) {

    # Get the survey design object
    design <- model$survey.design

    # Now we need to know the variables of interest
    vars <- attributes(model$terms)$variables
    vars <- as.character(vars)[2:length(vars)]

    # Add vars to design if they aren't already there (fixes issues with functions)
    adds <- which(!(vars %in% names(design$variables)))
    for (var in vars[adds]) {

      design[,var] <- mf[,var]

    }

    if (center.response == FALSE) {
      vars <- vars[!(vars %in% resp)]
    }

    # Call gscale()
    design <- gscale(x = vars, data = design, center.only = TRUE)

    call$design <- design
    call[[1]] <- survey::svyglm
    new <- eval(call)

  }

  # weights?
  if (survey == FALSE && !is.null(call$weights)) {

    weights <- TRUE
    mf <- mf[,names(mf) != "(weights)"] # just getting rid of the column

  } else {

    weights <- FALSE

  }

  # calling gscale(), incorporating the weights
  if (weights == TRUE) {

    if (center.response == FALSE) {

      vars <- vars[!(vars %in% resp)]

    }

    mf <- gscale(x = vars, data = mf, binary.inputs = binary.inputs,
                center.only = TRUE, weights = the_weights)

    mf$the_weights <- the_weights

  } else {

    if (center.response == FALSE) {
      # Now we need to know the variables of interest
      vars <- vars[!(vars %in% resp)]
      mf <- gscale(x = vars, data = mf, binary.inputs = binary.inputs,
                  center.only = TRUE)

    } else {

      mf <- gscale(x = vars, data = mf, binary.inputs = binary.inputs,
                  center.only = TRUE)

    }

  }


  if (survey == FALSE) {

    if (offset == TRUE && weights == FALSE) {

      mf$the_offset <- the_offset

      new <- j_update(model, formula = as.formula(formc),
                    offset = the_offset, data = mf)

    } else if (weights == TRUE && offset == FALSE) {

      new <- j_update(model, formula = as.formula(formc),
                    weights = the_weights, data = mf)

    } else if (weights == TRUE && offset == TRUE) {

      mf$the_offset <- the_offset

      new <- j_update(model, formula = as.formula(formc),
                    weights = the_weights, offset = the_offset, data = mf)

    } else {

      new <- j_update(model, formula = as.formula(formc), data = mf)

    }

  }


  return(new)

}

