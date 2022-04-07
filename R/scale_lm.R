#' @export
#' @rdname scale_mod

scale_mod <- function(model, ...) {

  UseMethod("scale_mod")

}


#' @export
#'

scale_lm <- scale_mod


#' Scale variables in fitted regression models
#'
#' `scale_mod` (previously known as `scale_lm`) takes fitted regression models
#'  and scales all
#'  predictors by dividing each by 1 or 2 standard deviations (as chosen by the
#'  user).
#'
#' @param model A regression model of type \code{lm}, \code{glm},
#' \code{\link[survey]{svyglm}}, or [lme4::merMod]. Other model types
#' may work as well but are not tested.
#'
#' @param binary.inputs Options for binary variables. Default is \code{"0/1"};
#'   \code{"0/1"} keeps original scale; \code{"-0.5,0.5"} rescales 0 as -0.5
#'   and
#'   1 as 0.5; \code{center} subtracts the mean; and \code{full} treats them
#'   like other continuous variables.
#'
#' @param n.sd How many standard deviations should you divide by for
#'   standardization? Default is 1, though some prefer 2.
#'
#' @param center Default is \code{TRUE}. If \code{TRUE}, the predictors are
#'   also
#'   mean-centered. For binary predictors, the \code{binary.inputs} argument
#'   supersedes this one.
#'
#' @param center.only Rather than actually scale predictors, just mean-center
#'   them.
#'
#' @param scale.response Should the response variable also be rescaled? Default
#'   is \code{FALSE}.
#'
#' @param data If you provide the data used to fit the model here, that data
#'   frame is used to re-fit the model instead of the [stats::model.frame()]
#'   of the model. This is particularly useful if you have variable
#'   transformations or polynomial terms specified in the formula.
#'
#' @param vars A character vector of variable names that you want to be
#'   scaled. If NULL, the default, it is all predictors.
#'
#' @param ... Arguments passed on to [gscale()].
#'
#' @inheritParams gscale
#'
#' @details This function will scale all continuous variables in a regression
#'   model for ease of interpretation, especially for those models that have
#'   interaction terms. It can also mean-center all of them as well, if
#'   requested.
#'
#'   The scaling happens on the input data, not the terms themselves. That
#'   means interaction terms are still properly calculated because they are
#'   the product of standardized predictors, not a standardized product of
#'   predictors.
#'
#'   This function re-estimates the model, so for large models one should
#'   expect a runtime equal to the first run.
#'
#' @return The functions returns a re-fitted model object, inheriting
#'   from whichever class was supplied.
#'
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @family standardization
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
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#'  multilevel regression: Inferential and graphical techniques.
#'  \emph{Multivariate Behavioral Research}, \emph{40}(3), 373-400.
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple regression/correlation analyses for the behavioral sciences} (3rd
#' ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' @examples
#'
#' fit <- lm(formula = Murder ~ Income * Illiteracy,
#'           data = as.data.frame(state.x77))
#' fit_scale <- scale_mod(fit)
#' fit_scale <- scale_mod(fit, center = TRUE)
#'
#' # With weights
#' fitw <- lm(formula = Murder ~ Income * Illiteracy,
#'            data = as.data.frame(state.x77),
#'            weights = Population)
#' fitw_scale <- scale_mod(fitw)
#' fitw_scale <- scale_mod(fitw, center = TRUE, binary.input = "0/1")
#'
#' # With svyglm
#' if (requireNamespace("survey")) {
#' library(survey)
#' data(api)
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell*meals,design=dstrat)
#' regmodel_scale <- scale_mod(regmodel)
#' regmodel_scale <- scale_mod(regmodel, binary.input = "0/1")
#' }
#'
#' @importFrom stats weighted.mean as.formula getCall formula
#' @importFrom stats model.matrix model.weights
#' @aliases scale_lm
#' @export
#' @rdname scale_mod
#'

scale_mod.default <- function(model, binary.inputs = "0/1", n.sd = 1,
   center = TRUE, scale.response = FALSE, center.only = FALSE, 
   scale.only = FALSE, data = NULL, vars = NULL,
   apply.weighted.contrasts = getOption("jtools-weighted.contrasts", FALSE),
   ...) {

  # Save data --- using the call to access the data to avoid problems w/
  # transformed data
  call <- getCall(model)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }

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
  all_vars <- attributes(terms(model))$variables
  # Use 2:length bc 1 is "list"
  all_vars <- as.character(all_vars)[2:length(all_vars)]

  # If offset supplied in the formula, detect it, delete it
  if (!is.null(attr(terms(form), "offset"))) {

    off_pos <- attr(terms(form), "offset") # Get index of offset in terms list
    offname <- attr(terms(form), "variables")[off_pos] # Get its name in list
    formc <- gsub(offname, "", formc)

  }

  # Now I'm quoting all the names so there's no choking on vars with functions
  # applied. I save the backticked names
  for (var in all_vars) {

    regex_pattern <- paste("(?<=(~|\\s|\\*|\\+))", escapeRegex(var),
                           "(?=($|~|\\s|\\*|\\+))", sep = "")
    backtick_name <- paste("`", var, "`", sep = "")
    formc <- gsub(regex_pattern, backtick_name, formc, perl = T)

  }

  formc <- paste0(formc, collapse = "")
  formc <- gsub("``", "`", formc, fixed = TRUE)

  if (!is.null(model.weights(mf))) {

    weights <- TRUE
    the_weights <- model.weights(mf)

  } else {

    weights <- FALSE
    the_weights <- NULL

  }

  if (!is.null(data)) {

    mf <- data
    # Keep only variables used
    mf <- get_all_vars(formula(model), data = mf)
    # And only the complete cases
    mf <- mf[complete.cases(mf),]

  } else {

    all_vars <- sapply(all_vars, function(x) {
                         gsub("`", "", x, fixed = TRUE )
                       })

  }

  if (scale.response == FALSE) {
      # Now we need to know the variables of interest
      all_vars <- all_vars[all_vars %nin% resp]
  }

  if (!is.null(vars)) {
    all_vars <- all_vars[all_vars %in% vars]
  }

  mf <- gscale(vars = all_vars, data = mf, binary.inputs = binary.inputs,
               n.sd = n.sd, scale.only = !center,
               center.only = center.only, weights = the_weights,
               apply.weighted.contrasts = apply.weighted.contrasts, ...)

  form <- as.formula(formc)

  # Create new environment
  e <- new.env()
  # Add everything from the model's data to this environment
  lapply(names(mf), function(x, env, f) {env[[x]] <- f[[x]]}, env = e, f = mf)
  # Add the offset to the environment
  e$the_offset <- the_offset
  # Add the weights to the environment
  e$the_weights <- the_weights
  # Add the environment to the formula
  environment(form) <- e

  # Get the model's original call
  call <- getCall(model)
  # Replace that call's formula with this new one that includes the modified
  # environment. Then set the `data` arg of the call to NULL so it looks only
  # in the new, modified environment
  call$formula <- form
  call$data <- NULL
  # Conditionally add the names of the offset and weights args
  if (!is.null(the_offset)) {
    call$offset <- quote(the_offset)
  }
  if (!is.null(the_weights)) {
    call$weights <- quote(the_weights)
  }
  # Update the model
  new <- eval(call)

  return(new)

}

#' @export
#'

scale_mod.svyglm <- function(model, binary.inputs = "0/1", n.sd = 1,
   center = TRUE, scale.response = FALSE, center.only = FALSE, data = NULL,
   vars = NULL,
   apply.weighted.contrasts = getOption("jtools-weighted.contrasts", FALSE),
   ...) {

  # Save data --- using the call to access the data to avoid problems w/
  # transformed data
  call <- getCall(model)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }

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
  all_vars <- attributes(terms(model))$variables
  # Use 2:length bc 1 is "list"
  all_vars <- as.character(all_vars)[2:length(all_vars)]

  # If offset supplied in the formula, detect it, delete it
  if (!is.null(attr(terms(form), "offset"))) {

    off_pos <- attr(terms(form), "offset") # Get index of offset in terms list
    offname <- attr(terms(form), "variables")[off_pos] # Get its name in list
    formc <- gsub(offname, "", formc)

  }

  # Now I'm quoting all the names so there's no choking on vars with functions
  # applied. I save the backticked names
  for (var in all_vars) {

    regex_pattern <- paste("(?<=(~|\\s|\\*|\\+))", escapeRegex(var),
                           "(?=($|~|\\s|\\*|\\+))", sep = "")
    backtick_name <- paste("`", var, "`", sep = "")
    formc <- gsub(regex_pattern, backtick_name, formc, perl = T)

  }

  formc <- paste0(formc, collapse = "")
  formc <- gsub("``", "`", formc, fixed = TRUE)

  # Get the survey design object
  design <- model$survey.design

  # Now we need to know the variables of interest
  all_vars <- attributes(model$terms)$variables
  all_vars <- as.character(all_vars)[2:length(all_vars)]

  # Add vars to design if they aren't already there
  # (fixes issues with functions)
  adds <- which(all_vars %nin% names(design$variables))
  for (var in all_vars[adds]) {

    design$variables[[var]] <- mf[[var]]

  }

  # Complete cases only
  design$variables <-
    design$variables[complete.cases(design$variables[all_vars]),]

  if (scale.response == FALSE) {
    all_vars <- all_vars[all_vars %nin% resp]
  }

  if (!is.null(vars)) {
    all_vars <- all_vars[all_vars %in% vars]
  }


  # Call gscale()
  design <- gscale(vars = all_vars, data = design, n.sd = n.sd,
                   scale.only = !center, center.only = center.only, ...)

  call$design <- quote(design)

  call[[1]] <- survey::svyglm
  new <- eval(call)

  return(new)

}

#' Center variables in fitted regression models
#'
#' `center_mod` (previously known as `center_lm`) takes fitted regression models
#'  and mean-centers the
#'   continuous variables in the model to aid interpretation, especially in
#'   the case of models with interactions. It is a wrapper to
#'   \code{\link{scale_mod}}.
#'
#' @param model A regression model of type \code{lm}, \code{glm}, or
#' \code{\link[survey]{svyglm}}; others may work as well but have not been
#' tested.
#'
#' @param binary.inputs Options for binary variables. Default is \code{0/1};
#'   \code{0/1} keeps original scale; \code{-0.5,0.5} rescales 0 as -0.5 and 1
#'   as 0.5; \code{center} subtracts the mean; and \code{full} treats them
#'   like other continuous variables.
#'
#' @param center.response Should the response variable also be centered?
#'   Default is \code{FALSE}.
#'
#' @inheritParams scale_mod
#'
#' @details This function will mean-center all continuous variables in a
#'   regression model for ease of interpretation, especially for those models
#'   that have
#'   interaction terms. The mean for \code{svyglm} objects is calculated using
#'   \code{svymean}, so reflects the survey-weighted mean. The weight variables
#'   in \code{svyglm} are not centered, nor are they in other \code{lm} family
#'   models.
#'
#'   This function re-estimates the model, so for large models one should
#'   expect
#'   a runtime equal to the first run.
#'
#' @return The functions returns a \code{lm} or \code{glm} object, inheriting
#'   from whichever class was supplied.
#'
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @family standardization
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
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#'  multilevel regression: Inferential and graphical techniques.
#'  \emph{Multivariate Behavioral Research}, \emph{40}(3), 373-400.
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple regression/correlation analyses for the behavioral sciences} (3rd
#' ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' @examples
#'
#' fit <- lm(formula = Murder ~ Income * Illiteracy,
#'           data = as.data.frame(state.x77))
#' fit_center <- center_mod(fit)
#'
#' # With weights
#' fitw <- lm(formula = Murder ~ Income * Illiteracy,
#'            data = as.data.frame(state.x77),
#'            weights = Population)
#' fitw_center <- center_mod(fitw)
#'
#' # With svyglm
#' if (requireNamespace("survey")) {
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc =~ fpc)
#' regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#' regmodel_center <- center_mod(regmodel)
#' }
#'
#' @export center_mod
#' @aliases center_lm

center_mod <- center_lm <- function(model, binary.inputs = "0/1",
    center.response = FALSE, data = NULL,
    apply.weighted.contrasts = getOption("jtools-weighted.contrasts", FALSE), 
    ...) {

  out <- scale_mod(model, binary.inputs = binary.inputs,
                  scale.response = center.response, center.only = TRUE,
                  data = data,
                  apply.weighted.contrasts = apply.weighted.contrasts, ...)

  return(out)

}
