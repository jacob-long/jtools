# Avoid duplicating code for the workhorse parts of the S3 method
do_partials <- function(model, vars = NULL, data = NULL, at = NULL,
                        center = TRUE, scale = c("response", "link"), 
                        set.offset = 1, ...) {
  # Get the original data if new data are not provided
  if (is.null(data)) {
    data <- get_data(model)
  }

  # Drop missing observations
  data <- drop_missing(model, data)

  # Make sure the vars are in the data
  if (any(vars %nin% names(data))) {
    stop_wrap(paste(vars %not% names(data), collapse = " and "),
              " not found in the data.")
  }
  # Make sure the vars are in the model
  if (any(vars %nin% original_terms(as.formula(formula(model))))) {
    the_terms <- original_terms(as.formula(formula(model)))
    stop_wrap(paste(vars %not% the_terms, collapse = " and "),
              " not found in the model.")
  }

  # Get fixed values of non-focal variables 
  controls <- get_control_values(model = model, data = data, preds = vars, 
                                 at = at, center = center)
  # Assign those values to the original data
  for (n in names(controls)) {
    data[[n]] <- controls[[n]]
  }

  resp <- get_response_name(model)
  predicted <- make_predictions(
    model = model, pred = NULL, new_data = data, interval = FALSE,
    outcome.scale = scale, return.orig.data = FALSE, ...
  )
  # Add a column to data with the predictions (replace original if it's there)
  data[[resp]] <- predicted[[resp]]

  return(data)
}

#' @rdname partialize
#' @export

partialize <- function(model, ...) {
  UseMethod("partialize")
}

#' @title Adjust observed data for partial residuals plots
#' @description This function is designed to facilitate the creation of partial
#'  residual plots, in which you can plot observed data alongside model 
#'  predictions. The difference is instead of the *actual* observed data, the 
#'  outcome variable is adjusted for the effects of the covariates.
#' @param model A regression model.
#' @param vars The variable(s) to *not* adjust for, as a string (or vector of
#'  strings). If I want to show the effect of `x` adjusting for the effect of
#'  `z`, then I would make `"x"` the `vars` argument.
#' @param data Optionally, provide the data used to fit the model (or some 
#'   other data frame with the same variables). Otherwise, it will be retrieved
#'   from the model or the global environment.
#' @param scale For GLMs, should the outcome variable be returned on the link
#'   scale or response scale? Default is `"response"`.
#' @inheritParams make_predictions
#' @return `data` plus the residualized outcome variable.
#' @details 
#'  The main use for working with partial residuals rather than the observed 
#'  values is to explore patterns in the model fit with respect to one or more
#'  variables while "controlling out" the effects of others. Plotting a 
#'  predicted line along with observed data may make a very well-fitting model
#'  look as if it is a poor fit if a lot of variation is accounted for by 
#'  variables other than the one on the x-axis.
#' 
#'  I advise consulting Fox and Weisberg (available free) for more details 
#'  on what partial residuals are. This function is designed to produce 
#'  data in a similar format to [effects::Effect()] when that function has 
#'  `residuals` set to `TRUE` and is plotted. I wanted a more modular function
#'  to produce the data separately. To be clear, the developers of the `effects`
#'  package have nothing to do with this function; `partialize`` is merely
#'  designed to replicate some of that functionality. 
#' @references 
#' Fox, J., & Weisberg, S. (2018). Visualizing fit and lack of fit in complex
#'  regression models with predictor effect plots and partial residuals. 
#'  *Journal of Statistical Software*, *87*(9), 1–27. 
#'  https://doi.org/10.18637/jss.v087.i09
#' @rdname partialize 
#' @export 
partialize.default <- function(model, vars = NULL, data = NULL, at = NULL,
                               center = TRUE, scale = c("response", "link"),
                               set.offset = 1, ...) {

  predicted <- do_partials(model, vars = vars, data = data, at = at,
                           center = center, scale = "link",
                           set.offset = set.offset, ...)
  resp <- get_response_name(model)
  # Add the residuals to the predictions
  resids <- residuals(model, type = "working")
  predicted[[resp]] <- predicted[[resp]] + resids[!is.na(resids)]

  # If we want it on the response scale, we need to transform back to it
  if (scale[1] == "response") {
    predicted[[resp]] <- family(model)$linkinv(predicted[[resp]])
  }
  
  return(tibble::as_tibble(predicted))
  
}

#' @export
partialize.brmsfit <- function(model, vars = NULL, data = NULL, at = NULL,
                               center = TRUE, scale = c("response", "link"),
                               set.offset = 1, ...) {

  predicted <- do_partials(model, vars = vars, data = data, at = at,
                           center = center, scale = "response",
                           set.offset = set.offset, ...)
  resp <- get_response_name(model)
  # Add the residuals to the predictions
  resids <- residuals(model, type = "ordinary")[,"Estimate"]
  predicted[[resp]] <- predicted[[resp]] + resids[!is.na(resids)]

  # If we want it on the link scale, we need to transform back to it
  # brms doesn't do predictions on the link scale, so this is the opposite
  # of the default behavior.
  if (scale[1] == "link") {
    predicted[[resp]] <- family(model)$linkfun(predicted[[resp]])
  }
  
  return(tibble::as_tibble(predicted))
  
}

#' @export
partialize.stanreg <- function(model, vars = NULL, data = NULL, at = NULL,
                               center = TRUE, scale = c("response", "link"),
                               set.offset = 1, ...) {

  predicted <- do_partials(model, vars = vars, data = data, at = at,
                           center = center, scale = "response",
                           set.offset = set.offset, ...)
  resp <- get_response_name(model)
  # Add the residuals to the predictions
  resids <- residuals(model)
  predicted[[resp]] <- predicted[[resp]] + resids[!is.na(resids)]

  # If we want it on the link scale, we need to transform back to it
  # brms doesn't do predictions on the link scale, so this is the opposite
  # of the default behavior.
  if (scale[1] == "link") {
    predicted[[resp]] <- family(model)$linkfun(predicted[[resp]])
  }
  
  return(tibble::as_tibble(predicted))
  
}

#' @export 
partialize.rq <- function(model, vars = NULL, data = NULL, at = NULL,
                          center = TRUE, scale = c("response", "link"),
                          set.offset = 1, ...) {

  predicted <- do_partials(model, vars = vars, data = data, at = at,
                           center = center, scale = "link",
                           set.offset = set.offset, ...)
  resp <- get_response_name(model)
  # Add the residuals to the predictions
  resids <- residuals(model, type = "working")
  predicted[[resp]] <- predicted[[resp]] + resids[!is.na(resids)]
  
  return(tibble::as_tibble(predicted))
  
}