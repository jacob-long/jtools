#' @title Make new data for generating predicted data from regression models.
#' @description This is a convenience function that helps automate the process
#'  of generating predicted data from regression model from a predictor(s). It
#'  is designed to give you the data frame for the `predict` method's `newdata` 
#'  argument.
#' @param model The model (e.g., `lm`, `glm`, `merMod`, `svyglm`)
#' @param pred The name of the focal predictor as a string. This is the variable
#'   for which, if you are plotting, you'd likely have along the x-axis (with
#'   the dependent variable as the y-axis).
#' @param pred.values The values of `pred` you want to include. Default is NULL,
#'   which means a sequence of equi-spaced values over the range of a numeric 
#'   predictor or each level of a non-numeric predictor.
#' @param at If you want to manually set the values of other variables in the
#'   model, do so by providing a named list where the names are the variables 
#'   and the list values are vectors of the values. This can be useful 
#'   especially when you are exploring interactions or other conditional 
#'   predictions.
#' @param data The data frame used in fitting the model. Default is NULL, in
#'   which case the data will be retrieved via `model.frame` or, if there are
#'   variable transformations in the formula, by looking in the environment 
#'   for the data.
#' @param center Set numeric covariates to their mean? Default is TRUE. You 
#'   may also just provide a vector of names (as strings) of covariates to 
#'   center. Note that for `svyglm` models, the survey-weighted means are used.
#'   For models with weights, these are weighted means.
#' @param set.offset If the model has an offset, the value to use for the offset
#'   variable. Default is NULL, in which case the median of the offset variable
#'   is used.
#' @param num.preds The number of predictions to generate. Default is 100. 
#'   Ignored if `pred.values` is not `NULL`.
#' @param ... Extra arguments passed to `get_formula()`
#' @return A data frame.
#' @details 
#' 
#' Please bear in mind that this does not generate the predictions. You will
#' need to do that with a `predict` function for your model or another
#' interface, such as the `prediction` package's titular function. 
#' 
#' @examples 
#' 
#' fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))
#' # Basic use
#' new_data <- make_new_data(fit, pred = "Frost")
#' # Set covariate to specific value
#' new_data <- make_new_data(fit, pred = "Frost", at = list(Murder = 5))
#' # Set covariate to several specific values
#' new_data <- make_new_data(fit, pred = "Frost", at = list(Murder = c(5, 10, 15)))
#' 
#' 
#' @rdname make_new_data
#' @export 

make_new_data <- function(model, pred, pred.values = NULL, at = NULL,
                          data = NULL, center = TRUE, set.offset = NULL,
                          num.preds = 100, ...) {
  design <- if ("svyglm" %in% class(model)) model$survey.design else NULL
  
  if (is.null(data) | "svyglm" %in% class(model)) {
    data <- get_data(model)
  }
  
  formula <- get_formula(model, ...)
  
  # This is where the magic happens
  values <- get_control_values(model = model, data = data, at = at, 
                               preds = pred, center = center, design = design,
                               set.offset = set.offset, formula = formula)
  
  # Check for missing variables
  all_vars <- c(pred, names(values)) %not% c("(weights)", "(offset)")
  if (any(all_vars %nin% names(data))) {
    stop_wrap("The variable(s) ", paste(all_vars %not% names(data),
                                        collapse = " and "), 
              " were not found in the data.")
  }
  
  values[[pred]] <- if (is.null(pred.values)) {
    pred_values(data[[pred]], length = num.preds)
  } else {pred.values}
  
  # Some packages require the response variable to be in newdata and brms also
  # gets upset if that response variable is of a different class.
  the_resp <- get_response_name(model, ...)
  if (the_resp %in% names(data)) {
    if (is.numeric(data[[the_resp]])) {
      values[[the_resp]] <- NaN
    } else if (is.character(data[[the_resp]])) {
      values[[the_resp]] <- NA_character_
    } else {
      values[[the_resp]] <- NA
    }
  } else {
    values[[the_resp]] <- NA
  }
  
  new_data <- expand.grid(values, stringsAsFactors = FALSE)
  
  # In multivariate/distributional models, brms requires variables in the
  # non-focal parts of the model to be included in the newdata even though
  # they are unused.
  if ("brmsfit" %in% class(model)) {
    if (any(names(model$data) %nin% names(new_data))) {
      missing_names <- names(model$data) %not% names(new_data)
      new_data[missing_names] <- NA
    }
  }
  
  # When a user provides a matrix column variable, most commonly the result
  # of using the `scale()` function, the `predict()` functions fail if the
  # new data use a numeric variable instead.
  var_classes <- sapply(names(new_data), function(x) {
    "matrix" %in% class(data[[x]])
  })
  if (any(var_classes)) {
    for (var in names(var_classes)[var_classes]) {
      new_data[[var]] <- as.matrix(new_data[[var]])
    }
  }
  
  return(tibble::as_tibble(new_data))
  
}

#' @rdname model_utils
#' @export 
get_offset_name <- function(model) {
  
  if (!is.null(model.offset(model.frame(model)))) {
  
    # subset gives bare name
    offname <-
      as.character(getCall(model)$offset)[length(getCall(model)$offset)]
    # Sometimes it's character(0)
    if (length(offname) == 0) {
      offname <- NULL
    } else {
      offname <- all.vars(as.formula(paste("~", offname)))
    }
    
    if (is.null(offname)) {
      index <- attr(terms(model), "offset")
      offname <- all.vars(terms(model))[index]
    }
    
  } else {
    
    offname <- NULL
    
  }
  
  return(offname)
  
}

#' @rdname model_utils
#' @export 
get_weights <- function(model, data) {
  
  if ("svyglm" %in% class(model)) {
    wname <- "(weights)"
    weights <- weights(model$survey.design)
    return(list(weights_name = wname, weights = weights))
  }
  
  if (("(weights)" %in% names(data) | !is.null(getCall(model)$weights))) {
    weights <- TRUE
    # subset gives bare name
    wname <- as.character(deparse(getCall(model)$weights))
    # Sometimes it's character(0)
    if (length(wname) == 0 | wname == "NULL") {
      wname <- NULL
    } else {
      wname <- all.vars(as.formula(paste("~", wname)))
    }
    
    if ("(weights)" %in% colnames(data) & !is.null(wname)) {
      colnames(data)[which(colnames(data) == "(weights)")] <- wname
    } else if ("(weights)" %in% colnames(data) & is.null(wname)) {
      wname <- "(weights)"
    } 
    
    if (wname %in% colnames(data)) {
      wts <- data[[wname]]
    } else {
      wts <- model.weights(model.frame(model))
    }
    
  } else {
    
    weights <- FALSE
    wname <- NULL
    wts <- rep(1, times = nrow(data))
    
  }
  
  return(list(weights_name = wname, weights = wts))
  
}

#' @title Utility functions for generating model predictions
#' @description These functions get information and data from regression models.
#' @param model The model (e.g., `lm`, `glm`, `merMod`, `svyglm`)
#' @param data For `get_weights()`, the data used to fit the model.
#' @param warn For `get_data()`, should there be a warning when `model.frame()` 
#'  won't work because of variable transformations? Default is TRUE but this
#'  may not be desired when `get_data()` is used inside of another function or
#'  used multiple times.
#' @param formula The formula for `model`, if desired. Otherwise `get_formula()`
#'  is called.
#' @param ... Arguments passed to `get_formula()`
#' @return 
#' 
#' * `get_data()`: The data used to fit the model.
#' * `get_response()`: The name of the response variable.
#' * `get_offset_name()`: The name of the offset variable.
#' * `get_weights()`: A list with `weights_name`, the name of the weighting
#'  variable, and `weights`, the weights themselves (or all 1 when there are
#'  no weights).
#' @rdname model_utils
#' @export 

get_data <- function(model, formula = NULL, warn = TRUE, ...) {
  
  if ("svyglm" %in% class(model)) {
    d <- model$survey.design$variables
    wname <- "(weights)"
    d[wname] <- weights(model$survey.design)
  } else {
    d <- model.frame(model)
  }
  # Grab the formula
  if (is.null(formula)) {
    formula <- get_formula(model, ...)
  }
  # See if we're modeling a distributional DV in brmsfit
  is_dpar <- "is_dpar" %in% names(attributes(formula))
  # Get response name (needed for distributional DV)
  resp <- as.character(deparse(get_lhs(formula)))
  
  # Check to see if model.frame names match formula names
  varnames <- names(d)
  # Drop weights and offsets placeholder variable names
  varnames <- varnames[varnames %nin% c("(offset)","(weights)")]
  # Get the untransformed variable names
  raw_vars <- all.vars(formula)
  # Add the offset, if any
  raw_vars <- c(raw_vars, get_offset_name(model))
  # Drop distributional DV
  if (is_dpar) {raw_vars <- raw_vars %not% resp}
  # If survey, return now
  if ("svyglm" %in% class(model)) {
    return(tibble::as_tibble(d[unique(c(raw_vars, wname))], rownames = NA))
  }
  if (any(raw_vars %nin% varnames)) {
    dat_name <- as.character(deparse(getCall(model)$data))
    
    if (warn == TRUE) {
      msg_wrap("Using data ", dat_name, " from global environment. This
        could cause incorrect results if ", dat_name, " has been altered since 
        the model was fit. You can manually provide the data to the \"data =\" 
        argument.")
    }
    
    # Get the environment where model was fit --- otherwise tests fail 
    env <- attr(formula, ".Environment")
    # Grab the data from the environment
    d <- eval(getCall(model)$data, envir = env)
    # Make sure weights are included
    if (!is.null(model.weights(model.frame(model)))) {
      # If the weights are transformed, preserve that
      if (length(getCall(model)$weights) > 1) {
        wname <- as.character(deparse(getCall(model)$weights))
        # Make sure weights variable is in the data
        if (last(as.character(getCall(model)$weights)) %in% names(d)) {
          d[wname] <- eval(getCall(model)$weights, d)
        } else { # check calling environment otherwise
          d[wname] <- eval(getCall(model)$weights, env)
        }
      } else {
        wname <- as.character(getCall(model)$weights)
      } 
      raw_vars <- c(raw_vars, wname)
    }
    # Check for variables from global environment
    if (any(raw_vars %nin% names(d))) {
      global_vars <- raw_vars %not% names(d)
      # Attach each to data
      if (!is.null(env)) { # only if there is an environment
        d[unique(global_vars)] <- mget(unique(global_vars), envir = env)
      }
    }
    tibble::as_tibble(d[unique(raw_vars)], rownames = NA)
    
  } else {
    
    if ("(weights)" %in% names(d)) {
      names(d) %just% "(weights)" <- get_weights(model, d)$weights_name
    }
    tibble::as_tibble(d, rownames = NA)
    
  }
}

# adapted from https://stackoverflow.com/a/13217607/5050156
#' @rdname model_utils
#' @export 
get_response_name <- function(model, ...) {
  formula <- get_formula(model, ...)
  tt <- terms(formula)
  vars <- as.character(attr(tt, "variables"))[-1] ## [1] is the list call
  response <- attr(tt, "response") # index of response var
  vars[response] 
}

#' @title Retrieve formulas from model objects
#' 
#' This function is primarily an internal helper function in `jtools` and
#' related packages to standardize the different types of formula objects used
#' by different types of models.
#' 
#' @param model The fitted model object.
#' @param resp For `brmsfit` objects, the response variable for which 
#'   the formula is desired. `brmsfit` objects may have multiple formulas, so
#'   this selects a particular one. If `NULL`, the first formula is chosen 
#'   (unless `dpar` is specified).
#' @param dpar For `brmsfit` objects, the distributional variable for which 
#'   the formula is desired. If `NULL`, no distributional parameter is used.
#'   If there are multiple responses with distributional parameters, then 
#'   `resp` should be specified or else the first formula will be used by 
#'   default.
#' @param ... Ignored.
#' 
#' @value A `formula` object.
#' @examples 
#' 
#' data(mtcars)
#' fit <- lm(mpg ~ cyl, data = mtcars)
#' get_formula(fit)
#' 
#' @export
#' @rdname get_formula
get_formula <- function(model, ...) {
  UseMethod("get_formula")
}

#' @rdname get_formula
#' @export
get_formula.default <- function(model, ...) {
  formula(model)
}

#' @rdname get_formula
#' @export
get_formula.brmsfit <- function(model, resp = NULL, dpar = NULL, ...) {
  form <- formula(model)
  if ("mvbrmsformula" %in% class(form)) {
    form <- as.list(form)[["forms"]]
    if (is.null(resp) & is.null(dpar)) {
      return(as.formula(form[[1]]))
    } else if (!is.null(resp)) {
      resps <- lapply(form, function(x) {
        all.vars(as.formula(paste0("~", get_response_name(x[[1]]))))
      })
      if (resp %nin% resps) {
        stop_wrap(resp, " wasn't found in the model formula.")
      } else {
        form <- form[[which(resp == resps)]]
      }
    } else {
      form <- form[[1]]
    }
  } else {
    if (is.null(resp) & is.null(dpar)) return(as.formula(form))
  }
  
  if (!is.null(dpar)) {
    form <- form[["pforms"]][[dpar]]
    attr(form, "is_dpar") <- TRUE
    return(as.formula(form))
  } else {
    return(as.formula(form))
  }
}

get_family <- function(model, ...) {
  UseMethod("get_family")
}

get_family.default <- function(model, ...) {
  family(model)
}

get_family.brmsfit <- function(model, resp = NULL, ...) {
  form <- formula(model)
  fam <- family(model)
  if ("mvbrmsformula" %in% class(form)) {
    form <- as.list(form)[["forms"]]
    if (is.null(resp)) {
      return(fam[[1]])
    } else if (!is.null(resp)) {
      resps <- lapply(form, function(x) {
        all.vars(as.formula(paste0("~", get_response_name(x[[1]]))))
      })
      if (resp %nin% resps) {
        stop_wrap(resp, " wasn't found in the model formula.")
      } else {
        return(fam[[which(resp == resps)]])
      }
    }
  } else {
    return(fam)
  }
}

# formerly built into make_new_data, but I want to use it for other times
# when I just want the values of non-focal predictors
get_control_values <- function(model, data, preds, at, center, design = NULL,
                               set.offset = NULL, formula = NULL, ...) {

  offname <- get_offset_name(model)
  weight_info <- get_weights(model, data)
  weights <- weight_info$weights
  wname <- weight_info$weights_name
  
  if (is.null(formula)) formula <- get_formula(model, ...)

  controls <- as.list(data %not% c(preds, names(at), wname, offname))
  controls <- controls %just% all.vars(formula)
  if (length(controls) > 0) {
    
    if (center[1] == TRUE | (length(center) == 1 & center == "all" &
                             "all" %nin% names(controls))) {
      center <- names(controls)
    } else if (center[1] == FALSE | (length(center) == 1 & center == "none" &
                                     "none" %nin% names(controls))) {
      center <- NULL
    }
    if (length(center) > 0) {
      controls[center] <- mapply(center_value, d = controls[center],
                                 name = center,
                                 MoreArgs = list(design = design,
                                                 weights = weights),
                                 SIMPLIFY = FALSE)
    }
    
    not_centered <- names(controls) %not% center
    if (length(not_centered) > 0) {
      controls[not_centered] <- lapply(controls[not_centered], zero_or_base)
    }
    
  } else {controls <- list()}

  if (!is.null(at)) {
    for (n in names(at)) {
      controls[[n]] <- at[[n]]
    }
  }

  if (!is.null(offname)) {
    if (is.null(set.offset)) {
      offset.num <- median(data[[offname]])
    } else {
      offset.num <- set.offset
    }
    
    controls[[offname]] <- offset.num
    msg <- paste("Outcome is based on a total of", offset.num, "exposures")
    message(msg)
  }

  return(controls)
}

## Centering (w/o data change)

center_value <- function(d, name = NULL, weights, design = NULL) {
  
  # Just need to pick a helper function based on survey vs no survey
  if (!is.null(design)) {
    
    out <- center_value_survey(d, design = design, name = name)
    
  } else {
    
    out <- center_value_non_survey(d, weights)
    
  }
  
  return(out)
  
  
}

## If not svydesign, centering is fairly straightforward

center_value_non_survey <- function(d, weights) {
  
  if (is.numeric(d)) {
    return(weighted.mean(d, weights, na.rm = TRUE))
  } else if (!is.logical(d)) {
    return(levels(factor(d))[1])
  } else {
    return(FALSE)
  }
  
}

## Svydesigns get their own function to make control flow easier to follow
center_value_survey <- function(d, design = NULL, name = NULL) {
  
    if (is.numeric(d)) { # might have just pulled out all non-focals
      return(survey::svymean(survey::make.formula(name), design))
    } else if (!is.logical(d)) {
      return(levels(factor(d))[1])
    } else {
      return(FALSE)
    }

}

pred_values <- function(x, length = 100) {
  if (is.numeric(x)) {
    seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length)
  } else {
    unique(x) %not% NA
  }
}

zero_or_base <- function(x) {
  if (is.numeric(x)) {
    0
  } else if (!is.logical(x)) {
    levels(factor(x))[1]
  } else {
    FALSE
  }
}