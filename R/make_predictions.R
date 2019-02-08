#' @rdname make_predictions
#' @export

make_predictions <- function(model, ...) {
  
  UseMethod("make_predictions")
  
}

# Helper for the final output 
prepare_return_data <- function(model, data, return.orig.data, 
                                partial.residuals, pm, pred, at, 
                                center, set.offset) {
  if (return.orig.data == FALSE & partial.residuals == FALSE) {
    o <- tibble::as_tibble(pm)
  } else {
    if (return.orig.data == TRUE & partial.residuals == FALSE) {
      o <- list(predictions = tibble::as_tibble(pm), data = 
                suppressMessages(d <- tibble::as_tibble(get_data(model))))
      # If left-hand side is transformed, make new column in original data for
      # the transformed version and evaluate it
      if (is_lhs_transformed(as.formula(formula(model)))) {
        o[[2]][get_response_name(model)] <- 
          eval(get_lhs(as.formula(formula(model))), o[[2]])
      }
    } else {
      o <- list(predictions = tibble::as_tibble(pm), data = 
                  suppressMessages(
                    partialize(model, vars = pred, at = at, data = data,
                               center = center, set.offset = set.offset)
                  )
                )
    }
  }
  return(o)
}

#### Default method ########################################################

#' @title Generate predicted data for plotting results of regression models
#'
#' @description This is an alternate interface to the underlying tools that
#'   make up [effect_plot()] as well as `interact_plot` and `cat_plot` from
#'   the `interactions` package.
#'   `make_predictions` creates the data to be plotted and adds information
#'   to the original data to make it more amenable for plotting with the
#'   predicted data.
#'   
#' @inheritParams make_new_data
#' @inheritParams effect_plot
#' 
#' @param data Optional, default is NULL. You may provide the data used to
#'   fit the model. This can be a better way to get mean values for centering
#'   and can be crucial for models with variable transformations in the formula
#'   (e.g., `log(x)`) or polynomial terms (e.g., `poly(x, 2)`). You will
#'   see a warning if the function detects problems that would likely be
#'   solved by providing the data with this argument and the function will
#'   attempt to retrieve the original data from the global environment.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction
#'   intervals around the line using \code{\link[ggplot2]{geom_ribbon}}.
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
#' @param vcov Optional. You may supply the variance-covariance matrix of the
#'  coefficients yourself. This is useful if you are using some method for
#'  robust standard error calculation not supported by the \pkg{sandwich}
#'  package.
#'
#' @param set.offset For models with an offset (e.g., Poisson models), sets an
#'   offset for the predicted values. All predicted values will have the same
#'   offset. By default, this is set to 1, which makes the predicted values a
#'   proportion. See details for more about offset support.
#'
#' @param robust Should robust standard errors be used to find confidence
#'   intervals for supported models? Default is FALSE, but you should specify
#'   the type of sandwich standard errors if you'd like to use them (i.e.,
#'   `"HC0"`, `"HC1"`, and so on). If `TRUE`, defaults to `"HC3"` standard
#'   errors.
#'
#' @param cluster For clustered standard errors, provide the column name of
#'   the cluster variable in the input data frame (as a string). Alternately,
#'   provide a vector of clusters.
#'   
#' @param return.orig.data Instead of returning a just the predicted data frame,
#'  should the original data be returned as well? If so, then a list will be 
#'  return with both the predicted data (as the first element) and the original
#'  data (as the second element). Default is FALSE. 
#'
#' @param partial.residuals If `return.orig.data` is TRUE, should the observed
#'   dependent variable be replaced with the partial residual? This makes a 
#'   call to [partialize()], where you can find more details.
#'   
#' @param new_data If you would prefer to generate your own hypothetical
#'   (or not hypothetical) data rather than have the function make a call to
#'   [make_new_data()], you can provide it.
#'   
#' @param ... Ignored.
#'
#' @family plotting tools
#' @importFrom stats coef coefficients lm predict sd qnorm getCall model.offset
#' @importFrom stats median ecdf quantile get_all_vars complete.cases qt
#' @rdname make_predictions
#' @export
#' 


make_predictions.default <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, interval = TRUE,
  int.type = c("confidence", "prediction"), int.width = .95,
  outcome.scale = "response", robust = FALSE, cluster = NULL, vcov = NULL,
  set.offset = NULL, new_data = NULL, return.orig.data = FALSE,
  partial.residuals = FALSE, ...) {
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    # Get the data ready with make_new_data()
    pm <- make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}
  
  
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")
  
  # Do the predictions using built-in prediction method if robust is FALSE
  if (robust == FALSE) {
    predicted <- as.data.frame(predict(model, newdata = pm,
                                       se.fit = interval,
                                       interval = int.type[1],
                                       type = link_or_lm))
  } else { # Use my custom robust predictions function
    if (is.null(vcov)) {
      the_vcov <- do_robust(model, robust, cluster, data)$vcov
    } else {
      the_vcov <- vcov
    }
    predicted <- as.data.frame(predict_rob(model, newdata = pm,
                                           se.fit = interval,
                                           interval = int.type[1],
                                           type = link_or_lm,
                                           .vcov = the_vcov))
  }
  
  pm[[get_response_name(model)]] <- predicted[[1]]
  
  ## Convert the confidence percentile to a number of S.E. to multiply by
  intw <- 1 - ((1 - int.width)/2)
  ## Try to get the residual degrees of freedom to get the critical value
  r.df <- try({
    df.residual(model)
  }, silent = TRUE)
  if (is.numeric(r.df)) {
    ses <- qt(intw, r.df)
  } else {
    message(wrap_str("Could not find residual degrees of freedom for this
                       model. Using confidence intervals based on normal
                       distribution instead."))
    ses <- qnorm(intw, 0, 1)
  }
  
  # See minimum and maximum values for plotting intervals
  if (interval == TRUE) { # only create SE columns if intervals are needed
    pm[["ymax"]] <- pm[[get_response_name(model)]] + (predicted[["se.fit"]]) * ses
    pm[["ymin"]] <- pm[[get_response_name(model)]] - (predicted[["se.fit"]]) * ses
  } else {
    # Do nothing
  }
  
  # Back-convert the predictions to the response scale
  if (outcome.scale == "response") {
    pm[[get_response_name(model)]] <-
      family(model)$linkinv(pm[[get_response_name(model)]])
    if (interval == TRUE) {
      pm[["ymax"]] <- family(model)$linkinv(pm[["ymax"]])
      pm[["ymin"]] <- family(model)$linkinv(pm[["ymin"]])
    }
  }
  
  # Use helper function to prepare the final return object
  o <- prepare_return_data(model = model, data = data,
                           return.orig.data = return.orig.data, 
                           partial.residuals = partial.residuals,
                           pm = pm, pred = pred, at = at, center = center,
                           set.offset = set.offset)
  return(o)
  
}

### svyglm method #############################################################
#' @export

make_predictions.svyglm <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, interval = TRUE, 
  int.type = c("confidence", "prediction"), int.width = .95, 
  outcome.scale = "response", set.offset = NULL, new_data = NULL,
  return.orig.data = FALSE, partial.residuals = FALSE, ...) {
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    # Get the data ready with make_new_data()
    pm <- make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}
  
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")
  
  # Do the predictions using built-in prediction method if robust is FALSE
  predicted <- as.data.frame(predict(model, newdata = pm,
                                     se.fit = TRUE, interval = int.type[1],
                                     type = link_or_lm))
  
  pm[[get_response_name(model)]] <- predicted[[1]]
  
  ## Convert the confidence percentile to a number of S.E. to multiply by
  intw <- 1 - ((1 - int.width)/2)
  ## Try to get the residual degrees of freedom to get the critical value
  r.df <- try({
    df.residual(model)
  }, silent = TRUE)
  if (is.numeric(r.df)) {
    ses <- qt(intw, r.df)
  } else {
    message(wrap_str("Could not find residual degrees of freedom for this
                       model. Using confidence intervals based on normal
                       distribution instead."))
    ses <- qnorm(intw, 0, 1)
  }
  
  # See minimum and maximum values for plotting intervals
  if (interval == TRUE) { # only create SE columns if intervals are needed
    pm[["ymax"]] <- pm[[get_response_name(model)]] + (predicted[["SE"]]) * ses
    pm[["ymin"]] <- pm[[get_response_name(model)]] - (predicted[["SE"]]) * ses
  } else {
    # Do nothing
  }
  
  
  # Back-convert the predictions to the response scale
  if (outcome.scale == "response") {
    pm[[get_response_name(model)]] <-
      family(model)$linkinv(pm[[get_response_name(model)]])
    if (interval == TRUE) {
      pm[["ymax"]] <- family(model)$linkinv(pm[["ymax"]])
      pm[["ymin"]] <- family(model)$linkinv(pm[["ymin"]])
    }
  }
  
  # Use helper function to prepare the final return object
  o <- prepare_return_data(model = model, data = data,
                           return.orig.data = return.orig.data, 
                           partial.residuals = partial.residuals,
                           pm = pm, pred = pred, at = at, center = center,
                           set.offset = set.offset)
  return(o)
  
}

### merMod method #############################################################
#' @export
make_predictions.merMod <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, interval = TRUE, 
  int.type = c("confidence", "prediction"), int.width = .95,
  outcome.scale = "response", re.form = ~0, add.re.variance = FALSE,
  boot = FALSE, sims = 1000, progress = "txt", set.offset = NULL, 
  new_data = NULL, return.orig.data = FALSE, partial.residuals = FALSE,
  message = TRUE, ...) {
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    # Get the data ready with make_new_data()
    pm <- make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}
  
  resp <- get_response_name(model)
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")
  
  if (interval == TRUE && boot == FALSE && message == TRUE) {
    msg_wrap("Confidence intervals for merMod models is an experimental
              feature. The intervals reflect only the variance of the
              fixed effects, not the random effects.")
  }
  
  # Do the predictions using built-in prediction method if robust is FALSE
  if (interval == FALSE & is.null(model.offset(model.frame(model)))) {
    predicted <- as.data.frame(predict(model, newdata = pm,
                                       type = link_or_lm,
                                       re.form = re.form,
                                       allow.new.levels = FALSE))
    
    pm[[get_response_name(model)]] <- predicted[[1]]
    
  } else { # Use my custom predictions function
    
    if (interactive() & boot == TRUE & progress != "none") {
      cat("Bootstrap progress:\n")
    }
    predicted <- predict_mer(model, newdata = pm, use_re_var = add.re.variance,
                             se.fit = TRUE, allow.new.levels = TRUE, 
                             type = link_or_lm, re.form = re.form,
                             boot = boot, sims = sims, prog_arg = progress, ...)
    
    if (boot == TRUE) {
      raw_boot <- predicted
      
      ## Convert the confidence percentile to a number of S.E. to multiply by
      intw <- 1 - ((1 - int.width) / 2)
      # Set the predicted values at the median
      fit <- sapply(as.data.frame(raw_boot), median)
      upper <- sapply(as.data.frame(raw_boot), quantile, probs = intw)
      lower <- sapply(as.data.frame(raw_boot), quantile, probs = 1 - intw)
      
      # Add to predicted frame
      pm[[resp]] <- fit
      pm[["ymax"]] <- upper
      pm[["ymin"]] <- lower
      
      # Drop the cases that should be missing if I had done it piecewise
      pm <- pm[complete.cases(pm),]
    } else {
      ## Convert the confidence percentile to a number of S.E. to multiply by
      intw <- 1 - ((1 - int.width)/2)
      ## Try to get the residual degrees of freedom to get the critical value
      r.df <- try({
        df.residual(model)
      }, silent = TRUE)
      if (is.numeric(r.df)) {
        ses <- qt(intw, r.df)
      } else {
        message(wrap_str("Could not find residual degrees of freedom for this
                       model. Using confidence intervals based on normal
                       distribution instead."))
        ses <- qnorm(intw, 0, 1)
      }
      pm[[get_response_name(model)]] <- predicted[[1]]
      pm[["ymax"]] <-
        pm[[get_response_name(model)]] + (predicted[["se.fit"]]) * ses
      pm[["ymin"]] <-
        pm[[get_response_name(model)]] - (predicted[["se.fit"]]) * ses
    }
  }
  
  
  # Back-convert the predictions to the response scale
  if (outcome.scale == "response") {
    pm[[resp]] <- family(model)$linkinv(pm[[resp]])
    if (interval == TRUE) {
      pm[["ymax"]] <- family(model)$linkinv(pm[["ymax"]])
      pm[["ymin"]] <- family(model)$linkinv(pm[["ymin"]])
    }
  }
  
  # Use helper function to prepare the final return object
  o <- prepare_return_data(model = model, data = data,
                           return.orig.data = return.orig.data, 
                           partial.residuals = partial.residuals,
                           pm = pm, pred = pred, at = at, center = center,
                           set.offset = set.offset)
  return(o)
  
}


### stanreg method ###########################################################
#' @export

make_predictions.stanreg <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, estimate = c("mean", "median"), interval = TRUE,
  int.width = .95, re.form = ~0,  set.offset = NULL, new_data = NULL,
  return.orig.data = FALSE, partial.residuals = FALSE, ...) {
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    # Get the data ready with make_new_data()
    pm <- make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}
  
  predicted <- 
    rstanarm::posterior_predict(model, 
                                newdata = pm %not% get_response_name(model), 
                                re.form = re.form)
  
  # the 'ppd' object is a weird pseudo-matrix that misbehaves when
  # I try to make it into a data frame
  if (estimate[1] == "mean") {
    pm[[get_response_name(model)]] <- colMeans(predicted)
  } else if (estimate[1] == "median") {
    pm[[get_response_name(model)]] <- apply(predicted, 2, median)
  }
  
  if (interval == TRUE) {
    
    ints <- rstanarm::predictive_interval(predicted, prob = int.width)
    
    pm[["ymax"]] <- ints[,2]
    pm[["ymin"]] <- ints[,1]
    
  }
  
  # Use helper function to prepare the final return object
  o <- prepare_return_data(model = model, data = data,
                           return.orig.data = return.orig.data, 
                           partial.residuals = partial.residuals,
                           pm = pm, pred = pred, at = at, center = center,
                           set.offset = set.offset)
  return(o)
  
}

### brmsfit method ###########################################################
#' @export
make_predictions.brmsfit <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, estimate = c("mean", "median"), interval = TRUE,
  int.width = .95, re.form = ~0,  set.offset = NULL, new_data = NULL,
  return.orig.data = FALSE, partial.residuals = FALSE, ...) {
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    # Get the data ready with make_new_data()
    pm <- make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}
  
  intw <- c(((1 - int.width)/2), 1 - ((1 - int.width)/2))
  
  # the 'ppd' object is a weird pseudo-matrix that misbehaves when
  # I try to make it into a data frame
  if (estimate[1] == "mean") {
    predicted <- as.data.frame(predict(model,
                                 newdata = pm %not% get_response_name(model),
                                 re_formula = re.form, robust = FALSE,
                                 probs = intw))
    pm[[get_response_name(model)]] <- predicted[[1]]
  } else if (estimate[1] == "median") {
    predicted <- as.data.frame(predict(model,
                                 newdata = pm %not% get_response_name(model),
                                 re_formula = re.form, robust = TRUE,
                                 probs = intw))
    pm[[get_response_name(model)]] <- predicted[[1]]
  }
  
  if (interval == TRUE) {
    pm[["ymax"]] <- predicted[[4]]
    pm[["ymin"]] <- predicted[[3]]
  }
  
  # Use helper function to prepare the final return object
  o <- prepare_return_data(model = model, data = data,
                           return.orig.data = return.orig.data, 
                           partial.residuals = partial.residuals,
                           pm = pm, pred = pred, at = at, center = center,
                           set.offset = set.offset)
  return(o)
  
}


### quantile regression #######################################################
#' @export

make_predictions.rq <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, interval = TRUE, int.width = .95,
  se = c("nid", "iid", "ker"), set.offset = NULL, new_data = NULL,
  return.orig.data = FALSE, partial.residuals = FALSE, ...) {
  
  se <- match.arg(se, c("nid", "iid", "ker"), several.ok = FALSE)
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    # Get the data ready with make_new_data()
    pm <- make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}
  
  predicted <- as.data.frame(predict(model, newdata = pm,
                                     interval = "confidence",
                                     se = se, level = int.width))
  
  pm[[get_response_name(model)]] <- predicted[[1]]
  
  if (interval == TRUE) {
    pm[["ymax"]] <- predicted[["higher"]]
    pm[["ymin"]] <- predicted[["lower"]]
  }
  
  # Use helper function to prepare the final return object
  o <- prepare_return_data(model = model, data = data,
                           return.orig.data = return.orig.data, 
                           partial.residuals = partial.residuals,
                           pm = pm, pred = pred, at = at, center = center,
                           set.offset = set.offset)
  return(o)
  
}

