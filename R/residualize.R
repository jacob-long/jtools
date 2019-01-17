#' @export

residualize <- function(model, ...) {
  UseMethod("residualize")
}

#' @title Adjust observed data based on model components
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
#' @return `data` plus the residualized outcome variable.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname residualize 
#' @export 

residualize.default <- function(model, vars = NULL, data = NULL,
                        scale = c("response", "link")) {
  
  # Get the original data if new data are not provided
  if (is.null(data)) {
    data <- get_data(model)
  }
  
  # Get the terms predictions
  t_frame <- predict(model, type = "terms", newdata = data)
  # Save the constant
  constant <- attr(t_frame, "constant")
  # Convert to tibble so it is a data frame and doesnt turn into a vector if
  # I drop all but one predictor
  t_frame <- tibble::as_tibble(t_frame)
  # Generate the predictions other than the parts contributed by vars
  preds <- rowSums(t_frame %not% vars) + constant
  
  # If we want it on the response scale, we need to transform back to it
  if (scale[1] == "response") {
    preds <- family(model)$linkinv(preds)
  }
  
  # Add a column to data with the predictions 
  # (or replace original if its there)
  data[[get_response_name(model)]] <- preds
  return(tibble::as_tibble(data))
  
}

#' @export
residualize.merMod <- function(model, vars = NULL, data = NULL,
                                scale = c("response", "link")) {
  
  msg_wrap("Residualizing for mixed models is in an experimental state. 
           Results may not be correct.")

  # Get the original data if new data are not provided
  if (is.null(data)) {
    data <- get_data(model)
  }
  
  # Get the terms predictions
  t_frame <- pred_terms_merMod(model, newdata = data)
  # Save the constant
  constant <- attr(t_frame, "constant")
  # Convert to tibble so it is a data frame and doesnt turn into a vector if
  # I drop all but one predictor
  t_frame <- tibble::as_tibble(t_frame)
  # Generate the predictions other than the parts contributed by vars
  preds <- rowSums(t_frame %not% vars) + constant
  
  # If we want it on the response scale, we need to transform back to it
  if (scale[1] == "response") {
    preds <- family(model)$linkinv(preds)
  }
  
  # Add a column to data with the predictions 
  # (or replace original if its there)
  data[[get_response_name(model)]] <- preds
  return(tibble::as_tibble(data))
}

residd <- function(model, vars = NULL, data = NULL) {
  # Get the original data if new data are not provided
  if (is.null(data)) {
    data <- get_data(model)
  }
  
  pdata <- data
  
  mean_or_base <- function(x) {
    if (is.numeric(x)) {
      mean(x, na.rm = TRUE)
    } else if (!is.logical(x)) {
      levels(factor(x))[1]
    } else {
      FALSE
    }
  }
  
  vars <- names(pdata) %nin% vars
  
  pdata[vars] <- lapply(pdata[vars], mean_or_base) 
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")
  
  # Get the terms predictions
  t_frame <- predict(model, type = link_or_lm, newdata = pdata)
  # Convert to tibble so it is a data frame and doesnt turn into a vector if
  # I drop all but one predictor
  t_frame <- tibble::as_tibble(t_frame)
  
  # If we want it on the response scale, we need to transform back to it
  # if (scale[1] == "response") {
    t_frame[[1]] <- family(model)$linkinv(t_frame[[1]])
  # }
  
  # Add a column to data with the predictions 
  # (or replace original if its there)
  data[[get_response_name(model)]] <- t_frame[[1]]
  return(tibble::as_tibble(data))
}