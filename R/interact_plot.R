#' Plot interaction effects in regression models
#'
#' \code{interact_plot()} plots regression lines at user-specified levels of a moderator
#' variable to explore interactions. The plotting is done with \code{ggplot2} rather than
#' base graphics, which some similar functions use.
#'
#' @param formula A regression formula, like would be provided to \code{lm()}, in
#'   quotation marks. This does not need to include the interaction term. Alternately,
#'   give an \code{lm} object and the formula will be extracted. Interaction terms
#'   will be ignored.
#'
#' @param data A data frame.
#'
#' @param pred The predictor variable involved in the interaction in quotes.
#'
#' @param modx The moderator variable involved in the interaction in quotes.
#'
#' @param modxvals For which two values of the moderator should simple slopes analysis
#'   be performed? Default is \code{NULL}. If \code{NULL}, then the customary +/-
#'   1 standard deviation from the mean values are used. This would not be appropriate
#'   in the case of a binary moderator, however, as well as in some other use cases.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered.
#'
#' @param weights If weights are being used, provide the variable name where they are stored.
#'
#' @param x.label A character object specifying the desired x-axis label. If \code{NULL},
#'   the variable name is used.
#'
#' @param y.label A character object specifying the desired x-axis label. If \code{NULL},
#'   the variable name is used.
#'
#' @param mod.labels A character vector of labels for each level of the moderator values,
#'   provided in the same order as the \code{modxvals} argument. If \code{NULL},
#'   the valeus themselves are used as labels unless \code{modxvals} is also \code{NULL}.
#'   In that case, "+1 SD" and "-1 SD" are used.
#'
#' @param main.title A character object that will be used as an overall title for the
#'   plot. If \code{NULL}, no main title is used.
#'
#' @param legend.main A character object that will be used as the title that appears
#'   above the legend. If \code{NULL}, the name of the moderating variable is used.
#'
#' @param color.class No options available for this in-development feature.
#'
#' @details This function provides a means for plotting conditional effects for the
#'   purpose of exploring interactions in the context of regression. You must have the
#'   package \code{ggplot2} installed to benefit from these plotting functions.
#'
#'   The function is designed for two-way interactions. For additional terms, the
#'   \code{\link[effects]{effects}} package may be better suited to the task.
#'
#'   The function can accept either a character object specifying the formula to be
#'   tested or a \code{lm} object instead, from which the formula will be extracted. All
#'   interactions will be stripped from the formula, leaving only the specified
#'   interaction. The function refits the model, so other features of your fitted
#'   model will be ignored (like the standard errors).
#'
#' @return The functions returns a \code{ggplot} object, which can be treated like
#'   a user-created plot and expanded upon as such.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @seealso \code{\link[rockchalk]{plotSlopes}} performs a similar function, but
#'   but with the base graphics package--this function is meant, in part, to simulate
#'   its features.
#'
#'   \code{\link{sim_slopes}} performs a simple slopes analysis with a similar
#'   argument syntax to this function.
#'
#' @examples
#' # Using a fitted model as formula input
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder + Illiteracy,
#'   data=states)
#' interact_plot(formula=fit, data=states, pred="Murder",
#'   modx="Illiteracy")
#'
#' # Writing out formula
#' interact_plot(formula="Income ~ HSGrad + Murder + Illiteracy", data=states,
#'   pred="Murder", modx="Illiteracy")
#'
#' @importFrom stats coef coefficients lm predict sd
#' @export interact_plot

interact_plot <- function(formula, data, pred, modx, modxvals = NULL, centered=NULL, weights = NULL, x.label = NULL, y.label = NULL, mod.labels = NULL, main.title = NULL, legend.main = NULL, color.class="qual") {

  # Duplicating the dataframe so it can be manipulated as needed
  d <- as.data.frame(data)

  # For setting dimensions correctly later
  nc <- ncol(data)

  # Handling user-supplied weights
  if (!is.null(weights)) {
    weight <- d[,weights]
  }

  # Get the formula from lm object if given
  if (class(formula)=="lm") {
    formula <- formula(formula)
    formula <- paste(formula[2],formula[1],formula[3])
  }

  # Remove interactions if found
  if (grepl("\\*", formula)) {
    # By swapping the asterisk for a plus sign, preserves all the predictors
    formula <- gsub("\\*", "\\+", as.character(formula))
    # Later on the formula syntax will be borked if I add a plus sign
    addplus <- FALSE
  } else {
    # Since there isn't a hanging plus sign, one needs to be added later
    addplus <- TRUE
  }

  # Remove moderator from formula
  if (grepl(modx, formula)) {
    formula <- gsub(modx, "", as.character(formula))
  }

  # Remove predictor from formula for sake of consistency, will add back later
  if (grepl(pred, formula)) {
    formula <- gsub(pred, "", as.character(formula))
  }

  # Pulling the name of the response variable for labeling
  resp <- sub("(.*)(?=~).*", x=formula, perl=T, replacement="\\1")
  resp <- trimws(resp)

  # Handling user-requested centered vars
  if (!is.null(centered)){
    for (var in centered) {
      d[,var] <- d[,var] - mean(d[,var])
    }
  } else { # Center all non-focal
    # Scaling the non-focal variables to make the slopes more interpretable (0 = mean)
    for (j in 1:ncol(d)) {
      if ((names(d)[j] %in% c(pred, resp, modx))==FALSE) {
        d[,j] <- as.vector(scale(d[,j]))
      }
    }
  }

  # Fixes a data type error with predict() later
  d <- as.data.frame(d)

  # Default to +/- 1 SD
  if (is.null(modxvals)) {
    modsd <- sd(d[,modx])
    modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx])-modsd)
    modxvalssd <- round(modxvalssd,3)
    names(modxvalssd) <- c("+1 SD", "-1 SD")
    modxvals2 <- modxvalssd
  } else { # Use user-supplied values otherwise
    modxvals2 <- modxvals
  }

  # Creating a set of dummy values of the focal predictor for use in predict()
  xpreds <- seq(from=range(d[,pred])[1], to=range(d[,pred])[2], length.out=100)
  xpreds <- rep(xpreds, length(modxvals2))

  # Create values of moderator for use in predict()
  facs <- rep(modxvals2[1],100)

  # Looping here allows for a theoretically limitless amount of moderator values
  for (i in 2:length(modxvals2)){
    facs <- c(facs, rep(modxvals2[i], 100))
  }

  # Creating matrix for use in predict()
  pm <- matrix(rep(0, 100*(nc)*length(modxvals2)), ncol=(nc))
  # Naming columns
  colnames(pm) <- c(colnames(d))
  # Convert to dataframe
  pm <- as.data.frame(pm)
  # Add values of moderator to df
  pm[,modx] <- facs
  # Add values of focal predictor to df
  pm[,pred] <- xpreds

  # Making sure formula has both focal variables and an interaction thereof
  form <- paste(formula, "+", modx, "*", pred, sep="")

  # Creating model, adding weights if needed
  if (is.null(weights)) {
    model <- lm(form, data=d)

  } else {
    model <- lm(form, data=d, weights=weight)
  }

  # Create predicted values based on specified levels of the moderator, focal predictor
  pm[,resp] <- predict(model, pm)

  # For plotting, it's good to have moderator as factor...
  # but not until after predict() is used
  pm[,modx] <- as.factor(pm[,modx])

  # Adding for future flexibility in color specification
  if (color.class == "qual") {
    colors <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')
  }

  if (is.null(x.label)){
    x.label <- pred
  }

  if (is.null(y.label)){
    y.label <- resp
  }

  if (is.null(mod.labels)) {
    if (!is.null(modxvalssd)){
      pm[,modx] <- factor(pm[,modx], labels=c("-1 SD", "+1 SD"))
    }
  }

  if (is.null(legend.main)){
    legend.main <- modx
  }

  p <- ggplot2::ggplot(pm, ggplot2::aes(x=pm[,pred], y=pm[,resp], colour=pm[,modx]))
  p <- p + ggplot2::geom_path()
  p <- p + ggplot2::theme_bw() + ggplot2::labs(x = x.label, y = y.label)
  p <- p + ggplot2::scale_color_discrete(name=modx)

  if (!is.null(main.title)){
    p <- p + ggplot2::ggtitle(main.title)
  }
  return(p)

}
