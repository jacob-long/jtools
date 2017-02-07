#' Plot interaction effects in regression models
#'
#' \code{interact_plot()} plots regression lines at user-specified levels of a moderator
#' variable to explore interactions. The plotting is done with \code{ggplot2} rather than
#' base graphics, which some similar functions use.
#'
#' @param model A regression model of type \code{lm}, \code{glm}, or
#' \code{\link[survey]{svyglm}}. It should contain the interaction of interest.
#'
#' @param pred The name of the predictor variable involved in the interaction in
#'  quotations.
#'
#' @param modx The name of the moderator variable involved in the interaction in
#'  quotations.
#'
#' @param modxvals For which values of the moderator should lines be plotted?
#'   Default is \code{NULL}. If \code{NULL}, then the customary +/- 1 standard
#'   deviation from the mean values are used for continuous moderators. If the
#'   moderator is a factor variable and \code{modxvals} is \code{NULL}, each level
#'   of the factor is included. If \code{"mean-plus-minus"}, plots a line when
#'   moderator is at mean in addition to the default +/- standard deviation.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered.
#'   If \code{NULL}, all non-focal variables are mean-centered. If variables are specified,
#'   then no others are centered. Alternately, "all" will center focal variables
#'   as well.
#'
#' @param plot.points Logical. If \code{TRUE}, plots the actual data points as a
#'   scatterpolot on top of the interaction lines. If moderator is a factor, the dots
#'   will be the same color as their parent factor.
#'
#' @param interval Logical. If \code{TRUE}, plots confidence/prediction intervals
#'   the line using \code{\link[ggplot2]{geom_ribbon}}.
#'
#' @param int.type Type of interval to plot. Options are "confidence" or "prediction".
#'   Default is confidence interval.
#'
#' @param int.width How large should the interval be, relative to the standard error?
#'   The default, .95, corresponds to roughly 1.96 standard errors and a .05 alpha
#'   level for values outside the range. In other words, for a confidence interval,
#'   .95 is analagous to a 95\% confidence interval.
#'
#' @param x.label A character object specifying the desired x-axis label. If \code{NULL},
#'   the variable name is used.
#'
#' @param y.label A character object specifying the desired x-axis label. If \code{NULL},
#'   the variable name is used.
#'
#' @param mod.labels A character vector of labels for each level of the moderator values,
#'   provided in the same order as the \code{modxvals} argument. If \code{NULL},
#'   the values themselves are used as labels unless \code{modxvals} is also \code{NULL}.
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
#'   This function has not been extensively tested with non-linear models and may
#'   display unusual behavior and/or errors when used with them.
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
#' # Using a fitted lm model
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder*Illiteracy,
#'   data = states)
#' interact_plot(model = fit, pred = "Murder",
#'   modx = "Illiteracy")
#'
#' # Using interval feature
#' fit <- lm(accel ~ mag*dist, data=attenu)
#' interact_plot(fit, pred = "mag", modx = "dist", interval = TRUE,
#'   int.type = "confidence", int.width = .8)
#'
#'
#' @importFrom stats coef coefficients lm predict sd qnorm
#' @export interact_plot

interact_plot <- function(model, pred, modx, modxvals = NULL, centered = NULL,
                          plot.points = FALSE, interval = FALSE,
                          int.type = c("confidence","prediction"), int.width = .95,
                          x.label = NULL, y.label = NULL, mod.labels = NULL,
                          main.title = NULL, legend.main = NULL, color.class="Set2") {

  # Duplicating the dataframe so it can be manipulated as needed
  d <- as.data.frame(model$model)

  # For setting dimensions correctly later
  nc <- ncol(d)

  # Get the formula from lm object if given
  formula <- formula(model)
  formula <- paste(formula[2],formula[1],formula[3])

  # Pulling the name of the response variable for labeling
  resp <- sub("(.*)(?=~).*", x=formula, perl=T, replacement="\\1")
  resp <- trimws(resp)

  # Handling user-requested centered vars
  if (!is.null(centered) && centered != "all") {
    for (var in centered) {
      d[,var] <- d[,var] - mean(d[,var])
    }
  } else if (!is.null(centered) && centered == "all") {
    for (var in 1:nc) {
      d[,var] <- d[,var] - mean(d[,var])
    }
  } else { # Center all non-focal
    # Scaling the non-focal variables to make the slopes more interpretable (0 = mean)
    for (j in 1:ncol(d)) {
      if ((names(d)[j] %in% c(pred, resp, modx))==FALSE && is.numeric(d[,j])) {
        d[,j] <- as.vector(scale(d[,j]))
      }
    }
  }

  # Fixes a data type error with predict() later
  d <- as.data.frame(d)

  # Default to +/- 1 SD unless modx is factor
  if (is.null(modxvals) && !is.factor(d[,modx])) {
    modsd <- sd(d[,modx])
    modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx])-modsd)
    names(modxvalssd) <- c("+1 SD", "-1 SD")
    modxvals2 <- modxvalssd
  } else if (class(modxvals) == "character" && modxvals == "mean-plus-minus") {
    modsd <- sd(d[,modx])
    modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx]), mean(d[,modx])-modsd)
    names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")
    modxvals2 <- modxvalssd
  } else if (is.null(modxvals) && is.factor(d[,modx])){
    modxvals2 <- levels(d[,modx])
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
  pm <- matrix(rep(0, 100*(nc+2)*length(modxvals2)), ncol=(nc+2))
  # Naming columns
  colnames(pm) <- c(colnames(d),"ymax","ymin")
  # Convert to dataframe
  pm <- as.data.frame(pm)
  # Add values of moderator to df
  pm[,modx] <- facs
  # Add values of focal predictor to df
  pm[,pred] <- xpreds

  # Create predicted values based on specified levels of the moderator, focal predictor
  predicted <- as.data.frame(predict(model, pm, se.fit=T, interval=int.type[1]))
  pm[,resp] <- predicted[,1] # this is the actual values

  ## Convert the confidence percentile to a number of S.E. to multiply by
  intw <- 1 - ((1 - int.width)/2)
  ses <- qnorm(intw, 0, 1)

  # See minimum and maximum values for plotting intervals
  if (class(model)[1] == "lm" || class(model)[1] == "glm") {
    pm[,"ymax"] <- pm[,resp] + (predicted[,"se.fit"])*ses
    pm[,"ymin"] <- pm[,resp] - (predicted[,"se.fit"])*ses
  } else if (class(model)[1]=="svyglm") {
    pm[,"ymax"] <- pm[,resp] + (predicted[,"SE"])*ses
    pm[,"ymin"] <- pm[,resp] - (predicted[,"SE"])*ses
  }

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
    if (exists("modxvalssd") && length(modxvalssd)==2){
      pm[,modx] <- factor(pm[,modx], labels=c("-1 SD", "+1 SD"))
    } else if (exists("modxvalssd") && length(modxvalssd)==3){
      pm[,modx] <- factor(pm[,modx], labels=c("-1 SD", "Mean", "+1 SD"))
    }
  } else if (length(mod.labels)==length(modxvals2)) {
    pm[,modx] <- factor(pm[,modx], labels=mod.labels)
  } else {warning("mod.labels argument was not the same length as modxvals. Ignoring...")}

  if (is.null(legend.main)){
    legend.main <- modx
  }

  p <- ggplot2::ggplot(pm, ggplot2::aes(x=pm[,pred], y=pm[,resp], colour=pm[,modx]))
  p <- p + ggplot2::geom_path()

  # Plot intervals if requested
  if (interval==TRUE) {
    p <- p + ggplot2::geom_ribbon(data=pm, ggplot2::aes(ymin=pm[,"ymin"],
                                                        ymax=pm[,"ymax"],
                                                        alpha=.05),
                                  show.legend = FALSE)
  }

  if (plot.points==TRUE && is.factor(d[,modx])) {
    p <- p + ggplot2::geom_point(data=d, ggplot2::aes(x=d[,pred], y=d[,resp],
                                              colour=d[,modx]), position = "jitter")
  } else if (plot.points == TRUE && !is.factor(d[,modx])) {
    p <- p + ggplot2::geom_point(data=d, ggplot2::aes(x=d[,pred], y=d[,resp]),
                                 inherit.aes = F, position = "jitter")
  }

  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::labs(x = x.label, y = y.label)
  p <- p + ggplot2::scale_colour_brewer(name = legend.main, palette=color.class)

  if (!is.null(main.title)){
    p <- p + ggplot2::ggtitle(main.title)
  }

  return(p)

}

#' @export

print.interact_plot <- function(x, ...) {
  print(x)
}
