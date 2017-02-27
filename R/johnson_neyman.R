#' Calculate Johnson-Neyman intervals for 2-way interactions
#'
#' \code{johnson_neyman} finds so-called "Johnson-Neyman" intervals for
#' understanding where simple slopes are significant in the context of
#' interactions in multiple linear regression.
#'
#' @param model A regression model of type \code{lm} or \code{\link[survey]{svyglm}}.
#'    It should contain the interaction of interest.
#'
#' @param pred The predictor variable involved in the interaction.
#'
#' @param modx The moderator variable involved in the interaction.
#'
#' @param vmat Optional. You may supply the variance-covariance matrix of the
#'   coefficients yourself. This is useful if you are using robust standard
#'   errors, as you could if using the \pkg{sandwich} package.
#'
#' @param alpha The alpha level. By default, the standard 0.05.
#'
#' @param plot Should a plot of the results be printed? Default is \code{TRUE}.
#'   The \code{ggplot2} object is returned either way.
#'
#' @details
#'
#'  The interpretation of the values given by this function is important and not
#'  always immediately intuitive. For an interaction between a predictor variable
#'  and moderator variable, it is often the case that the slope of the predictor
#'  is statistically significant at only some values of the moderator. For
#'  example, perhaps the effect of your predictor is only significant when the
#'  moderator is set at some high value.
#'
#'  The Johnson-Neyman interval provides the two values of the moderator at
#'  which the slope of the predictor goes from non-significant to significant.
#'  Usually, the predictor's slope is only significant \emph{outside} of the
#'  range given by the function. The output of this function will make it clear
#'  either way.
#'
#'  This technique is not easily ported to 3-way interaction contexts. You could,
#'  however, look at the J-N interval at two different levels of a second
#'  moderator. This does forego a benefit of the J-N technique, which is not
#'  having to pick arbitrary points. If you want to do this, just use the
#'  \code{\link{sim_slopes}} function's ability to handle 3-way interactions
#'  and request Johnson-Neyman intervals for each.
#'
#' @return
#'
#'  \item{bounds}{The two numbers that make up the interval.}
#'  \item{cbands}{A dataframe with predicted values of the predictor's slope
#'    and lower/upper bounds of confidence bands if you would like to make your
#'    own plots}
#'  \item{plot}{The \code{ggplot} object used for plotting. You can tweak the
#'    plot like you could any other from \code{ggplot}.}
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @family interaction tools
#'
#' @references
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and multilevel
#'  regression: Inferential and graphical techniques. \emph{Multivariate Behavioral
#'  Research}, \emph{40}(3), 373-400.
#'  \url{http://dx.doi.org/10.1207/s15327906mbr4003_5}
#'
#' Johnson, P.O. & Fay, L.C. (1950). The Johnson-Neyman technique, its theory
#'  and application. \emph{Psychometrika}, \emph{15}, 349-367.
#'  \url{http://dx.doi.org/10.1007/BF02288864}
#'
#' @examples
#' # Using a fitted lm model
#' states <- as.data.frame(state.x77)
#' states$HSGrad <- states$`HS Grad`
#' fit <- lm(Income ~ HSGrad + Murder*Illiteracy,
#'   data = states)
#' johnson_neyman(model = fit, pred = Murder,
#'   modx = Illiteracy)
#'
#' @importFrom stats vcov
#' @export
#'

johnson_neyman <- function(model, pred, modx, vmat = NULL, alpha = 0.05,
                           plot = TRUE) {

  # Parse unquoted variable names
  predt <- as.character(substitute(pred))
  modxt <- as.character(substitute(modx))

  # Doing a check so it works when called from inside a function
  if (!(predt %in% names(coef(model)))) {
    pred <- as.character(eval(pred))
    modx <- as.character(eval(modx))
  } else {
    pred <- predt
    modx <- modxt
  }

  # Structure
  out <- NULL
  out <- structure(out, pred = pred, modx = modx, alpha = alpha, plot = plot)

  # Set critical t value
  alpha <- alpha/2
  tcrit <- qnorm(alpha, 0, 1)
  tcrit <- abs(tcrit) # Reverse the sign since it gives negative at these low vals

  # Construct interaction term

  ## Hard to predict which order lm() will have the predictors in
  intterm1 <- paste(pred, ":", modx, sep = "") # first possible ordering
  intterm1tf <- any(intterm1 %in% names(coef(model))) # is it in the coef names?
  intterm2 <- paste(modx, ":", pred, sep = "") # second possible ordering
  intterm2tf <- any(intterm2 %in% names(coef(model))) # is it in the coef names?

  ## Now we know which of the two is found in the coefficents
  inttermstf <- c(intterm1tf, intterm2tf) # Using this to get the index of the TRUE one
  intterms <- c(intterm1, intterm2) # Both names, want to keep one
  intterm <- intterms[which(inttermstf)] # Keep the index that is TRUE


  # Construct constituent terms to calculate the subsequent quadratic a,b,c
  if (is.null(vmat)) {

    # Get vcov
    vmat <- vcov(model)

    # Variance of interaction term (gamma_3)
    covy3 <- vmat[intterm,intterm]
    # Variance of predictor term (gamma_1)
    covy1 <- vmat[pred,pred]
    # Covariance of predictor and interaction terms (gamma_1 by gamma_3)
    covy1y3 <- vmat[intterm,pred]
    # Actual interaction coefficient (gamma_3)
    y3 <- coef(model)[intterm]
    # Actual predictor coefficient (gamma_1)
    y1 <- coef(model)[pred]

  } else { # user-supplied vcov, useful for robust calculation

    # Variance of interaction term (gamma_3)
    covy3 <- vmat[intterm,intterm]
    # Variance of predictor term (gamma_1)
    covy1 <- vmat[pred,pred]
    # Covariance of predictor and interaction terms (gamma_1 by gamma_3)
    covy1y3 <- vmat[intterm,pred]
    # Actual interaction coefficient (gamma_3)
    y3 <- coef(model)[intterm]
    # Actual predictor coefficient (gamma_1)
    y1 <- coef(model)[pred]

  }

  # Now we use this info to construct a quadratic equation
  a <- tcrit^2 * covy3 - y3^2
  b <- 2 * (tcrit^2 * covy1y3 - y1*y3)
  c <- tcrit^2 * covy1 - y1^2

  # Now we define a function to test for number of real solutions to it
  ## The discriminant can tell you how many there will be
  discriminant <- function(a,b,c) {
    disc <- b^2-4*a*c

    # If the discriminant is zero or something else, can't proceed.
    if (disc > 0) {
      out <- disc
    } else if (disc == 0) {
      msg <- "There is only one real solution for the Johnson-Neyman interval.
      Values cannot be supplied."
      stop(msg)
    } else {
      msg <- "There are no real solutions for the Johnson-Neyman interval.
      Values cannot be supplied."
      stop(msg)
    }

    return(out)
  }

  disc <- discriminant(a,b,c)

  # As long as the above didn't error, let's solve the quadratic with this function
  quadsolve <- function(a,b,c, disc) {
    # first return value
    x1 <- (-b+sqrt(disc))/(2*a)
    # second return value
    x2 <- (-b-sqrt(disc))/(2*a)
    # return a vector of both values
    result <- c(x1,x2)
    # make sure they are in increasing order
    result <- sort(result, decreasing = F)

    return(result)

  }

  bounds <- quadsolve(a,b,c, disc)
  names(bounds) <- c("Lower", "Higher")

  # Need to calculate confidence bands
  cbands <- function(x2, y1, y3, covy1, covy3, covy1y3, tcrit, predl, modx) {

    upper <- c() # Upper values
    slopes <- c() # predicted slope line
    lower <- c() # Lower values

    # Iterate through mod values given
    slopesf <- function(i) {
      # Slope
      s <- y1 + y3*i
      return(s)
    }
    upperf <- function(i, s) {
      # Upper confidence band
      u <- s + tcrit * sqrt((covy1 + 2*i*covy1y3 + i^2 * covy3))
      return(u)
    }
    lowerf <- function(i, s) {
      # Lower confidence band
      l <- s - tcrit * sqrt((covy1 + 2*i*covy1y3 + i^2 * covy3))
      return(l)
    }

    slopes <- sapply(x2, slopesf, simplify = "vector", USE.NAMES = FALSE)
    upper <- mapply(upperf, x2, slopes)
    lower <- mapply(lowerf, x2, slopes)

    out <- matrix(c(x2, slopes, lower, upper), ncol = 4)
    colnames(out) <- c(modx, predl, "Lower", "Upper")
    out <- as.data.frame(out)
    return(out)

  }

  # Getting the range of the moderator
  modrange <- range(model.frame(model)[,modx])
  modrangeo <- range(model.frame(model)[,modx]) # for use later
  modsd <- sd(model.frame(model)[,modx]) # let's expand outside observed range
  modrange[1] <- modrange[1] - modsd
  modrange[2] <- modrange[2] + modsd

  # Generating values to feed to the CI function from the range
  x2 <- seq(from = modrange[1], to = modrange[2], length.out = 1000)

  # Make slopes colname
  predl <- paste("Slope of", pred)

  cbs <- cbands(x2, y1, y3, covy1, covy3, covy1y3, tcrit, predl, modx)

  out$bounds <- bounds
  out <- structure(out, modrange = modrangeo)

  # Need to check whether sig vals are within or outside bounds
  sigs <- which((cbs$Lower < 0 & cbs$Upper < 0) | (cbs$Lower > 0 & cbs$Upper > 0))

  # Going to split cbands values into significant and insignificant pieces
  insigs <- setdiff(1:1000, sigs)

  # Create grouping variable in cbs
  index <- 1:1000 %in% insigs
  cbs$Significance[index] <- "Insignificant"
  index <- 1:1000 %in% sigs
  cbs$Significance[index] <- "Significant"
  cbs$Significance <- factor(cbs$Significance)

  # Give user this little df
  out$cbands <- cbs

  # I'm looking for whether the significant vals are inside or outside
  ## Would like to find more elegant way to do it
  index <- which(cbs$Significance == "Significant")[1]

  if (index != 0) {
    inside <- (cbs[index,modx] > bounds[1] && cbs[index,modx] < bounds[2])
  } else {
    stop("No moderator values in the range of the observed data were associated with significant slopes of the predictor.")
  }


  out <- structure(out, inside = inside)

  # Splitting df into three pieces
  cbso1 <- cbs[cbs[,modx] < bounds[1],]
  cbso2 <- cbs[cbs[,modx] > bounds[2],]
  cbsi <- cbs[(cbs[,modx] > bounds[1] & cbs[,modx] < bounds[2]),]

  # Create label based on alpha level
  alpha <- alpha*2 # Undoing what I did earlier
  alpha <- gsub("0\\.", "\\.", as.character(alpha))
  pmsg <- paste("p <", alpha)

  # Let's make a J-N plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_path(data = cbso1, ggplot2::aes(x = cbso1[,modx],
                                    y = cbso1[,predl],
                                    color = cbso1[,"Significance"])) +
    ggplot2::geom_path(data = cbsi, ggplot2::aes(x = cbsi[,modx],
                                                  y = cbsi[,predl],
                                                  color = cbsi[,"Significance"])) +
    ggplot2::geom_path(data = cbso2, ggplot2::aes(x = cbso2[,modx],
                                                  y = cbso2[,predl],
                                                  color = cbso2[,"Significance"])) +
    ggplot2::geom_ribbon(data = cbso1,
                         ggplot2::aes(x = cbso1[,modx], ymin = cbso1[,"Lower"],
                                      ymax = cbso1[,"Upper"],
                                      fill = cbso1[,"Significance"]), alpha = 0.2) +
    ggplot2::geom_ribbon(data = cbsi,
                         ggplot2::aes(x = cbsi[,modx], ymin = cbsi[,"Lower"],
                                      ymax = cbsi[,"Upper"],
                                      fill = cbsi[,"Significance"]), alpha = 0.2) +
    ggplot2::geom_ribbon(data = cbso2,
                         ggplot2::aes(x = cbso2[,modx], ymin = cbso2[,"Lower"],
                                      ymax = cbso2[,"Upper"],
                                      fill = cbso2[,"Significance"]), alpha = 0.2) +
    ggplot2::scale_fill_manual(values = c("Significant" = "#00BFC4",
                                          "Insignificant" = "#F8766D"),
                               labels = c("n.s.", pmsg)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +

    ggplot2::geom_segment(ggplot2::aes(x = modrangeo[1], xend = modrangeo[2],
                                       y = 0, yend = 0,
                                       linetype = "Range of\nobserved\ndata"),
                          lineend = "square", size = 1.25)

    if (out$bounds[1] < modrange[1]) {
      # warning("The lower bound is outside the range of the plotted data")
    } else {
      plot <- plot + ggplot2::geom_vline(ggplot2::aes(xintercept = out$bounds[1]),
                                         linetype = 2, color = "#00BFC4")
    }

    if (out$bounds[2] > modrange[2]) {
      # warning("The upper bound is outside the range of the plotted data")
    } else {
      plot <- plot + ggplot2::geom_vline(ggplot2::aes(xintercept = out$bounds[2]),
                                         linetype = 2, color = "#00BFC4")
    }

    plot <- plot + ggplot2::xlim(range(cbs[,modx])) +
      ggplot2::labs(title = "Johnson-Neyman plot", x = modx, y = predl) +

      ggplot2::scale_color_manual(values = c("Significant" = "#00BFC4",
                                           "Insignificant" = "#F8766D"),
                                guide = "none") +
      theme_apa(legend.pos = "right", legend.font.size = 10) +

      ggplot2::theme(legend.key.size = ggplot2::unit(1, "lines"))


  out$plot <- plot

  class(out) <- "johnson_neyman"

  return(out)

}

#' @export

print.johnson_neyman <- function(x, ...) {

  atts <- attributes(x)

  # Describe whether sig values are inside/outside the interval
  if (atts$inside == FALSE) {
    inout <- "OUTSIDE"
  } else {
    inout <- "INSIDE"
  }

  x$bounds <- round(x$bounds, 4)
  atts$modrange <- round(atts$modrange, 4)
  alpha <- gsub("0\\.", "\\.", as.character(atts$alpha))
  pmsg <- paste("p <", alpha)

  # Print the output
  cat("JOHNSON-NEYMAN INTERVAL\n\n")
  cat("The slope of", atts$pred, "is", pmsg, "when", atts$modx,
      "is", inout, "this interval:\n")
  cat("[", x$bounds[1], ", ", x$bounds[2], "]\n", sep = "")
  cat("Note: The range of observed values of ", atts$modx, " is [", atts$modrange[1], ", ",
      atts$modrange[2], "]\n\n", sep = "")

  # If requested, print the plot
  if (atts$plot == TRUE) {
    print(x$plot)
  }

}

