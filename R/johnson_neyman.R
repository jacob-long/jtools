#' Calculate Johnson-Neyman intervals for 2-way interactions
#'
#' \code{johnson_neyman} finds so-called "Johnson-Neyman" intervals for
#' understanding where simple slopes are significant in the context of
#' interactions in multiple linear regression.
#'
#' @param model A regression model. It is tested with `lm`, `glm`, and
#'    `svyglm` objects, but others may work as well. It should contain the
#'     interaction of interest. Be aware that just because the computations
#'     work, this does not necessarily mean the procedure is appropriate for
#'     the type of model you have.
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
#' @param control.fdr Logical. Use the procedure described in Esarey & Sumner
#'   (2017) to limit the false discovery rate? Default is FALSE. See details
#'   for more on this method.
#'
#' @param line.thickness How thick should the predicted line be? This is
#'   passed to `geom_path` as the `size` argument, but because of the way the
#'   line is created, you cannot use `geom_path` to modify the output plot
#'   yourself.
#'
#' @param df How should the degrees of freedom be calculated for the critical
#'   test statistic? Previous versions used the large sample approximation; if
#'   alpha was .05, the critical test statistic was 1.96 regardless of sample
#'   size and model complexity. The default is now "residual", meaning the same
#'   degrees of freedom used to calculate p values for regression coefficients.
#'   You may instead choose any number or "normal", which reverts to the
#'   previous behavior. The argument is not used if `control.fdr = TRUE`.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 2. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired
#'   number.
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
#'  One weakness of this method of probing interactions is that it is analagous
#'  to making multiple comparisons without any adjustment to the alpha level.
#'  Esarey & Sumner (2017) proposed a method for addressing this, which is
#'  implemented in the `interactionTest` package. This function implements that
#'  procedure with modifications to the `interactionTest` code (that package is
#'  not required to use this function). If you set `control.fdr = TRUE`, an
#'  alternative *t* statistic will be calculated based on your specified alpha
#'  level and the data. This will always be a more conservative test than when
#'  `control.fdr = FALSE`. The printed output will report the calculated
#'  critical *t* statistic.
#'
#'  This technique is not easily ported to 3-way interaction contexts. You could,
#'  however, look at the J-N interval at two different levels of a second
#'  moderator. This does forgo a benefit of the J-N technique, which is not
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
#'  \url{http://doi.org/10.1207/s15327906mbr4003_5}
#'
#' Esarey, J., & Sumner, J. L. (2017). Marginal effects in interaction models:
#'  Determining and controlling the false positive rate. Comparative Political
#'  Studies, 1–33. Advance online publication.
#'  \url{https://doi.org/10.1177/0010414017730080}
#'
#' Johnson, P.O. & Fay, L.C. (1950). The Johnson-Neyman technique, its theory
#'  and application. \emph{Psychometrika}, \emph{15}, 349-367.
#'  \url{http://doi.org/10.1007/BF02288864}
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
#' @importFrom stats vcov qt
#' @export
#'

johnson_neyman <- function(model, pred, modx, vmat = NULL, alpha = 0.05,
                           plot = TRUE, control.fdr = FALSE,
                           line.thickness = 0.5, df = "residual",
                           digits = getOption("jtools-digits", 2)) {

  # Parse unquoted variable names
  predt <- as.character(substitute(pred))
  modxt <- as.character(substitute(modx))

  # Doing a check so it works when called from inside a function
  if (!(predt %in% as.character(attr(terms(model), "variables")))) {
    pred <- as.character(eval(pred))
    modx <- as.character(eval(modx))
  } else {
    pred <- predt
    modx <- modxt
  }

  # Handling df argument
  if (df == "residual") {
    df <- df.residual(model)
  } else if (df == "normal") {
    df <- Inf
  } else if (!is.numeric(df)) {
    stop("df argument must be 'residual', 'normal', or a number.")
  }

  # Structure
  out <- list()
  out <- structure(out, pred = pred, modx = modx, alpha = alpha, plot = plot,
                   digits = digits, control.fdr = control.fdr)

  # Construct interaction term

  ## Hard to predict which order lm() will have the predictors in
  ### First, have to handle the weirdness of merMods
  if (inherits(model, "merMod")) {
    # first possible ordering
    intterm1 <- paste(pred, ":", modx, sep = "")
    # is it in the coef names?
    intterm1tf <- any(intterm1 %in% names(lme4::fixef(model)))
    # second possible ordering
    intterm2 <- paste(modx, ":", pred, sep = "")
    # is it in the coef names?
    intterm2tf <- any(intterm2 %in% names(lme4::fixef(model)))

    # Taking care of other business, creating coefs object for later
    coefs <- lme4::fixef(model)
  } else {
    # first possible ordering
    intterm1 <- paste(pred, ":", modx, sep = "")
    # is it in the coef names?
    intterm1tf <- any(intterm1 %in% names(coef(model)))
    # second possible ordering
    intterm2 <- paste(modx, ":", pred, sep = "")
    # is it in the coef names?
    intterm2tf <- any(intterm2 %in% names(coef(model)))

    # Taking care of other business, creating coefs object for later
    coefs <- coef(model)
  }

  ## Now we know which of the two is found in the coefficents
  # Using this to get the index of the TRUE one
  inttermstf <- c(intterm1tf, intterm2tf)
  intterms <- c(intterm1, intterm2) # Both names, want to keep one
  intterm <- intterms[which(inttermstf)] # Keep the index that is TRUE

  # Getting the range of the moderator
  modrange <- range(model.frame(model)[,modx])
  modrangeo <- range(model.frame(model)[,modx]) # for use later
  modsd <- sd(model.frame(model)[,modx]) # let's expand outside observed range
  modrange[1] <- modrange[1] - modsd
  modrange[2] <- modrange[2] + modsd

  alpha <- alpha / 2

  if (control.fdr == FALSE) {
    # Set critical t value
    tcrit <- qt(alpha, df = df)
    # Reverse the sign since it gives negative at these low vals
    tcrit <- abs(tcrit)
  } else if (control.fdr == TRUE) { # Use Esarey & Sumner (2017) correction

    ## Predictor beta
    predb <- coefs[pred]
    ## Interaction term beta
    intb <- coefs[intterm]

    ## Covariance matrix of betas
    vcovs <- vcov(model)
    ## Predictor variance
    vcov_pred <- vcovs[pred,pred]
    ## Interaction variance
    vcov_int <- vcovs[intterm,intterm]
    ## Predictor-Interaction covariance
    vcov_pred_int <- vcovs[pred,intterm]
    ## Generate sequence of numbers along range of moderator
    range_sequence <- seq(from = modrangeo[1], to = modrangeo[2],
                          by = (modrangeo[2] - modrangeo[1])/1000)

    ## Produces a sequence of marginal effects
    marginal_effects <- predb + intb*range_sequence
    ## SEs of those marginal effects
    me_ses <- sqrt(vcov_pred + (range_sequence^2) * vcov_int +
                     2 * range_sequence * vcov_pred_int)

    ## t-values across range of marginal effects
    ts <- marginal_effects / me_ses
    ## Residual DF
    df <- df.residual(model)
    ## Get the minimum p values used in the adjustment
    ps <- 2 * pmin(pt(ts, df = df), (1 - pt(ts, df = df)))
    ## Multipliers
    multipliers <- seq_along(marginal_effects) / length(marginal_effects)
    ## Order the pvals
    ps_o <- order(ps)

    ## Adapted from interactionTest package function fdrInteraction
    test <- 0
    i <- 1 + length(marginal_effects)

    while (test == 0 & i > 1) {

      i <- i - 1
      test <- min(ps[ps_o][1:i] <= multipliers[i] * (alpha * 2))

    }

    tcrit <- abs(qt(multipliers[i] * alpha, df))

  }

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
    y3 <- coefs[intterm]
    # Actual predictor coefficient (gamma_1)
    y1 <- coefs[pred]

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
    disc <- b^2 - 4 * a * c

    # If the discriminant is zero or something else, can't proceed.
    if (disc > 0) {
      out <- disc
    } else if (disc == 0) {
      # msg <- "There is only one real solution for the Johnson-Neyman interval.
      # Values cannot be supplied."
      # warning(msg)
      return(NULL)
    } else {
      # msg <- "There are no real solutions for the Johnson-Neyman interval.
      # Values cannot be supplied."
      # warning(msg)
      return(NULL)
    }

    return(out)
  }

  disc <- discriminant(a,b,c)
  # Create value for attribute containing info on success/non-success of
  # finding j-n interval analytically
  if (is.null(disc)) {
    failed <- TRUE
  } else {
    failed <- FALSE
  }


  # As long as the above didn't error, let's solve the quadratic with
  # this function
  quadsolve <- function(a,b,c, disc) {
    # first return value
    x1 <- (-b + sqrt(disc)) / (2 * a)
    # second return value
    x2 <- (-b - sqrt(disc)) / (2 * a)
    # return a vector of both values
    result <- c(x1, x2)
    # make sure they are in increasing order
    result <- sort(result, decreasing = FALSE)

    return(result)

  }

  if (!is.null(disc)) {
    bounds <- quadsolve(a,b,c, disc)
  } else {
    bounds <- c(-Inf, Inf)
  }
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

  # Generating values to feed to the CI function from the range
  x2 <- seq(from = modrange[1], to = modrange[2], length.out = 1000)

  # Make slopes colname
  predl <- paste("Slope of", pred)

  cbs <- cbands(x2, y1, y3, covy1, covy3, covy1y3, tcrit, predl, modx)

  out$bounds <- bounds
  out <- structure(out, modrange = modrangeo)

  # Need to check whether sig vals are within or outside bounds
  sigs <- which((cbs$Lower < 0 & cbs$Upper < 0) |
                  (cbs$Lower > 0 & cbs$Upper > 0))

  # Going to split cbands values into significant and insignificant pieces
  insigs <- setdiff(1:1000, sigs)

  # Create grouping variable in cbs
  cbs$Significance <- rep(NA, nrow(cbs))
  cbs$Significance <- factor(cbs$Significance, levels = c("Insignificant",
                                                          "Significant"))
  index <- 1:1000 %in% insigs
  cbs$Significance[index] <- "Insignificant"
  index <- 1:1000 %in% sigs
  cbs$Significance[index] <- "Significant"

  # Give user this little df
  out$cbands <- cbs

  # I'm looking for whether the significant vals are inside or outside
  ## Would like to find more elegant way to do it
  index <- which(cbs$Significance == "Significant")[1]

  if (!is.na(index) & index != 0) {
    inside <- (cbs[index,modx] > bounds[1] && cbs[index,modx] < bounds[2])
    all_sig <- NULL # Indicator for whether all values are either T or F

    # We don't know from this first test, so we do another check here
    if (is.na(which(cbs$Significance == "Insignificant")[1])) {
      all_sig <- TRUE
    } else {
      all_sig <- FALSE
    }

  } else {
    inside <- FALSE
    all_sig <- TRUE
  }


  out <- structure(out, inside = inside, failed = failed, all_sig = all_sig)

  # Splitting df into three pieces
  cbso1 <- cbs[cbs[,modx] < bounds[1],]
  cbso2 <- cbs[cbs[,modx] > bounds[2],]
  cbsi <- cbs[(cbs[,modx] > bounds[1] & cbs[,modx] < bounds[2]),]

  # Create label based on alpha level
  alpha <- alpha * 2 # Undoing what I did earlier
  alpha <- gsub("0\\.", "\\.", as.character(alpha))
  pmsg <- paste("p <", alpha)

  # Let's make a J-N plot
  plot <- ggplot2::ggplot() +

    ggplot2::geom_path(data = cbso1, ggplot2::aes(x = cbso1[,modx],
                                    y = cbso1[,predl],
                                    color = cbso1[,"Significance"]),
                       size = line.thickness) +

    ggplot2::geom_path(data = cbsi,
                       ggplot2::aes(x = cbsi[,modx], y = cbsi[,predl],
                                    color = cbsi[,"Significance"]),
                       size = line.thickness) +

    ggplot2::geom_path(data = cbso2,
                       ggplot2::aes(x = cbso2[,modx], y = cbso2[,predl],
                                    color = cbso2[,"Significance"]),
                       size = line.thickness) +

    ggplot2::geom_ribbon(data = cbso1,
                         ggplot2::aes(x = cbso1[,modx], ymin = cbso1[,"Lower"],
                                      ymax = cbso1[,"Upper"],
                                      fill = cbso1[,"Significance"]),
                         alpha = 0.2) +

    ggplot2::geom_ribbon(data = cbsi,
                         ggplot2::aes(x = cbsi[,modx], ymin = cbsi[,"Lower"],
                                      ymax = cbsi[,"Upper"],
                                      fill = cbsi[,"Significance"]),
                         alpha = 0.2) +

    ggplot2::geom_ribbon(data = cbso2,
                         ggplot2::aes(x = cbso2[,modx], ymin = cbso2[,"Lower"],
                                      ymax = cbso2[,"Upper"],
                                      fill = cbso2[,"Significance"]),
                         alpha = 0.2) +

    ggplot2::scale_fill_manual(values = c("Significant" = "#00BFC4",
                                          "Insignificant" = "#F8766D"),
                               labels = c("n.s.", pmsg),
                               breaks = c("Insignificant","Significant"),
                               drop = FALSE,
                               guide = ggplot2::guide_legend(order = 2)) +

    ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +

    ggplot2::geom_segment(ggplot2::aes(x = modrangeo[1], xend = modrangeo[2],
                                       y = 0, yend = 0,
                                       linetype = "Range of\nobserved\ndata"),
                          lineend = "square", size = 1.25) +

    # Adding this scale allows me to have consistent ordering
    ggplot2::scale_linetype_discrete(guide = ggplot2::guide_legend(order = 1))

    if (out$bounds[1] < modrange[1]) {
      # warning("The lower bound is outside the range of the plotted data")
    } else if (all_sig == FALSE) {
      plot <- plot +
        ggplot2::geom_vline(ggplot2::aes(xintercept = out$bounds[1]),
                                         linetype = 2, color = "#00BFC4")
    }

    if (out$bounds[2] > modrange[2]) {
      # warning("The upper bound is outside the range of the plotted data")
    } else if (all_sig == FALSE) {
      plot <- plot +
        ggplot2::geom_vline(ggplot2::aes(xintercept = out$bounds[2]),
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

  # Add FDR info
  if (control.fdr == TRUE) {
    out$t_value <- tcrit
  }

  class(out) <- "johnson_neyman"

  return(out)

}

#' @export
#' @importFrom crayon bold inverse underline

print.johnson_neyman <- function(x, ...) {

  atts <- attributes(x)

  # Describe whether sig values are inside/outside the interval
  if (atts$inside == FALSE) {
    inout <- inverse("OUTSIDE")
  } else {
    inout <- inverse("INSIDE")
  }

  b_format <- num_print(x$bounds, atts$digits)
  m_range <- num_print(atts$modrange, atts$digits)
  alpha <- gsub("0\\.", "\\.", as.character(atts$alpha))
  pmsg <- paste("p <", alpha)

  # Print the output
  cat(bold(underline("JOHNSON-NEYMAN INTERVAL")), "\n\n")
  if (all(is.finite(x$bounds))) {
    cat_wrap("When ", atts$modx, " is ", inout, " the interval [",
             b_format[1], ", ", b_format[2], "], the slope of ", atts$pred,
             " is ", pmsg, ".", brk = "\n\n")
    cat_wrap(italic("Note: The range of observed values of", atts$modx,
        "is "), "[", m_range[1], ", ", m_range[2], "]", brk = "\n\n")
  } else {
    cat_wrap("The Johnson-Neyman interval could not be found.
        Is the p value for your interaction term below
        the specified alpha?", brk = "\n\n")
  }
  if (atts$control.fdr == TRUE) {
    cat("Interval calculated using false discovery rate adjusted",
        "t =", num_print(x$t_value, atts$digits),
        "\n\n")
  }

  # If requested, print the plot
  if (atts$plot == TRUE) {
    print(x$plot)
  }

}
