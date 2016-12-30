#' Perform a simple slopes analysis
#'
#' \code{sim_slopes()} conducts a simple slopes analysis for the purposes of understanding
#' interaction effects in linear regression.
#'
#' @param formula A regression formula, like would be provided to \code{lm()}, in
#'   quotation marks. This does not need to include the interaction term. Alternately,
#'   give an \code{lm} object and the formula will be extracted. Interaction terms
#'   with non-focal predictors will be ignored and entered as separate terms.
#'
#' @param data A data frame.
#'
#' @param pred The predictor variable involved in the interaction in quotes.
#'
#' @param modx The moderator variable involved in the interaction in quotes.
#'
#' @param modxvals For which values of the moderator should simple slopes analysis
#'   be performed? Default is \code{NULL}. If \code{NULL}, then the customary +/-
#'   1 standard deviation from the mean values are used. This would not be appropriate
#'   in the case of a binary moderator, however, as well as in some other use cases.
#'   There is no specific limit on the number of variables provided.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered. If
#'   \code{NULL}, all non-focal predictors are centered. If not \code{NULL}, only
#'   the user-specified predictors are centered.
#'
#' @param weights If weights are being used, provide the variable name where they are stored.
#'
#' @param robust Logical. If \code{TRUE}, computes heteroskedasticity-robust standard errors.
#'
#' @param robust.type Type of heteroskedasticity-robust standard errors to use if \code{robust=TRUE}.
#'   See details of \code{\link{j_summ}} for more on options.
#'
#' @param digits How many significant digits after the decimal point should the output
#'   contain?
#'
#' @details This allows the user to perform a simple slopes analysis for the purpose
#'   of probing interaction effects in a linear regression. Only two-way interactions
#'   are supported.
#'
#'   The function can accept either a character object specifying the formula to be
#'   tested or a \code{lm} object instead, from which the formula will be extracted. All
#'   interactions will be stripped from the formula, leaving only the specified
#'   interaction. The function refits the model, so other features of your fitted
#'   model will be ignored (like the standard errors).
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @seealso \code{\link{interact_plot}} accepts similar syntax and will plot the
#'   results with \code{\link[ggplot2]{ggplot}}.
#'
#'   \code{\link[rockchalk]{testSlopes}} performs a hypothesis test of differences
#'   and provides Johnson-Neyman intervals.
#'
#'   \code{\link[pequod]{simpleSlope}} performs a similar analysis and can analyze
#'   a second moderator.
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
#' # Using a fitted model as formula input
#' fit <- lm(Income ~ `HS Grad` + Murder + Illiteracy,
#'   data=as.data.frame(state.x77))
#' sim_slopes(formula=fit, data=as.data.frame(state.x77), pred="Murder", modx="Illiteracy")
#'
#' # Writing out formula
#' sim_slopes(formula="Income ~ `HS Grad` + Murder + Illiteracy",
#'   data=as.data.frame(state.x77), pred="Murder", modx="Illiteracy")
#'
#' @importFrom stats coef coefficients lm predict sd
#' @export sim_slopes

sim_slopes <- function(formula, data, pred, modx, modxvals = NULL, centered = NULL, weights = NULL, robust=FALSE, robust.type="HC3", digits = 3) {

  # Check arguments
  if (!is.numeric(digits)) { # digits
    stop("The digits argument must be an integer.")
  }

  if (!is.null(modxvals) && !is.vector(modxvals)) {
    stop("The modxvals argument must be a vector of at least length 2 if it used.")
  }

  d <- as.data.frame(data)

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
  }
  # Remove moderator from formula
  if (grepl(modx, formula)) {
    formula <- gsub(modx, "", as.character(formula))
  }

  # Removing the focal predictor
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
    # Centering the non-focal variables to make the slopes more interpretable (0 = mean)
    for (j in 1:ncol(d)) {
      if ((names(d)[j] %in% c(pred, resp, modx))==FALSE) {
        d[,j] <- as.vector((d[,j] - mean(d[,j])))
      }
    }
  }

  # Default to +/- 1 SD
  if (is.null(modxvals)) {
    modsd <- sd(d[,modx]) # the SD

    # Store the values
    modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx])-modsd)

    names(modxvalssd) <- c("+1 SD", "-1 SD")
    modxvals2 <- modxvalssd

  } else { #otherwise use given values
    modxvals2 <- modxvals
  }

  # Looping so any amount of moderator values can be used
  for (i in 1:length(modxvals2)) {

    # The moderator value-adjusted variable
    d$modp <- d[,modx] - modxvals2[i]

    # The lm() formula
    formulap <- paste(formula, " + modp*", pred, sep="")

    # Creating the model
    if (is.null(weights)) {
      modelp <- lm(formulap, data=d)
    } else {
      modelp <- lm(formulap, data=d, weights=weight)
    }

    # Getting SEs, robust or otherwise
    if (robust==TRUE) {

      # Hold things up if required packages not installed
      if (!requireNamespace("sandwich", quietly = TRUE)) {
        stop("When robust is set to TRUE, you need to have the \'sandwich\' package for robust standard errors.
           Please install it or set robust to FALSE.",
           call. = FALSE)
      }

      # Hold things up if required packages not installed
      if (!requireNamespace("lmtest", quietly = TRUE)) {
        stop("When robust is set to TRUE, you need to have the \'lmtest\' package for robust standard errors.
           Please install it or set robust to FALSE.",
           call. = FALSE)
      }

      slopep <- lmtest::coeftest(modelp, vcov=sandwich::vcovHC(modelp, type=robust.type))[pred,]
      intp <- lmtest::coeftest(modelp, vcov=sandwich::vcovHC(modelp, type=robust.type))["(Intercept)",]

    } else {

      slopep <- coef(summary(modelp))[pred,]
      intp <- coef(summary(modelp))["(Intercept)",]

    }

    # Use the labels for the automatic +/- 1 SD
    if (is.null(modxvals)) {

      cat("Slope of ", pred, " when ", modx, " = ", round(modxvalssd[i],digits), " (", names(modxvalssd)[i], ")", ": \n", sep="")
      print(round(slopep, digits))

      # Print conditional intercept
      cat("Conditional intercept"," when ", modx, " = ", round(modxvalssd[i],digits), " (", names(modxvalssd)[i], ")", ": \n", sep="")
      print(round(intp, digits))
      cat("\n")

    } else { # otherwise don't use labels

      cat("Slope of ", pred, " when ", modx, " = ", round(modxvals[i],digits), ": \n", sep="")
      print(round(slopep,digits))

      # Print conditional intercept
      cat("Conditional intercept", " when ", modx, " = ", round(modxvals[i],digits), ": \n", sep="")
      print(round(intp, digits))
      cat("\n")

    }
  }
}
