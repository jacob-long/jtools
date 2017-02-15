#' Perform a simple slopes analysis
#'
#' \code{sim_slopes()} conducts a simple slopes analysis for the purposes of understanding
#' interaction effects in linear regression.
#'
#' @param model A regression model of type \code{lm} or \code{\link[survey]{svyglm}}.
#'    It should contain the interaction of interest.
#'
#' @param pred The predictor variable involved in the interaction.
#'
#' @param modx The moderator variable involved in the interaction.
#'
#' @param modxvals For which values of the moderator should simple slopes analysis
#'   be performed? Default is \code{NULL}. If \code{NULL}, then the values will be
#'   the customary +/- 1 standard deviation from the mean as well as the mean itself.
#'   There is no specific limit on the number of variables provided. Factor variables
#'   are not particularly suited to simple slopes analysis, but you could have a
#'   numeric moderator with values of 0 and 1 and give \code{c(0,1)} to compare the
#'   slopes at the different conditions.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered. If
#'   \code{NULL}, all non-focal predictors are centered. If not \code{NULL}, only
#'   the user-specified predictors are centered. User can also use "none" or "all"
#'   arguments.
#'
#' @param robust Logical. If \code{TRUE}, computes heteroskedasticity-robust standard errors.
#'
#' @param robust.type Type of heteroskedasticity-robust standard errors to use if \code{robust=TRUE}.
#'   See details of \code{\link{j_summ}} for more on options.
#'
#' @param cond.int Should conditional intercepts be printed in addition to the slopes?
#'   Default is \code{FALSE}.
#'
#' @param digits How many significant digits after the decimal point should the output
#'   contain?
#'
#' @details This allows the user to perform a simple slopes analysis for the purpose
#'   of probing interaction effects in a linear regression. Only two-way interactions
#'   are supported.
#'
#'   The function accepts a \code{lm} object and uses it to recompute models with
#'   the moderating variable set to the levels requested. \code{\link[survey]{svyglm}}
#'    objects are also accepted, though users should be cautioned against using
#'   simple slopes analysis with non-linear models (\code{svyglm} also estimates
#'   linear models).
#'
#' @return
#'
#'  \item{slopes}{A table of coefficients for the focal predictor at each value of
#'  the moderator}
#'  \item{ints}{A table of coefficents for the intercept at each value of the moderator}
#'  \item{modxvals}{The values of the moderator used in the analysis}
#'  \item{mods}{A list containing each regression model created to estimate the conditional
#'  coefficients.}
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
#' fiti <- lm(Income ~ `HS Grad` + Murder*Illiteracy,
#'   data=as.data.frame(state.x77))
#' sim_slopes(model=fiti, pred=Murder, modx=Illiteracy)
#'
#'
#' @importFrom stats coef coefficients lm predict sd update
#' @export
#'

sim_slopes <- function(model, pred, modx, modxvals = NULL, centered = NULL,
                       robust=FALSE, robust.type="HC3", cond.int = FALSE,
                       digits = 3) {

  # Allows unquoted variable names
  pred <- as.character(substitute(pred))
  modx <- as.character(substitute(modx))

  # Create object to return
  ss <- NULL

  # Check arguments
  if (!is.numeric(digits)) { # digits
    stop("The digits argument must be an integer.")
  }
  ss$digits <- digits

  if (!is.null(modxvals) && !is.vector(modxvals)) {
    stop("The modxvals argument must be a vector of at least length 2 if it is used.")
  }

  # Save data from model object
  d <- model.frame(model)

  if (is.factor(d[,modx])){
    stop("Factor variables are not supported. You can try using a binary numeric variable and set modxvals = c(0,1), however.")
  }

  # Deal with svyglm objects
  if (class(model)[1] == "svyglm" || class(model)[1] == "svrepglm") {
    survey <- TRUE

    design <- model$survey.design
    d <- design$variables

    # Focal vars so the weights don't get centered
    fvars <- as.character(attributes(model$terms)$variables)

  } else {
    survey <- FALSE
  }

  # Pulling the name of the response variable for labeling
  formula <- formula(model)
  formula <- paste(formula[2],formula[1],formula[3])

  resp <- sub("(.*)(?=~).*", x=formula, perl=T, replacement="\\1")
  resp <- trimws(resp)

  ss$resp <- resp
  ss$modx <- modx
  ss$pred <- pred

  # weights?
  if (survey == FALSE && "(weights)" %in% names(d)) {
    weights <- TRUE
    wname <- as.character(model$call["weights"])
    colnames(d)[which(colnames(d) == "(weights)")] <- wname
  } else {
    weights <- FALSE
    wname <- NULL
  }

  # Handling user-requested centered vars
  if (!is.null(centered) && (centered != "all" || centered != "none")){
    for (var in centered) {
      if (survey == FALSE && !is.factor(d[,var])) {
        d[,var] <- d[,var] - mean(d[,var])
      } else if (!is.factor(d[,var])) {
        d[,var] <- d[,var] - survey::svymean(as.formula(paste("~", var, sep = "")),
                                             design = design)
      }
    }
  } else if (!is.null(centered) && centered == "none") {

  } else if (!is.null(centered) && centered == "all") {
    for (var in names(d)) {
      if (survey == FALSE && !is.factor(d[,var]) && var != wname) {
        d[,var] <- d[,var] - mean(d[,var])
      } else if (is.numeric(d[,var]) && var %in% fvars) {
        d[,var] <- d[,var] - survey::svymean(as.formula(paste("~", var, sep = "")),
                                             design = design)
      }
    }
  } else { # Center all non-focal
    # Centering the non-focal variables to make the slopes more interpretable (0 = mean)
    for (j in 1:ncol(d)) {
      if ((names(d)[j] %in% c(pred, resp, modx, wname))==FALSE &&
          is.numeric(d[,j]) && survey == FALSE) {
        d[,j] <- as.vector((d[,j] - mean(d[,j])))
      } else if (survey == TRUE && fvars[j] %in% c(pred, resp, modx) == FALSE &&
         names(d)[j] %in% fvars && is.numeric(d[,j])) {
        d[,j] <- as.vector(d[,j] - survey::svymean(as.formula(paste("~", names(d)[j],
                                                                     sep = "")),
                                                    design = design))
      }
    }
  }

  # Default to +/- 1 SD unless modx is factor
  if (is.null(modxvals) && !is.factor(d[,modx])) {
    if (survey == FALSE) {
      modsd <- sd(d[,modx])
      modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx]), mean(d[,modx])-modsd)
      names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")
      modxvals2 <- modxvalssd
      ss$def <- TRUE
    } else if (survey == TRUE) {
      modsd <- sqrt(survey::svyvar(as.formula(paste("~", modx, sep = "")), design = design))
      modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")), design = design)
      modxvalssd <- c(modmean+modsd, modmean, modmean-modsd)
      names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")
      modxvals2 <- modxvalssd
      ss$def <- TRUE
    }
  } else { # Use user-supplied values otherwise
    modxvals2 <- modxvals
    ss$def <- FALSE
  }

  # Need to make a matrix filled with NAs to store values from looped model-making
  holdvals <- rep(NA, length(modxvals2)*4)
  retmat <- matrix(holdvals, nrow=length(modxvals2))

  # Create another matrix to hold intercepts (no left-hand column needed)
  retmati <- retmat

  # Value labels
  colnames(retmat) <- c(paste("Value of ", modx, sep = ""), "Est.", "S.E.", "p")
  colnames(retmati) <- c(paste("Value of ", modx, sep = ""), "Est.", "S.E.", "p")

  # Make empty list to put actual models into
  mods <- as.list(rep(NA, length(modxvals2)))

  # Looping so any amount of moderator values can be used
  for (i in 1:length(modxvals2)) {

    # Update works differently for svyglm objects, so needs to be done separately
    if (survey == FALSE) {

      # Creating extra "copy" of model frame to change for model update
      dt <- d

      # The moderator value-adjusted variable
      dt[,modx] <- dt[,modx] - modxvals2[i]

      # Creating the model
      newmod <- update(model, data=dt)

      # Getting SEs, robust or otherwise
      if (robust==TRUE) {

        # Use j_summ to get the coefficients
        sum <- jtools::j_summ(newmod, robust = T, robust.type = robust.type)
        summat <- sum$coeftable

        slopep <- summat[pred,c("Est.","S.E.","p")]
        intp <- summat["(Intercept)",c("Est.","S.E.","p")]

        retmat[i,1] <- modxvals2[i]
        retmat[i,2:4] <- slopep[]

        retmati[i,1] <- modxvals2[i]
        retmati[i,2:4] <- intp[]

      } else {

        sum <- jtools::j_summ(newmod)
        summat <- sum$coeftable

        slopep <- summat[pred,c("Est.","S.E.","p")]
        intp <- summat["(Intercept)",c("Est.","S.E.","p")]

        retmat[i,1] <- modxvals2[i]
        retmat[i,2:4] <- slopep[]

        retmati[i,1] <- modxvals2[i]
        retmati[i,2:4] <- intp[]

      }

    } else if (survey == TRUE) {

      # Create new design to modify
      designt <- design

      # Create new df to modify
      dt <- d

      # Set the moderator at the given value
      dt[,modx] <- dt[,modx] - modxvals2[i]

      # Update design
      designt$variables <- dt

      # Update model
      newmod <- update(model, design = designt)

      # Get the coefs
      sum <- jtools::j_summ(newmod)
      summat <- sum$coeftable

      slopep <- summat[pred,c("Est.","S.E.","p")]
      intp <- summat["(Intercept)",c("Est.","S.E.","p")]

      retmat[i,1] <- modxvals2[i]
      retmat[i,2:4] <- slopep[]

      retmati[i,1] <- modxvals2[i]
      retmati[i,2:4] <- intp[]

    }

    mods[[i]] <- newmod

  }

    ss$slopes <- retmat
    ss$ints <- retmati
    ss$modxvals <- modxvals2

    ss$robust <- robust
    ss$cond.int <- cond.int

    ss$mods <- mods

    class(ss) <- "sim_slopes"
    return(ss)

  }



#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.sim_slopes <- function(x, ...) {

  for (i in 1:length(x$modxvals)) {

    # Use the labels for the automatic +/- 1 SD
    if (x$def == TRUE) {

      cat("Slope of ", x$pred, " when ", x$modx, " = ",
          round(x$modxvals[i],x$digits), " (", names(x$modxvals)[i], ")",
          ": \n", sep="")
      print(round(x$slopes[i,2:4], x$digits))

      # Print conditional intercept
      if (x$cond.int == TRUE) {
        cat("Conditional intercept"," when ", x$modx, " = ",
            round(x$modxvals[i],x$digits), " (", names(x$modxvals)[i], ")",
            ": \n", sep="")
        print(round(x$ints[i,2:4], x$digits))
        cat("\n")
      } else {cat("\n")}

    } else { # otherwise don't use labels

      cat("Slope of ", x$pred, " when ", x$modx, " = ", round(x$modxvals[i],x$digits),
          ": \n", sep="")
      print(round(x$slopes[i,2:4],x$digits))

      # Print conditional intercept
      if (x$cond.int == TRUE) {
        cat("Conditional intercept", " when ", x$modx, " = ",
            round(x$modxvals[i],x$digits), ": \n", sep="")
        print(round(x$ints[i,2:4], x$digits))
        cat("\n")
      } else {cat("\n")}

    }
  }
}
