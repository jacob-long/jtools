#' Perform a simple slopes analysis
#'
#' \code{sim_slopes()} conducts a simple slopes analysis for the purposes of
#' understanding two- and three-way interaction effects in linear regression.
#'
#' @param model A regression model of type \code{lm} or \code{\link[survey]{svyglm}}.
#'    It should contain the interaction of interest.
#'
#' @param pred The predictor variable involved in the interaction.
#'
#' @param modx The moderator variable involved in the interaction.
#'
#' @param mod2 Optional. The name of the second moderator
#'  variable involved in the interaction.
#'
#' @param modxvals For which values of the moderator should simple slopes analysis
#'   be performed? Default is \code{NULL}. If \code{NULL}, then the values will be
#'   the customary +/- 1 standard deviation from the mean as well as the mean itself.
#'   There is no specific limit on the number of variables provided. Factor variables
#'   are not particularly suited to simple slopes analysis, but you could have a
#'   numeric moderator with values of 0 and 1 and give \code{c(0,1)} to compare the
#'   slopes at the different conditions. Two-level factor variables are coerced
#'   to numeric 0/1 variables, but are not standardized/centered like they could
#'   be if your input data had a numeric 0/1 variable.
#'
#' @param mod2vals For which values of the second moderator should the plot be
#'   facetted by? That is, there will be a separate plot for each level of this
#'   moderator. Defaults are the same as \code{modxvals}.
#'
#' @param centered A vector of quoted variable names that are to be mean-centered. If
#'   \code{NULL}, all non-focal predictors are centered. If not \code{NULL}, only
#'   the user-specified predictors are centered. User can also use "none" or "all"
#'   arguments. The response variable is not centered unless specified directly.
#'
#' @param standardize Logical. Would you like to standardize the variables
#'   that are centered? Default is \code{FALSE}, but if \code{TRUE} it will
#'   standardize variables specified by the \code{centered} argument. Note that
#'   non-focal predictors are centered when \code{centered = NULL}, its default.
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
#' @param n.sd How many standard deviations should be used if \code{standardize
#'   = TRUE}? Default is 1, but some prefer 2.
#'
#' @details This allows the user to perform a simple slopes analysis for the purpose
#'   of probing interaction effects in a linear regression. Two- and three-way
#'   interactions are supported, though one should be warned that three-way
#'   interactions are not easy to interpret in this way.
#'
#'   The function accepts a \code{lm} object and uses it to recompute models with
#'   the moderating variable set to the levels requested. \code{\link[survey]{svyglm}}
#'    objects are also accepted, though users should be cautioned against using
#'   simple slopes analysis with non-linear models (\code{svyglm} also estimates
#'   linear models).
#'
#'   Factor moderators are coerced to a 0/1 numeric variable and are not centered,
#'   even when requested in arguments. To avoid this, modify your data to change
#'   the factor to a binary numeric variable. Factors with more than 2 levels
#'   trigger an error.
#'
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
#' @family interaction tools
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
#'
#' # Using a fitted model as formula input
#' fiti <- lm(Income ~ `HS Grad` + Murder*Illiteracy,
#'   data=as.data.frame(state.x77))
#' sim_slopes(model=fiti, pred=Murder, modx=Illiteracy)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' regmodel <- svyglm(api00~ell*meals,design=dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals)
#'
#' # 3-way with survey and factor input
#' regmodel <- svyglm(api00~ell*meals*sch.wide,design=dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals, mod2 = sch.wide)
#'
#' @importFrom stats coef coefficients lm predict sd update getCall
#' @export
#'

sim_slopes <- function(model, pred, modx, mod2 = NULL, modxvals = NULL,
                       mod2vals = NULL, centered = NULL, standardize = FALSE,
                       robust=FALSE, robust.type="HC3", cond.int = FALSE,
                       digits = 3, n.sd = 1) {

  # Allows unquoted variable names
  pred <- as.character(substitute(pred))
  modx <- as.character(substitute(modx))
  mod2 <- as.character(substitute(mod2))
  # To avoid unexpected behavior, need to un-un-parse mod2 when it is NULL
  if (length(mod2) == 0) {
    mod2 <- NULL
    mod2vals2 <- NULL
  }

  # Create object to return
  ss <- NULL

  # Check arguments
  if (!is.numeric(digits)) { # digits
    stop("The digits argument must be an integer.")
  }
  ss <- structure(ss, digits = digits)

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
    # for some reason I can't get rid of the "list" as first element
    fvars <- fvars[2:length(fvars)]

  } else {
    survey <- FALSE
  }

  # Pulling the name of the response variable for labeling
  formula <- formula(model)
  formula <- paste(formula[2],formula[1],formula[3])

  resp <- sub("(.*)(?=~).*", x=formula, perl=T, replacement="\\1")
  resp <- trimws(resp)

  # Saving key arguments as attributes of return object
  ss <- structure(ss, resp = resp, modx = modx, mod2 = mod2, pred = pred,
                  cond.int = cond.int)

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
  if (!is.null(centered) && centered != "all" && centered != "none"){
    # Need to handle surveys differently within this condition
    if (survey == FALSE) {
      d <- gscale(x = centered, data = d, center.only = !standardize, n.sd = n.sd)
    } else {
      design <- gscale(x = centered, data = design, center.only = !standardize,
                       n.sd = n.sd)
      d <- design$variables
    }

  } else if (!is.null(centered) && centered == "none") {

  } else if (!is.null(centered) && centered == "all") {
    # Need to handle surveys differently within this condition
    vars <- names(d)[!(names(d) %in% c(resp, wname))] # saving all vars expect response
    if (survey == FALSE) {
      d <- gscale(x = vars, data = d, center.only = !standardize, n.sd = n.sd)
    } else {
      # Need a different strategy for not iterating over unimportant vars for
      # survey designs
      ndfvars <- fvars[!(fvars %in% c(resp, wname))]
      if (length(ndfvars) > 0) {
        design <- gscale(x = ndfvars, data = design, center.only = !standardize,
                         n.sd = n.sd)
        d <- design$variables
      }
    }

  } else { # Center all non-focal
    # Centering the non-focal variables to make the slopes more interpretable
    vars <- names(d)[!(names(d) %in% c(pred, resp, modx, mod2, wname))]
    # Need to handle surveys differently within this condition
    if (survey == FALSE) {
      d <- gscale(x = vars, data = d, center.only = !standardize, n.sd = n.sd)
    } else {
      # Need a different strategy for not iterating over unimportant vars for
      # survey designs
      nfvars <- fvars[!(fvars %in% c(pred, resp, modx, mod2, wname))]
      if (length(nfvars) > 0) {
        design <- gscale(x = nfvars, data = design, center.only = !standardize,
                         n.sd = n.sd)
        d <- design$variables
      }
    }
  }

  # Default to +/- 1 SD unless modx is factor
  if (is.null(modxvals) && !is.factor(d[,modx]) && length(unique(d[,modx])) > 2) {
    if (survey == FALSE) {
      modsd <- sd(d[,modx])
      modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx]), mean(d[,modx])-modsd)
      names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")
      modxvals2 <- modxvalssd
      ss <- structure(ss, def = TRUE)
    } else if (survey == TRUE) {
      modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
      modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")), design = design)
      modxvalssd <- c(modmean+modsd, modmean, modmean-modsd)
      names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")
      modxvals2 <- modxvalssd
      ss <- structure(ss, def = TRUE)
    }
  } else if (!is.null(modxvals) && !is.factor(d[,modx]) &&
             length(unique(d[,modx])) > 2 && modxvals == "plus-minus") {
    if (survey == FALSE) {
      modsd <- sd(d[,modx])
      modxvalssd <- c(mean(d[,modx])+modsd, mean(d[,modx])-modsd)
      names(modxvalssd) <- c("+1 SD", "-1 SD")
      modxvals2 <- modxvalssd
      ss <- structure(ss, def = TRUE)
    } else if (survey == TRUE) {
      modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
      modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")), design = design)
      modxvalssd <- c(modmean+modsd, modmean-modsd)
      names(modxvalssd) <- c("+1 SD", "-1 SD")
      modxvals2 <- modxvalssd
      ss <- structure(ss, def = TRUE)
    }
  } else if (!is.factor(d[,modx]) && length(unique(d[,modx])) == 2) {
    # Detecting binary variable
    modxvals2 <- as.numeric(levels(factor(d[,modx])))
    ss <- structure(ss, def = FALSE)
  } else if (length(unique(d[,modx])) < 2) {
    msg <- "Every observation has the same value for the moderator. Did you enter
            your data correctly?"
    stop(msg)
  } else if (is.factor(d[,modx]) && length(levels(d[,modx])) == 2) {
    # We can work with a two-level factor
    names <- levels(d[,modx])
    condition <- suppressWarnings(all(is.na(as.numeric(levels(d[,modx])))))
    if (condition) {
      modxvals2 <- c(0,1)
    } else {
      modxvals2 <- sort(as.numeric(levels(d[,modx])), decreasing = FALSE)
    }
    d[,modx] <- as.numeric(d[,modx]) - 1
    names(modxvals2) <- names
    ss <- structure(ss, def = TRUE)
  } else if (is.factor(d[,modx]) && length(levels(d[,modx])) != 2) {
    stop("Factor moderators can only have 2 levels.")
  } else { # Use user-supplied values otherwise
    modxvals2 <- modxvals
    ss <- structure(ss, def = FALSE)
  }

  # Default to +/- 1 SD unless mod2 is factor
  if (!is.null(mod2)) {
  if (is.null(mod2vals) && !is.factor(d[,mod2]) &&
      length(unique(d[,mod2])) > 2) {
    if (survey == FALSE) {
      mod2sd <- sd(d[,mod2])
      mod2valssd <- c(mean(d[,mod2])+mod2sd, mean(d[,mod2]), mean(d[,mod2])-mod2sd)
      names(mod2valssd) <- c("+1 SD", "Mean", "-1 SD")
      mod2vals2 <- mod2valssd
      ss <- structure(ss, def2 = TRUE)
    } else if (survey == TRUE) {
      mod2sd <- svysd(as.formula(paste("~", mod2, sep = "")), design = design)
      mod2mean <- survey::svymean(as.formula(paste("~", mod2, sep = "")), design = design)
      mod2valssd <- c(mod2mean+mod2sd, mod2mean, mod2mean-mod2sd)
      names(mod2valssd) <- c("+1 SD", "Mean", "-1 SD")
      mod2vals2 <- mod2valssd
      ss <- structure(ss, def2 = TRUE)
    }
  } else if (!is.factor(d[,mod2]) && length(unique(d[,mod2])) == 2) {
    # Detecting binary variable
    mod2vals2 <- as.numeric(levels(factor(d[,mod2])))
    ss <- structure(ss, def2 = FALSE)
  } else if (length(unique(d[,mod2])) < 2) {
    msg <- "Every observation has the same value for the 2nd moderator. Did you
            enter your data correctly?"
    stop(msg)
  } else if (is.factor(d[,mod2]) && length(levels(d[,mod2])) == 2) {
    # We can work with a two-level factor
    names <- levels(d[,mod2])
    condition <- suppressWarnings(all(is.na(as.numeric(levels(d[,mod2])))))
    if (condition) {
      mod2vals2 <- c(0,1)
    } else {
      mod2vals2 <- sort(as.numeric(levels(d[,mod2])), decreasing = FALSE)
    }
    d[,mod2] <- as.numeric(d[,mod2]) - 1
    names(mod2vals2) <- names
    ss <- structure(ss, def2 = TRUE)
  } else if (is.factor(d[,mod2]) && length(levels(d[,mod2])) != 2) {
    stop("Factor moderators can only have 2 levels.")
  } else {
    # Use user-supplied values otherwise
    mod2vals2 <- mod2vals
    ss <- structure(ss, def2 = FALSE)
  }
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
  mods <- list()

  # Make empty list to hold above list if 2nd mod used
  if (!is.null(mod2)) {

    # Make empty list to put each matrix into
    mats <- list()
    imats <- list()
  }

  # Looping through (perhaps non-existent)
  for (j in 1:length(mod2vals2)) {

  # Looping so any amount of moderator values can be used
  for (i in 1:length(modxvals2)) {

    # Update works differently for svyglm objects, so needs to be done separately
    if (survey == FALSE) {

      # Creating extra "copy" of model frame to change for model update
      dt <- d

      # The moderator value-adjusted variable
      dt[,modx] <- dt[,modx] - modxvals2[i]

      if (!is.null(mod2)) {

        # The moderator value-adjusted variable
        dt[,mod2] <- dt[,mod2] - mod2vals2[j]


      }

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

      if (!is.null(mod2)) {

        # The moderator value-adjusted variable
        dt[,mod2] <- dt[,mod2] - mod2vals2[j]

      }

      # Update design
      designt$variables <- dt

      # Update model
      ## Have to do all this to avoid adding survey to dependencies
      call <- getCall(model)
      call$design <- designt
      call[[1]] <- survey::svyglm
      newmod <- eval(call)

      # Get the coefs
      sum <- j_summ(newmod)
      summat <- sum$coeftable

      slopep <- summat[pred,c("Est.","S.E.","p")]
      intp <- summat["(Intercept)",c("Est.","S.E.","p")]

      retmat[i,1] <- modxvals2[i]
      retmat[i,2:4] <- slopep[]

      retmati[i,1] <- modxvals2[i]
      retmati[i,2:4] <- intp[]

    }

    mods <- list(mods, newmod)

  }

    if (!is.null(mod2)) {
      mats[[j]] <- retmat
      imats[[j]] <- retmati

      # Now reset the return matrices
      holdvals <- rep(NA, length(modxvals2)*4)
      retmat <- matrix(holdvals, nrow=length(modxvals2))

      # Create another matrix to hold intercepts (no left-hand column needed)
      retmati <- retmat

      # Value labels
      colnames(retmat) <- c(paste("Value of ", modx, sep = ""), "Est.", "S.E.", "p")
      colnames(retmati) <- c(paste("Value of ", modx, sep = ""), "Est.", "S.E.", "p")

    }

  } # end mod2 loop


    ss <- structure(ss, modxvals = modxvals2, robust = robust, cond.int = cond.int)

    ss$mods <- mods

    if (!is.null(mod2)) {
      ss$slopes <- mats
      ss$ints <- imats
      ss <- structure(ss, mod2vals = mod2vals2)
    } else {
      ss$slopes <- retmat
      ss$ints <- retmati
    }

    class(ss) <- "sim_slopes"
    return(ss)

  }



#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.sim_slopes <- function(x, ...) {

  # This is to make refactoring easier after switch to attributes
  ss <- x
  x <- attributes(x)

  # This helps deal with the fact sometimes mod2vals has no length, so we want
  # to loop just once
  if (!is.null(x$mod2)) {
    length <- length(x$mod2vals)
  } else {
    length <- 1
  }

  # Loop through each value of second moderator...if none, just one loop
  for (j in 1:length) {

    # If we're using second moderator, need to make things make sense to inner loop
    if (!is.null(x$mod2)) {
      m <- NULL
      m$slopes <- ss$slopes[[j]]
      m$ints <- ss$ints[[j]]

      # Printing output to make it clear where each batch of second moderator
      # slopes begins
      if (x$def2 == FALSE) {
        cat("#######################################################\n")
        cat("While", x$mod2, "(2nd moderator)", "=", x$mod2vals[j], "\n")
        cat("#######################################################\n\n")
      } else {
        cat("#######################################################\n")
        cat("While ", x$mod2, " (2nd moderator)", " = ", x$mod2vals[j], " (",
            names(x$mod2vals)[j], ")", "\n", sep = "")
        cat("#######################################################\n\n")
      }

    } else {
      m <- ss
    }

  for (i in 1:length(x$modxvals)) {

    # Use the labels for the automatic +/- 1 SD
    if (x$def == TRUE) {

      cat("Slope of ", x$pred, " when ", x$modx, " = ",
          round(x$modxvals[i],x$digits), " (", names(x$modxvals)[i], ")",
          ": \n", sep="")
      print(round(m$slopes[i,2:4], x$digits))

      # Print conditional intercept
      if (x$cond.int == TRUE) {
        cat("Conditional intercept"," when ", x$modx, " = ",
            round(x$modxvals[i],x$digits), " (", names(x$modxvals)[i], ")",
            ": \n", sep="")
        print(round(m$ints[i,2:4], x$digits))
        cat("\n")
      } else {cat("\n")}

    } else { # otherwise don't use labels

      cat("Slope of ", x$pred, " when ", x$modx, " = ",
          round(x$modxvals[i],x$digits),
          ": \n", sep="")
      print(round(m$slopes[i,2:4],x$digits))

      # Print conditional intercept
      if (x$cond.int == TRUE) {
        cat("Conditional intercept", " when ", x$modx, " = ",
            round(x$modxvals[i],x$digits), ": \n", sep="")
        print(round(m$ints[i,2:4], x$digits))
        cat("\n")
      } else {cat("\n")}

    }
  }
  } # end mod2 loop
}
