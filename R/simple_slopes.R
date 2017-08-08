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
#' @param cond.int Should conditional intercepts be printed in addition to the
#'   slopes? Default is \code{FALSE}.
#'
#' @param johnson_neyman Should the Johnson-Neyman interval be calculated?
#'   Default is \code{TRUE}. This can be performed separately with
#'   \code{\link{johnson_neyman}}.
#'
#' @param jnplot Should the Johnson-Neyman interval be plotted as well? Default
#'   is \code{FALSE}.
#'
#' @param jnalpha What should the alpha level be for the Johnson-Neyman interval?
#'   Default is .05, which corresponds to a 95\% confidence interval.
#'
#' @param robust Logical. If \code{TRUE}, computes heteroskedasticity-robust
#'   standard errors.
#'
#' @param robust.type Type of heteroskedasticity-robust standard errors to use
#'   if \code{robust=TRUE}. See details of \code{\link{j_summ}} for more on
#'    options.
#'
#' @param digits How many significant digits after the decimal point should the
#'   output contain?
#'
#' @param n.sd How many standard deviations should be used if \code{standardize
#'   = TRUE}? Default is 1, but some prefer 2.
#'
#' @details This allows the user to perform a simple slopes analysis for the purpose
#'   of probing interaction effects in a linear regression. Two- and three-way
#'   interactions are supported, though one should be warned that three-way
#'   interactions are not easy to interpret in this way.
#'
#'   For more about Johnson-Neyman intervals, see \code{\link{johnson_neyman}}.
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
#' @return
#'
#'  \item{slopes}{A table of coefficients for the focal predictor at each value of
#'  the moderator}
#'  \item{ints}{A table of coefficients for the intercept at each value of the moderator}
#'  \item{modxvals}{The values of the moderator used in the analysis}
#'  \item{mods}{A list containing each regression model created to estimate the conditional
#'  coefficients.}
#'  \item{jn}{If \code{johnson_neyman = TRUE}, a list of `johnson_neyman`
#'  objects from \code{\link{johnson_neyman}}. These contain the values of the
#'  interval and the plots. If a 2-way interaction, the list will be of length
#'  1. Otherwise, there will be 1 `johnson_neyman` object for each value of the
#'  2nd moderator for 3-way interactions.}
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
#'  \url{http://dx.doi.org/10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied multiple
#' regression/correlation analyses for the behavioral sciences} (3rd ed.).
#' Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' @examples
#'
#' # Using a fitted model as formula input
#' fiti <- lm(Income ~ Frost + Murder*Illiteracy,
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
#' @importFrom stats coef coefficients lm predict sd update getCall vcov
#' @export
#'

sim_slopes <- function(model, pred, modx, mod2 = NULL, modxvals = NULL,
                       mod2vals = NULL, centered = NULL, standardize = FALSE,
                       cond.int = FALSE, johnson_neyman = TRUE, jnplot = FALSE,
                       jnalpha = .05, robust = FALSE, robust.type = "HC3",
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
  ss <- list()

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

  # weights?
  if (survey == FALSE && "(weights)" %in% names(d)) {
    weights <- TRUE
    wname <- as.character(model$call["weights"])
    colnames(d)[which(colnames(d) == "(weights)")] <- wname
  } else {
    weights <- FALSE
    wname <- NULL
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

      if (weights == FALSE) { # can have weights w/o svyglm

        modsd <- sd(d[,modx]) # save the SD
        # Now save a vector of those focal values
        modxvalssd <- c(mean(d[,modx]) + modsd,
                        mean(d[,modx]),
                        mean(d[,modx]) - modsd)
        # Name the vector for better labeling of the plot
        names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")

      } else { # Same thing, but with weights

        modsd <- wtd.sd(d[,modx], d[,wname])
        modxvalssd <- c(weighted.mean(d[,modx], d[,wname]) + modsd,
                        weighted.mean(d[,modx], d[,wname]),
                        weighted.mean(d[,modx], d[,wname]) - modsd)
        names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")

      }

      modxvals2 <- modxvalssd # Saving the values to a new object for use later
      ss <- structure(ss, def = TRUE)

    } else if (survey == TRUE) {

      modsd <- svysd(as.formula(paste("~", modx, sep = "")), design = design)
      # Have to construct the formula this way since the syntax for svymean
      # differs from mean
      modmean <- survey::svymean(as.formula(paste("~", modx, sep = "")),
                                 design = design)
      modxvalssd <- c(modmean + modsd,
                      modmean,
                      modmean - modsd)
      names(modxvalssd) <- c("+1 SD", "Mean", "-1 SD")

      modxvals2 <- modxvalssd
      ss <- structure(ss, def = TRUE)

    }
  } else if (!is.null(modxvals) && !is.factor(d[,modx]) &&
             length(unique(d[,modx])) > 2 && modxvals == "plus-minus") {

    if (survey == FALSE) {

      if (weights == FALSE) {

        modsd <- sd(d[,modx])
        modxvalssd <- c(mean(d[,modx]) + modsd,
                        mean(d[,modx]) - modsd)
        names(modxvalssd) <- c("+1 SD", "-1 SD")
        modxvals2 <- modxvalssd
        ss <- structure(ss, def = TRUE)

      } else {

        modsd <- wtd.sd(d[,modx], d[,wname])
        modxvalssd <- c(weighted.mean(d[,modx], d[,wname]) + modsd,
                        weighted.mean(d[,modx], d[,wname]) - modsd)
        names(modxvalssd) <- c("+1 SD", "-1 SD")
        modxvals2 <- modxvalssd
        ss <- structure(ss, def = TRUE)

      }

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
  if (!is.null(mod2)) { # Check whether there is 2nd moderator

  if (is.null(mod2vals) && !is.factor(d[,mod2]) &&
      length(unique(d[,mod2])) > 2) { # continuous mod2 w/ no specified vals

    if (survey == FALSE) {

      if (weights == FALSE) {
        mod2sd <- sd(d[,mod2])
        mod2valssd <- c(mean(d[,mod2]) + mod2sd,
                        mean(d[,mod2]),
                        mean(d[,mod2]) - mod2sd)
        names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                               paste("Mean of", mod2),
                               paste("Mean of", mod2, "-1 SD"))

      } else {

        mod2sd <- wtd.sd(d[,mod2], d[,wname])
        mod2valssd <- c(weighted.mean(d[,mod2], d[,wname]) + mod2sd,
                        weighted.mean(d[,mod2], d[,wname]),
                        weighted.mean(d[,mod2], d[,wname]) - mod2sd)
        names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                               paste("Mean of", mod2),
                               paste("Mean of", mod2, "-1 SD"))

      }

      mod2vals2 <- mod2valssd
      mod2vals2 <- sort(mod2vals2, decreasing = F)
      ss <- structure(ss, def2 = TRUE)

    } else if (survey == TRUE) {

      mod2sd <- svysd(as.formula(paste("~", mod2, sep = "")), design = design)
      mod2mean <- survey::svymean(as.formula(paste("~", mod2, sep = "")), design = design)
      mod2valssd <- c(mod2mean + mod2sd, mod2mean, mod2mean - mod2sd)
      names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                             paste("Mean of", mod2),
                             paste("Mean of", mod2, "-1 SD"))
      mod2vals2 <- mod2valssd
      mod2vals2 <- sort(mod2vals2, decreasing = F)
      ss <- structure(ss, def2 = TRUE)

    }
  } else if (!is.null(mod2vals) && mod2vals == "plus-minus" &&
             length(unique(d[,mod2])) > 2) {

    if (survey == FALSE) {

      if (weights == FALSE) {

        mod2sd <- sd(d[,mod2])
        mod2valssd <- c(mean(d[,mod2]) + mod2sd,
                        mean(d[,mod2]) - mod2sd)
        names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                               paste("Mean of", mod2, "-1 SD"))

      } else {

        mod2sd <- wtd.sd(d[,mod2])
        mod2valssd <- c(weighted.mean(d[,mod2], d[,wname]) + mod2sd,
                        weighted.mean(d[,mod2], d[,wname]) - mod2sd)
        names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                               paste("Mean of", mod2, "-1 SD"))

      }

      mod2vals2 <- mod2valssd
      mod2vals2 <- sort(mod2vals2, decreasing = F)
      ss <- structure(ss, def2 = TRUE)

    } else if (survey == TRUE) {

      mod2sd <- svysd(as.formula(paste("~", mod2, sep = "")), design = design)
      mod2mean <- survey::svymean(as.formula(paste("~", mod2, sep = "")), design = design)
      mod2valssd <- c(mod2mean+mod2sd, mod2mean-mod2sd)
      names(mod2valssd) <- c(paste("Mean of", mod2, "+1 SD"),
                             paste("Mean of", mod2, "-1 SD"))
      mod2vals2 <- mod2valssd
      mod2vals2 <- sort(mod2vals2, decreasing = F)
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

  # Create a list to hold Johnson-Neyman objects
  jns <- list()

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

    # We don't want to do the J-N interval with the 1st moderator adjusted,
    # so we do it here. Requires an extra model fit.
    if (johnson_neyman == TRUE) {

      if (survey == FALSE) {

        # Creating extra "copy" of model frame to change for model update
        dt <- d

        if (!is.null(mod2)) { # We *do* need to adjust the 2nd moderator for J-N

          # The moderator value-adjusted variable
          dt[,mod2] <- dt[,mod2] - mod2vals2[j]

        }

        # Creating the model
        newmod <- update(model, data=dt)

        # Getting SEs, robust or otherwise
        if (robust==TRUE) {

          # For J-N
          covmat <- sandwich::vcovHC(newmod, type = robust.type)

        } else {

          # For J-N
          covmat <- vcov(newmod)

        }

      } else if (survey == TRUE) {

        # Create new design to modify
        designt <- design

        # Create new df to modify
        dt <- d

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

        # For J-N
        covmat <- vcov(newmod)

      }

      jn <- tryCatch(johnson_neyman(newmod, pred = pred, modx = modx,
                                    vmat = covmat, plot = jnplot,
                                    alpha = jnalpha),
                     error = function(e) {return("No values")})
      if (j != 0) {
          jns[[j]] <- jn
      }

    }

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


    ss <- structure(ss, modxvals = modxvals2, robust = robust,
                    cond.int = cond.int, johnson_neyman = johnson_neyman,
                    jnplot = jnplot, jns = jns)

    ss$mods <- mods
    ss$jn <- jns

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

  # Is the plot wanted? Needed for 3-way interaction handling
  wantplot <- FALSE
  if (x$johnson_neyman == TRUE && !is.null(x$mod2)) {
    value <- any(sapply(x$jns,FUN = function(x) {class(x) == "johnson_neyman"}))
    if (value) {
      # Find where the valid ones are
      index <- which(sapply(x$jns,
                            FUN = function(x) {class(x) == "johnson_neyman"}))
      # Save the first one
      index <- index[1]
      wantplot <- attributes(x$jns[[index]])$plot
    } else {
      wantplot <- FALSE
    }
  }

  # This helps deal with the fact sometimes mod2vals has no length, so we want
  # to loop just once
  if (!is.null(x$mod2)) {
    length <- length(x$mod2vals)
  } else {
    length <- 1
  }

  # Need to make some things outside the loops for efficent grid plotting
  labs <- c()
  plots <- list()
  legend <- NULL

  # Loop through each value of second moderator...if none, just one loop
  for (j in 1:length) {

    # If we're using second moderator, need to make things make sense to inner loop
    if (!is.null(x$mod2)) {
      m <- NULL
      m$slopes <- ss$slopes[[j]]
      m$ints <- ss$ints[[j]]

      x$mod2vals <- round(x$mod2vals, x$digits)

      # Printing output to make it clear where each batch of second moderator
      # slopes begins
      if (x$def2 == FALSE) {
        cat("#######################################################\n")
        cat("While", x$mod2, "(2nd moderator)", "=", x$mod2vals[j], "\n")
        cat("#######################################################\n\n")
      } else {
        # If the user went with default +/- SD or used a factor variable,
        # we use the labels
        cat("#######################################################\n")
        cat("While ", x$mod2, " (2nd moderator)", " = ", x$mod2vals[j], " (",
            names(x$mod2vals)[j], ")", "\n", sep = "")
        cat("#######################################################\n\n")
      }


      if (x$johnson_neyman == TRUE) {

        # Handling occasions when there is no solution for the J-N interval
        if (x$jns[[j]][1] != "No values") {
          # For 3-way interactions, we don't want each plot being printed
          attributes(x$jns[[j]])$plot <- FALSE
          print(x$jns[[j]])
          skip <- FALSE # Giving a way to skip over instances with no J-N vals
        } else {
          cat("The Johnson-Neyman interval could not be found. Is your interaction term significant?\n\n")
          skip <- TRUE # Giving a way to skip over instances with no J-N vals
        }

        # Tell user we can't plot if they don't have cowplot installed
        if (wantplot == TRUE && !requireNamespace("cowplot", quietly = TRUE)) {
          msg <- "To plot Johnson-Neyman plots for 3-way interactions,
          you need the cowplot package."
          warning(msg)
        } else if (wantplot == TRUE && requireNamespace("cowplot",
                                                        quietly = TRUE)) {

          if (is.null(legend) && skip == FALSE) {
            # We save the legend the first time around to use w/ cowplot
            legend <- cowplot::get_legend(x$jns[[j]]$plot +
                                          theme_apa(legend.font.size = 8) +
                                          ggplot2::theme(legend.position = "top"))

            # Now we get rid of it for the actual plotting of the first plot
            x$jns[[j]]$plot <- x$jns[[j]]$plot +
              ggplot2::theme(legend.position = "none")

            # Save the legend to the first spot in the list of plots
            plots[[1]] <- legend
            # Since we have two columns, we reserve an empty spot in the short
            # first row by putting NULL in the second spot on the list
            plots[[2]] <- ggplot2::ggplot() + ggplot2::theme_void() # white background
            # We give these two plots empty labels
            labs <- c("","")
          } else if (skip == FALSE) {
            # For each subsequent plot, we don't need to save the legend,
            # just need to get rid of it
            x$jns[[j]]$plot <- x$jns[[j]]$plot +
              ggplot2::theme(legend.position = "none")
          }

          # Add a label for cowplot
          if (skip == FALSE) { # only if we aren't skipping it
            labs <- c(labs, paste(x$mod2, "=", x$mod2vals[j]))

            # Add the plot to the plot list at whatever the current end is
            index <- length(plots) + 1
            plots[[index]] <- x$jns[[j]]$plot
          }

        }
      }

    } else {
      m <- ss

      # If we don't have a three-way interaction, just do what the user asked
      # with regard to J-N plots
      if (x$johnson_neyman == TRUE) {
        if (x$jns[[j]][1] != "No values") {
          print(x$jns[[j]])
        } else {
          cat("The Johnson-Neyman interval could not be found. Is your interaction term significant?\n\n")
        }
      }

    }



    # Clearly label simple slopes
    cat("SIMPLE SLOPES ANALYSIS\n\n")

  for (i in 1:length(x$modxvals)) {

    # Use the labels for the automatic +/- 1 SD, factors
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

  # If 3-way interaction and the user has `cowplot`, here's where we make the
  # final output
  if (wantplot == TRUE && !is.null(x$mod2) &&
      requireNamespace("cowplot", quietly = TRUE) && length(plots) > 2) {
    # This makes the legend row smaller than the others
    sizes <- c(0.2, rep(1, times = (length(plots)-1)))
    # Now we put it all together--vjust is at a non-default level
    print(cowplot::plot_grid(plotlist = plots, labels = labs, label_size = 10,
                               align = "auto", ncol = 2, rel_heights = sizes,
                             vjust = 0, scale = 1))
  } else if (wantplot == TRUE && !is.null(x$mod2) && length(plots) == 2) {
    # If only 1 of the values of the 2nd moderator had defined J-N values, then
    # we don't want to use cowplot.

    # We want to give it its legend back before printing
    plots[[1]] <- plots[[1]] + ggplot2::theme(legend.position = "right")
    print(plots[[1]])
  }

}
