#' Perform a simple slopes analysis.
#'
#' \code{sim_slopes} conducts a simple slopes analysis for the purposes of
#' understanding two- and three-way interaction effects in linear regression.
#'
#' @param model A regression model of type \code{lm} or
#'    \code{\link[survey]{svyglm}}.
#'    It should contain the interaction of interest.
#'
#' @param pred The predictor variable involved in the interaction.
#'
#' @param modx The moderator variable involved in the interaction.
#'
#' @param mod2 Optional. The name of the second moderator
#'  variable involved in the interaction.
#'
#' @param modxvals For which values of the moderator should simple slopes
#'   analysis be performed? Default is \code{NULL}. If \code{NULL}, then the
#'   values will be the customary +/- 1 standard deviation from the mean as
#'   well as the mean itself. There is no specific limit on the number of
#'   variables provided. If
#'   \code{"plus-minus"}, uses just +/- 1 standard
#'   deviation without the mean. You may also choose `"terciles"` to split
#'   the data into equally-sized groups and choose the point at the mean of
#'   each of those groups.
#'
#'   Factor variables
#'   are not particularly suited to simple slopes analysis, but you could have
#'   a numeric moderator with values of 0 and 1 and give \code{c(0,1)} to
#'   compare the slopes at the different conditions. Two-level factor
#'   variables are coerced to numeric 0/1 variables, but are not
#'   standardized/centered like they could be if your input data had a numeric
#'   0/1 variable.
#'
#' @param mod2vals For which values of the second moderator should the plot be
#'   facetted by? That is, there will be a separate plot for each level of this
#'   moderator. Defaults are the same as \code{modxvals}.
#'
#' @param centered A vector of quoted variable names that are to be
#'   mean-centered. If \code{NULL}, all non-focal predictors are centered. If
#'   not \code{NULL}, only the user-specified predictors are centered. User
#'   can also use "none" or "all" arguments. The response variable is not
#'   centered unless specified directly.
#'
#' @param scale Logical. Would you like to standardize the variables
#'   that are centered? Default is \code{FALSE}, but if \code{TRUE} it will
#'   scale variables specified by the \code{centered} argument. Note that
#'   non-focal predictors are centered when \code{centered = NULL}, its
#'   default.
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
#' @param jnalpha What should the alpha level be for the Johnson-Neyman
#'   interval? Default is .05, which corresponds to a 95\% confidence interval.
#'
#' @param robust Logical. If \code{TRUE}, computes heteroskedasticity-robust
#'   standard errors.
#'
#' @param robust.type Type of heteroskedasticity-robust standard errors to use
#'   if \code{robust=TRUE}. See details of \code{\link{j_summ}} for more on
#'   options.
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 2. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired
#'   number.
#'
#' @param n.sd How many standard deviations should be used if \code{scale
#'   = TRUE}? Default is 1, but some prefer 2.
#'
#' @param standardize Deprecated. Equivalent to `scale`. Please change your
#'  scripts to use `scale` instead as this argument will be removed in the
#'  future.
#'
#' @param ... Arguments passed to \code{\link{johnson_neyman}}.
#'
#' @details This allows the user to perform a simple slopes analysis for the
#'   purpose of probing interaction effects in a linear regression. Two- and
#'   three-way interactions are supported, though one should be warned that
#'   three-way interactions are not easy to interpret in this way.
#'
#'   For more about Johnson-Neyman intervals, see \code{\link{johnson_neyman}}.
#'
#'   The function accepts a \code{lm} object and uses it to recompute models
#'   with the moderating variable set to the levels requested.
#'   \code{\link[survey]{svyglm}} objects are also accepted, though users
#'   should be cautioned against using simple slopes analysis with non-linear
#'   models (\code{svyglm} also estimates linear models).
#'
#'   Factor moderators are coerced to a 0/1 numeric variable and are not
#'   centered, even when requested in arguments. To avoid this, modify your
#'   data to change the factor to a binary numeric variable. Factors with more
#'   than 2 levels trigger an error.
#'
#' @return
#'
#'  \item{slopes}{A table of coefficients for the focal predictor at each
#'  value of the moderator}
#'  \item{ints}{A table of coefficients for the intercept at each value of the
#'    moderator}
#'  \item{modxvals}{The values of the moderator used in the analysis}
#'  \item{mods}{A list containing each regression model created to estimate
#'     the conditional coefficients.}
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
#'   \code{\link[rockchalk]{testSlopes}} performs a hypothesis test of
#'       differences and provides Johnson-Neyman intervals.
#'
#'   \code{\link[pequod]{simpleSlope}} performs a similar analysis and can
#'        also analyze a second moderator.
#'
#' @references
#'
#' Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
#'  multilevel regression: Inferential and graphical techniques.
#'  \emph{Multivariate Behavioral Research}, \emph{40}(3), 373-400.
#'  \url{http://dx.doi.org/10.1207/s15327906mbr4003_5}
#'
#' Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). \emph{Applied
#' multiple regression/correlation analyses for the behavioral sciences} (3rd
#' ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.
#'
#' @examples
#'
#' # Using a fitted model as formula input
#' fiti <- lm(Income ~ Frost + Murder * Illiteracy,
#'   data=as.data.frame(state.x77))
#' sim_slopes(model = fiti, pred = Murder, modx = Illiteracy)
#'
#' # With svyglm
#' library(survey)
#' data(api)
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                     data = apistrat, fpc = ~fpc)
#' regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals)
#'
#' # 3-way with survey and factor input
#' regmodel <- svyglm(api00 ~ ell * meals * sch.wide, design = dstrat)
#' sim_slopes(regmodel, pred = ell, modx = meals, mod2 = sch.wide)
#'
#' @importFrom stats coef coefficients lm predict sd update getCall vcov
#' @export
#'

sim_slopes <- function(model, pred, modx, mod2 = NULL, modxvals = NULL,
                       mod2vals = NULL, centered = NULL, scale = FALSE,
                       cond.int = FALSE, johnson_neyman = TRUE, jnplot = FALSE,
                       jnalpha = .05, robust = FALSE, robust.type = "HC3",
                       digits = getOption("jtools-digits", default = 2),
                       n.sd = 1, standardize = NULL, ...) {

  # Allows unquoted variable names
  pred <- as.character(substitute(pred))
  modx <- as.character(substitute(modx))
  mod2 <- as.character(substitute(mod2))
  # To avoid unexpected behavior, need to un-un-parse mod2 when it is NULL
  if (length(mod2) == 0) {
    mod2 <- NULL
    mod2vals2 <- NULL
  }

  # Check for deprecated argument
  if (!is.null(standardize)) {
    warning("The standardize argument is deprecated. Please use 'scale'",
      " instead.")
    scale <- standardize
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

  # Is it a svyglm?
  if (class(model)[1] == "svyglm" || class(model)[1] == "svrepglm") {

    survey <- TRUE
    design <- model$survey.design
    d <- design$variables

    # Focal vars so the weights don't get centered
    fvars <- as.character(attributes(terms(model))$variables)
    # for some reason I can't get rid of the "list" as first element
    fvars <- fvars[2:length(fvars)]
    all.vars <- fvars

    facvars <- c()
    for (v in fvars) {
      if (is.factor((d[,v])) && length(unique(d[,v])) > 2) {
        facvars <- c(facvars, v)
      }
    }

  } else {

    survey <- FALSE
    design <- NULL

    fvars <- as.character(attributes(terms(model))$variables)
    # for some reason I can't get rid of the "list" as first element
    fvars <- fvars[2:length(fvars)]
    all.vars <- fvars

    facvars <- c()
    for (v in fvars) {
      if (is.factor((d[,v])) && length(unique(d[,v])) > 2) {
        facvars <- c(facvars, v)
      }
    }

  }

  # weights?
  if (survey == FALSE && ("(weights)" %in% names(d) |
                          !is.null(getCall(model)$weights))) {
    weights <- TRUE
    wname <- as.character(getCall(model)["weights"])
    if (any(colnames(d) == "(weights)")) {
      colnames(d)[which(colnames(d) == "(weights)")] <- wname
    }
    wts <- d[,wname]

  } else {

    weights <- FALSE
    wname <- NULL
    wts <- rep(1, times = nrow(d))

  }

  # offset?
  if (!is.null(model.offset(model.frame(model)))) {

    off <- TRUE
    offname <- as.character(getCall(model)$offset[-1]) # subset gives bare varname

    # Getting/setting offset name depending on whether it was specified in
    # argument or formula
    if (any(colnames(d) == "(offset)") & !is.null(offname)) {
      colnames(d)[which(colnames(d) == "(offset)")] <- offname
    } else if (any(colnames(d) == "(offset)") & is.null(offname)) {

      offname <- "(offset)"

      # This strategy won't work for svyglm
      if (survey == TRUE) {

        stop("For svyglm with offsets, please specify the offset with the
             'offset =' argument rather than in the model formula.")

      }

    }

    # See if offset term was logged
    if (offname == "(offset)") {
      offterm <- regmatches(as.character(formula(model)),
                            regexpr("(?<=(offset\\()).*(?=(\\)))",
                                    as.character(formula(model)), perl = TRUE))
      if (grepl("log(", offterm, fixed = TRUE)) {

        d[,offname] <- exp(d[,offname])

      }

    }

    # Exponentiate offset if it was logged
    if ("log" %in% as.character(getCall(model)$offset)) {
      d[,offname] <- exp(d[,offname])
    }

  } else {

      off <- FALSE
      offname <- NULL

  }

  # Pulling the name of the response variable for labeling
  formula <- formula(model)
  formula <- paste(formula[2],formula[1],formula[3])

  resp <- sub("(.*)(?=~).*", x = formula, perl = T, replacement = "\\1")
  resp <- trimws(resp)

  # Saving key arguments as attributes of return object
  ss <- structure(ss, resp = resp, modx = modx, mod2 = mod2, pred = pred,
                  cond.int = cond.int)

### Centering #################################################################

  # Update facvars by pulling out all non-focals
  facvars <-
    facvars[!(facvars %in% c(pred, resp, modx, mod2, wname, offname))]

  # Use utility function shared by all interaction functions
  c_out <- center_vals(d = d, weights = wts, facvars = facvars,
              fvars = fvars, pred = pred,
              resp = resp, modx = modx, survey = survey,
              design = design, mod2 = mod2, wname = wname,
              offname = offname, centered = centered,
              scale = scale, n.sd = n.sd)

  design <- c_out$design
  d <- c_out$d
  fvars <- c_out$fvars
  facvars <- c_out$facvars

### Getting moderator values ##################################################

  modxvals2 <- mod_vals(d, modx, modxvals, survey, wts, design,
                        modx.labels = NULL, any.mod2 = !is.null(mod2),
                        sims = TRUE)

  # Dealing with two-level factors
  if (is.factor(d[,modx]) && length(levels(d[,modx])) != 2) {

    stop("Factor moderators can only have 2 levels.")

  } else if (is.factor(d[,modx]) & length(unique(d[,modx])) == 2) {

    d[,modx] <- as.numeric(d[,modx]) - 1

  }

  # Now specify def or not
  if (is.character(modxvals) | is.null(modxvals)) {

    ss <- structure(ss, def = TRUE)

  } else {

    ss <- structure(ss, def = FALSE)

  }

  if (!is.null(mod2)) {

    mod2vals2 <- mod_vals(d, mod2, mod2vals, survey, wts, design,
                          modx.labels = NULL, any.mod2 = !is.null(mod2),
                          is.mod2 = TRUE, sims = TRUE)

    # Dealing with two-level factors
    if (is.factor(d[,mod2]) && length(levels(d[,mod2])) != 2) {

      stop("Factor moderators can only have 2 levels.")

    } else if (is.factor(d[,mod2]) & length(unique(d[,mod2])) == 2) {

      d[,mod2] <- as.numeric(d[,mod2]) - 1

    }

    # Now specify def or not
    if (is.character(mod2vals) | is.null(mod2vals)) {

      ss <- structure(ss, def2 = TRUE)

    } else {

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

  mod2val_len <- length(mod2vals2)
  if (mod2val_len == 0) {mod2val_len <- 1}
  modxval_len <- length(modxvals2)

  # Make empty list to put actual models into
  mods <- rep(list(NA), times = mod2val_len)

  # Make empty list to hold above list if 2nd mod used
  if (!is.null(mod2)) {

    # Make empty list to put each matrix into
    mats <- rep(list(NA), times = mod2val_len)
    imats <- rep(list(NA), times = mod2val_len)

  }

  # Looping through (perhaps non-existent)
  for (j in seq_len(mod2val_len)) {

    # We don't want to do the J-N interval with the 1st moderator adjusted,
    # so we do it here. Requires an extra model fit.

    if (survey == FALSE) {

      # Creating extra "copy" of model frame to change for model update
      dt <- d

      if (!is.null(mod2)) { # We *do* need to adjust the 2nd moderator for J-N

        # The moderator value-adjusted variable
        dt[,mod2] <- dt[,mod2] - mod2vals2[j]

      }

      # Creating the model
      newmod <- update(model, data = dt)

      # Getting SEs, robust or otherwise
      if (robust == TRUE) {

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
                                  alpha = jnalpha, digits = digits, ...),
                   error = function(e) {return("No values")},
                   warning = function(w) {return("No values")})

    if (j != 0) {
        jns[[j]] <- jn
    }

  # Looping so any amount of moderator values can be used
  for (i in seq_len(length(modxvals2))) {

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
      newmod <- update(model, data = dt)

      # Getting SEs, robust or otherwise
      if (robust == TRUE) {

        # Use j_summ to get the coefficients
        sum <- jtools::j_summ(newmod, robust = T, robust.type = robust.type,
                              model.fit = F)
        summat <- sum$coeftable

        slopep <- summat[pred,c("Est.","S.E.","p")]
        intp <- summat["(Intercept)",c("Est.","S.E.","p")]

        retmat[i,1] <- modxvals2[i]
        retmat[i,2:4] <- slopep[]

        retmati[i,1] <- modxvals2[i]
        retmati[i,2:4] <- intp[]

      } else {

        sum <- jtools::j_summ(newmod, model.fit = F)
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
      sum <- j_summ(newmod, model.fit = F)
      summat <- sum$coeftable

      slopep <- summat[pred,c("Est.","S.E.","p")]
      intp <- summat["(Intercept)",c("Est.","S.E.","p")]

      retmat[i,1] <- modxvals2[i]
      retmat[i,2:4] <- slopep[]

      retmati[i,1] <- modxvals2[i]
      retmati[i,2:4] <- intp[]

    }

    mods[[i + (j - 1)*modxval_len]] <- newmod

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

#### build jnplot for 3-way interactions ######################################

  # If 3-way interaction and the user has `cowplot`, here's where we make the
  # final output
  if (!is.null(mod2) & johnson_neyman == TRUE & jnplot == TRUE) {

    # plots <- as.list(rep(NA, length(mod2vals) + 2))
    plots <- as.list(rep(NA, length(mod2vals)))
    the_legend <- NULL

    for (j in 1:length(jns)) {

    # Tell user we can't plot if they don't have cowplot installed
    if (jnplot == TRUE && !is.null(mod2) &&
      !requireNamespace("cowplot", quietly = TRUE)) {

      msg <- "To plot Johnson-Neyman plots for 3-way interactions,
      you need the cowplot package."
      warning(msg)
      jnplot <- FALSE # No more attempts at plotting

    } else if (jnplot == TRUE && !is.null(mod2)) {

      if (is.null(the_legend)) {
        # We save the legend the first time around to use w/ cowplot
        the_legend <-
          cowplot::get_legend(jns[[j]]$plot +
             theme_apa(legend.font.size = 8) +
               ggplot2::theme(legend.position = "bottom"))

        # Now we get rid of it for the actual plotting of the first plot
        jns[[j]]$plot <- jns[[j]]$plot +
          ggplot2::theme(legend.position = "none")

      } else {
        # For each subsequent plot, we don't need to save the legend,
        # just need to get rid of it
        jns[[j]]$plot <- jns[[j]]$plot +
          ggplot2::theme(legend.position = "none")
      }

      # Add a label for cowplot
      mod2lab <- names(mod2vals2)[j]
      jns[[j]]$plot <-
        jns[[j]]$plot + ggplot2::ggtitle(paste(mod2, "=", mod2lab)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 11))

      # Add the plot to the plot list at whatever the current end is
      index <- j
      plots[[index]] <- jns[[j]]$plot

    }

  }

  if (length(plots) %% 2 == 1) {

    plots[[length(plots) + 1]] <- the_legend
    just_plots <- cowplot::plot_grid(plotlist = plots, align = "auto",
                                         ncol = 2, vjust = 0, scale = 1)
    with_legend <- just_plots

  } else {

    just_plots <- cowplot::plot_grid(plotlist = plots, ncol = 2)
    with_legend <- cowplot::plot_grid(just_plots, the_legend,
                                      rel_heights = c(1,.1), nrow = 2)

  }

  # Now we put it all together--vjust is at a non-default level
  ss$jnplot <- with_legend

  } else if (johnson_neyman == TRUE) {

    ss$jnplot <- jns[[1]]$plot

  }

  if (jnplot == FALSE | johnson_neyman == FALSE) {

    ss$jnplot <- NULL

  }

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

    # If we're using second moderator, need to make things make sense
    # to inner loop
    if (!is.null(x$mod2)) {

      m <- NULL
      m$slopes <- ss$slopes[[j]]
      m$ints <- ss$ints[[j]]

      x$mod2vals <- round(x$mod2vals, x$digits)

      # Printing output to make it clear where each batch of second moderator
      # slopes begins
      if (x$def2 == FALSE) {
        cat("############################################################\n")
        cat("While", x$mod2, "(2nd moderator)", "=", x$mod2vals[j], "\n")
        cat("############################################################\n\n")
      } else {
        # If the user went with default +/- SD or used a factor variable,
        # we use the labels
        cat("############################################################\n")
        cat("While ", x$mod2, " (2nd moderator)", " = ", x$mod2vals[j], " (",
            names(x$mod2vals)[j], ")", "\n", sep = "")
        cat("############################################################\n\n")
      }


      if (x$johnson_neyman == TRUE) {

        # For 3-way interactions, we don't want each plot being printed
        attributes(x$jns[[j]])$plot <- FALSE
        print(x$jns[[j]])

      }

    } else {
      m <- ss

      if (x$johnson_neyman == TRUE) {
        print(x$jns[[j]])
      }

    }

    # Clearly label simple slopes
    cat("SIMPLE SLOPES ANALYSIS\n\n")

    for (i in seq_len(length(x$modxvals))) {

      # Use the labels for the automatic +/- 1 SD, factors
      if (x$def == TRUE) {

        cat("Slope of ", x$pred, " when ", x$modx, " = ",
            round(x$modxvals[i],x$digits), " (", names(x$modxvals)[i], ")",
            ": \n", sep = "")
        print(round(m$slopes[i,2:4], x$digits))

        # Print conditional intercept
        if (x$cond.int == TRUE) {
          cat("Conditional intercept"," when ", x$modx, " = ",
              round(x$modxvals[i],x$digits), " (", names(x$modxvals)[i], ")",
              ": \n", sep = "")
          print(round(m$ints[i,2:4], x$digits))
          cat("\n")
        } else {cat("\n")}

      } else { # otherwise don't use labels

        cat("Slope of ", x$pred, " when ", x$modx, " = ",
            round(x$modxvals[i],x$digits),
            ": \n", sep = "")
        print(round(m$slopes[i,2:4], x$digits))

        # Print conditional intercept
        if (x$cond.int == TRUE) {
          cat("Conditional intercept", " when ", x$modx, " = ",
              round(x$modxvals[i],x$digits), ": \n", sep = "")
          print(round(m$ints[i,2:4], x$digits))
          cat("\n")
        } else {cat("\n")}

      }
    }
  } # end mod2 loop

  if (!is.null(x$mod2) && x$jnplot == TRUE) {
    print(ss$jnplot)
  }

}
