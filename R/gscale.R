#' Scale and/or center data, including survey designs
#'
#' \code{gscale} standardizes variables by dividing them by 2 standard
#' deviations and mean-centering them by default. It contains options for
#' handling binary variables separately. \code{gscale()} is a fork of
#' \code{rescale} from the \code{arm} package---the key feature
#' difference is that \code{gscale()} will perform the same functions for
#' variables in \code{\link[survey]{svydesign}} objects. \code{gscale()} is
#' also more user-friendly in that it is more flexible in how it accepts input.
#'
#'
#'
#' @param data A data frame or survey design. Only needed if you would like to
#'   rescale multiple variables at once. If \code{x = NULL}, all columns will
#'   be rescaled. Otherwise, \code{x} should be a vector of variable names. If
#'   \code{x} is a numeric vector, this argument is ignored.
#'
#' @param vars If `data` is a data.frame or similar, you can scale only
#'   select columns by providing a vector column names to this argument.
#'
#' @param binary.inputs Options for binary variables. Default is \code{center};
#'   \code{0/1} keeps original scale; \code{-0.5/0.5} rescales 0 as -0.5 and 1
#'   as 0.5; \code{center} subtracts the mean; and \code{full} subtracts the
#'   mean and divides by 2 sd.
#'
#' @param binary.factors Coerce two-level factors to numeric and apply scaling
#'   functions to them? Default is TRUE.
#'
#' @param n.sd By how many standard deviations should the variables be divided
#'   by? Default for `gscale` is 2, like \code{arm}'s \code{rescale}.
#'   1 is the more typical standardization scheme.
#'
#' @param center.only A logical value indicating whether you would like to mean
#'   -center the values, but not scale them.
#'
#' @param scale.only A logical value indicating whether you would like to scale
#'   the values, but not mean-center them.
#'
#' @param weights A vector of weights equal in length to \code{x}. If iterating
#'   over a data frame, the weights will need to be equal in length to all the
#'   columns to avoid errors. You may need to remove missing values before using
#'   the weights.
#'
#' @param apply.weighted.contrasts Factor variables cannot be scaled, but you
#'   can set the contrasts such that the intercept in a regression model will
#'   reflect the true mean (assuming all other variables are centered). If set
#'   to TRUE, the argument will apply weighted effects coding to all factors.
#'   This is similar to the R default effects coding, but weights according to
#'   how many observations are at each level. An adapted version of
#'   `contr.wec()` from the `wec` package is used to do this. See
#'   that package's documentation and/or Grotenhuis et al. (2016) for more
#'   info.
#'
#' @param messages Print messages when variables are not processed due to
#'   being non-numeric or all missing? Default is FALSE.
#'
#' @param x Deprecated. Pass numeric vectors to `data`. Pass vectors of column
#'   names to `vars`.
#'
#' @details
#'
#' This function is adapted from the `rescale` function of
#' the `arm` package. It is named \code{gscale()} after the
#' popularizer of this scaling method, Andrew \strong{G}elman. By default, it
#' works just like \code{rescale}. But it contains many additional options and
#' can also accept multiple types of input without breaking a sweat.
#'
#' Only numeric variables are altered when in a data.frame or survey design.
#' Character variables, factors, etc. are skipped.
#'
#' For those dealing with survey data, if you provide a \code{survey.design}
#' object you can rest assured that the mean-centering and scaling is performed
#' with help from the [`svymean()`][survey::svymean] and
#' [`svyvar()`][survey::svyvar] functions, respectively. It was among the
#' primary motivations for creating this function. \code{gscale()} will not
#' center or scale the weights variables defined in the survey design unless
#' the user specifically requests them in the \code{x =} argument.
#'
#' @family standardization
#'
#' @seealso
#'
#' \code{\link{j_summ}} is a replacement for the \code{summary} function for
#' regression models. On request, it will center and/or standardize variables
#' before printing its output.
#'
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @references
#'
#' Gelman, A. (2008). Scaling regression inputs by dividing by two standard
#' deviations. \emph{Statistics in Medicine}, \emph{27}, 2865–2873.
#' \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#'
#' Grotenhuis, M. te, Pelzer, B., Eisinga, R., Nieuwenhuis, R.,
#'  Schmidt-Catran, A., & Konig, R. (2017). When size matters: Advantages of
#'  weighted effect coding in observational studies. *International Journal of
#'  Public Health*, *62*, 163–167. https://doi.org/10.1007/s00038-016-0901-1 (
#'  open access)
#'
#' @examples
#'
#' x <- rnorm(10, 2, 1)
#' x2 <- rbinom(10, 1, .5)
#'
#' # Basic use
#' gscale(x)
#' # Normal standardization
#' gscale(x, n.sd = 1)
#' # Scale only
#' gscale(x, scale.only = TRUE)
#' # Center only
#' gscale(x, center.only = TRUE)
#' # Binary inputs
#' gscale(x2, binary.inputs = "0/1")
#' gscale(x2, binary.inputs = "full") # treats it like a continous var
#' gscale(x2, binary.inputs = "-0.5/0.5") # keep scale, center at zero
#' gscale(x2, binary.inputs = "center") # mean center it
#'
#' # Data frame as input
#' # loops through each numeric column
#' gscale(data = mtcars, binary.inputs = "-0.5/0.5")
#'
#' # Specified vars in data frame
#' gscale(mtcars, vars = c("hp", "wt", "vs"), binary.inputs = "center")
#'
#' # Weighted inputs
#'
#' wts <- runif(10, 0, 1)
#' gscale(x, weights = wts)
#' # If using a weights column of data frame, give its name
#' mtcars$weights <- runif(32, 0, 1)
#' gscale(mtcars, weights = weights) # will skip over mtcars$weights
#' # If using a weights column of data frame, can still select variables
#' gscale(mtcars, vars = c("hp", "wt", "vs"), weights = weights)
#'
#' # Survey designs
#' if (requireNamespace("survey")) {
#'   library(survey)
#'   data(api)
#'   ## Create survey design object
#'   dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                        data = apistrat, fpc=~fpc)
#'   # Creating test binary variable
#'   dstrat$variables$binary <- rbinom(200, 1, 0.5)
#'
#'   gscale(data = dstrat, binary.inputs = "-0.5/0.5")
#'   gscale(data = dstrat, vars = c("api00","meals","binary"),
#'          binary.inputs = "-0.5/0.5")
#' }
#'
#'
#'
#' @export


gscale <- function(data = NULL, vars = NULL, binary.inputs = "center",
                   binary.factors = TRUE, n.sd = 2,
                   center.only = FALSE, scale.only = FALSE, weights = NULL,
                   apply.weighted.contrasts =
                     getOption("jtools-weighted.contrasts", FALSE),
                   x = NULL, messages = FALSE) {

  if (binary.inputs %nin% c("center","full","0/1","-0.5/0.5")) {
    stop("binary.inputs must be one of \"center\", \"full\", \"0/1\",",
         "or \"-0.5/0.5\"")
  }

  if (!is.null(x)) {
    if (is.numeric(x)) {
      data <- x
      warning("The x argument is deprecated. Pass all data, even numeric ",
              "vectors, to the data argument. Converted x to data...")
    } else {
      vars <- x
      warning("The x argument is deprecated. Pass column names to the ",
              "vars argument.")
    }
  }

  if (is.data.frame(data)) {
    # Store temporary copy
    d <- data
    survey <- FALSE
    design <- NULL

  } else if (any(class(data) %in% c("survey.design2", "svyrep.design"))) {
    # Store design and data frame
    design <- data
    d <- design$variables
    survey <- TRUE

  } else if (!inherits(data, "data.frame") && !is.numeric(data)) {
    stop("data argument must be either a numeric vector or data frame ",
         "(or related class). If you think the data type provided should ",
         "be compatible, contact the developer of this package.")
  } else {
    survey <- FALSE
  }

  # Deal with weights
  if (!is.null(weights) & (is.data.frame(data) | survey == TRUE)) {
    # If it's the weight column, skip
    wname <- as.character(substitute(weights))
    wname2 <- weights

    if (length(wname) == 1 && wname %in% names(d)) {

      weights <- d[[wname]]

    } else if (length(wname2) == 1 && wname2 %in% names(d)) {

      weights <- d[[wname2]]

    }

  }


  # If it's a vector, just do the thing
  if (is.numeric(data)) {

    return(scaler(x = data, binary.inputs = binary.inputs,
                  n.sd = n.sd, center.only = center.only,
                  scale.only = scale.only, weights = weights,
                  binary.factors = binary.factors, messages = TRUE))

  } else {

    if (is.null(vars)) {
      vars <- names(d)
    } else if (any(vars %nin% names(d))) {
      stop(vars[which(vars %nin% names(d))[1]],
           " is not a column name in the supplied data.")
    }

    for (name in vars) {
      d[[name]] <- scaler(x = d[[name]], binary.inputs = binary.inputs,
                          n.sd = n.sd,
                          center.only = center.only, scale.only = scale.only,
                          weights = weights, binary.factors = binary.factors,
                          survey = survey, design = design, name = name,
                          messages = messages,
                          apply.weighted.contrasts = apply.weighted.contrasts)
    }

  }

  if (survey == FALSE) {

    return(d)

  } else {

    data$variables <- d
    return(data)

  }

}


#### Helper function #########################################################

#' @importFrom stats contrasts<-

scaler <- function(x, binary.inputs, n.sd = 2, center.only = FALSE,
                   scale.only = FALSE, weights = NULL, binary.factors = NULL,
                   survey = FALSE, design = NULL, name = NULL, messages,
                   apply.weighted.contrasts = FALSE) {

  # Filter out missing observations
  x.obs <- x[!is.na(x)]

  # Deal with all-missing data
  if (all(is.na(x))) {
    if (!is.null(name)) {
      if (messages) {message("All observations of ", name, " were missing.")}
      return(x)
    } else {
      if (messages) {message("All observations were missing.")}
      return(x)
    }
  }

  # Kick back non-numeric inputs
  if (!is.numeric(x) && !is.factor(x)) {
    if (!is.null(name)) {
      if (messages) {message(name, " is not numeric and was not scaled.")}
    } else {
      if (messages) {message("Non-numeric variable was not scaled.")}
    }
    return(x)
  } else if ("factor" %in% class(x.obs)) {
    # Dealing with two-level factors
    if (nlevels(x.obs) == 2 && binary.factors == TRUE) {

      x <- as.numeric(x) - 1
      x.obs <- as.numeric(x.obs) - 1

    } else {

      if (apply.weighted.contrasts == FALSE) {
        if (!is.null(name)) {
          if (messages) {message(name, " is not numeric and was not scaled.")}
        } else {
          if (messages) {message("Non-numeric variable was not scaled.")}
        }
      } else {
        contrasts(x) <- contr.weighted(x = x, weights = weights)
      }
      return(x)

    }
  }

  # If no weights, then make a vector of 1s
  if (is.null(weights)) {
    weights <- rep(1, times = length(x.obs))
  } else {
    weights <- weights[!is.na(x)] # dealing with missingness in x
  }

  # Create objects storing mean and sd
  if (survey == FALSE) {
    the_mean <- weighted.mean(x.obs, weights, na.rm = TRUE)
    the_sd <- n.sd * wtd.sd(x.obs, weights)
  } else {
    design$variables[[name]] <- x # Replaces 2-level factors with numeric dummy
    the_mean <- survey::svymean(as.formula(paste("~", name, sep = "")),
                                design = design, na.rm = TRUE)[1]
    the_sd <- n.sd * svysd(as.formula(paste("~",  name, sep = "")),
                           design = design, na.rm = TRUE)
  }

  # for binary cases
  if (length(unique(x.obs)) == 2) {

    if (binary.inputs == "0/1") {
      # Scale to 0/1
      x.obs <- (x.obs - min(x.obs)) / (max(x.obs) - min(x.obs))
      x[!is.na(x)] <- x.obs
      return(x)

    } else if (binary.inputs == "-0.5/0.5") {
      # Keep scale, but center at 0 (not mean center!)
      x[!is.na(x)] <- x.obs - 0.5
      return(x)

    } else if (binary.inputs == "center") {
      # Actually mean center
      x[!is.na(x)] <- x.obs - the_mean
      return(x)

    } else if (binary.inputs == "full") {
      # Do nothing, let function proceed to code for non-binary vectors
    }

  }

  if (center.only == FALSE && scale.only == FALSE) {

    x.obs <- (x.obs - the_mean) / the_sd

  } else if (center.only == TRUE && scale.only == FALSE) {

    x.obs <- x.obs - the_mean

  } else if (center.only == FALSE && scale.only == TRUE) {

    x.obs <- x.obs / the_sd

  } else if (center.only == TRUE && scale.only == TRUE) {

    stop("You cannot set both center.only and scale.only to TRUE.")

  }

  x[!is.na(x)] <- x.obs
  return(x)

}

#### standardize #############################################################

#' @title Standardize vectors, data frames, and survey designs
#' @description This function is a wrapper around [gscale()] that is configured
#'   to do a conventional standardization of continuous variables,
#'   mean-centering and dividing by one standard deviation.
#' @inheritParams gscale
#' @return A transformed version of the `data` argument.
#' @details Some more information can be found in the documentation for
#'   [gscale()]
#'
#' @family standardization
#' @examples
#'
#' # Standardize just the "qsec" variable in mtcars
#' standardize(mtcars, vars = "qsec")
#'
#' @rdname standardize
#' @export

standardize <- function(data = NULL, vars = NULL, binary.inputs = "center",
                        binary.factors = TRUE, weights = NULL) {

  gscale(data = data, vars = vars, binary.inputs = binary.inputs,
         binary.factors = binary.factors, n.sd = 1,
         center.only = FALSE, scale.only = FALSE, weights = weights,
         x = NULL)

}

#### center ##################################################################

#' @title Mean-center vectors, data frames, and survey designs
#' @description This function is a wrapper around [gscale()] that is configured
#'   to mean-center variables without affecting the scaling of those variables.
#' @inheritParams gscale
#' @return A transformed version of the `data` argument.
#' @details Some more information can be found in the documentation for
#'   [gscale()]
#' @family standardization
#' @examples
#'
#' # Standardize just the "qsec" variable in mtcars
#' standardize(mtcars, vars = "qsec")
#'
#' @rdname center
#' @export

center <- function(data = NULL, vars = NULL, binary.inputs = "center",
                        binary.factors = TRUE, weights = NULL) {

  gscale(data = data, vars = vars, binary.inputs = binary.inputs,
         binary.factors = binary.factors, n.sd = 1,
         center.only = TRUE, scale.only = FALSE, weights = weights,
         x = NULL)

}
