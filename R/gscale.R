#' Scale and/or center regression inputs, including from survey designs, by dividing by 2 SD
#'
#' \code{gscale()} standardizes variables by dividing them by 2 standard deviations
#' and mean-centering them by default. It contains options for handling binary
#' variables separately. \code{gscale()} is a fork of \code{\link[arm]{rescale}}
#' from the \pkg{arm} package---the key feature difference is that
#' \code{gscale()} will perform the same functions for variables in
#' \code{\link[survey]{svydesign}} objects. \code{gscale()} is also more
#' user-friendly in that it is more flexible in how it accepts input.
#'
#' @param x A vector to be rescaled, or a vector of variable names. If none provided, but data
#'   frame or \code{svydesign} object is, all columns will be processed and returned.
#'
#' @param binary.inputs Options for binary variables. Default is \code{center};
#'   \code{0/1} keeps original scale; \code{-0.5/0.5} rescales 0 as -0.5 and 1 as 0.5;
#'   \code{center} subtracts the mean; and \code{full} subtracts the mean and
#'   divides by 2 sd.
#'
#' @param data A data frame or survey design. Only needed if you would like to
#'   rescale multiple variables at once. If \code{x = NULL}, all columns will
#'   be rescaled. Otherwise, \code{x} should be a vector of variable names. If
#'   \code{x} is a numeric vector, this argument is ignored.
#'
#' @param n.sd By how many standard deviations should the variables be divided by?
#'   Default is 2, as in \pkg{arm}'s \code{\link[arm]{rescale}}. Choosing 1 would
#'   make for a more typical standardization scheme.
#'
#' @param center.only A logical value indicating whether you would like to mean-center
#'   the values, but not scale them.
#'
#' @param scale.only A logical value indicating whether you would like to scale
#'   the values, but not mean-center them.
#'
#' @param weights A vector of weights equal in length to \code{x}. If iterating
#'   over a data frame, the weights will need to be equal in length to all the
#'   columns to avoid errors. You may need to remove missing values before using
#'   the weights.
#'
#' @details
#'
#' This function is adapted from the \code{\link[arm]{rescale}} function of
#' the \pkg{arm} package. It is named \code{gscale()} after the
#' popularizer of this scaling method, Andrew \strong{G}elman. By default, it
#' works just like \code{rescale}. But it contains many additional options and
#' can also accept multiple types of input without breaking a sweat.
#'
#' Only numeric variables are altered when in a data.frame or survey design.
#' Character variables, factors, etc. are skipped.
#'
#' For those dealing with survey data, if you provide a \code{survey.design} object
#' you can rest assured that the mean-centering and scaling is performed
#' with help from the \code{\link[survey]{svymean}} and
#' \code{\link[survey]{svyvar}} functions, respectively. It was among the primary
#' motivations for creating this function. \code{gscale()} will not center or
#' scale the weights variables defined in the survey design unless the user
#' specifically requests them in the \code{x =} argument.
#'
#' @family standardization, scaling, and centering tools
#'
#' @seealso
#'
#' \code{\link{j_summ}} is a replacement for the \code{summary} function for
#' regression models. On request, it will center and/or standardize variables
#' before printing its output.
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @references
#'
#' Gelman, A. (2008). Scaling regression inputs by dividing by two standard
#' deviations. \emph{Statistics in Medicine}, \emph{27}, 2865–2873.
#' \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#'
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
#' gscale(c("hp", "wt", "vs"), data = mtcars, binary.inputs = "center")
#' 
#' # Weighted inputs
#' 
#' wts <- runif(10, 0, 1)
#' gscale(x, weights = wts)
#' # If using a weights column of data frame, give its name
#' mtcars$weights <- runif(32, 0, 1)
#' gscale(data = mtcars, weights = weights) # will skip over mtcars$weights
#' # If using a weights column of data frame, can still select variables
#' gscale(x = c("hp", "wt", "vs"), data = mtcars, weights = weights)
#'
#' # Survey designs
#' library(survey)
#' data(api)
#' ## Create survey design object
#' dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
#'                      data = apistrat, fpc=~fpc)
#' # Creating test binary variable
#' dstrat$variables$binary <- rbinom(200, 1, 0.5) 
#'
#' gscale(data = dstrat, binary.inputs = "-0.5/0.5")
#' gscale(c("api00","meals","binary"), data = dstrat,
#'        binary.inputs = "-0.5/0.5")
#'
#'
#'
#'
#' @export


gscale <- function(x = NULL, binary.inputs = "center", data = NULL, n.sd = 2,
                   center.only = FALSE, scale.only = FALSE, weights = NULL) {

if (!(binary.inputs %in% c("center","full","0/1","-0.5/0.5"))) {
  stop("binary.inputs must be one of \"center\", \"full\", \"0/1\", or \"-0.5/0.5\"")
}

# If it's a vector, just do the thing
if (is.numeric(x)) {

  # Filter out missing observations
  x.obs <- x[!is.na(x)]

  # for binary cases
  if (length(unique(x.obs))==2) {

    if (binary.inputs == "0/1") {
      # Scale to 0/1
      x <- (x-min(x.obs))/(max(x.obs)-min(x.obs))
      return(x)

    } else if (binary.inputs == "-0.5/0.5") {
      # Keep scale, but center at 0 (not mean center!)
      return(x-0.5)

    } else if (binary.inputs == "center") {
      # Actually mean center
      if (is.null(weights)) {
        return(x - mean(x.obs, na.rm = TRUE))
      } else {
        return(x - weighted.mean(x.obs, weights, na.rm = TRUE))
      }

    } else if (binary.inputs == "full") {
      # Treat it just like continuous vars

      # support for weights
      if (is.null(weights)) {

        if (center.only == FALSE && scale.only == FALSE) {

          return((x - mean(x.obs, na.rm = TRUE)) / (n.sd*sd(x.obs, na.rm = TRUE)))

        } else if (center.only == TRUE && scale.only == FALSE) {

          return(x - mean(x.obs, na.rm = TRUE))

        } else if (center.only == FALSE && scale.only == TRUE) {

          return(x / n.sd*sd(x.obs, na.rm = TRUE))

        } else if (center.only == TRUE && scale.only == TRUE) {

          stop("You cannot set both center.only and scale.only to TRUE.")

        }
      } else {

        if (center.only == FALSE && scale.only == FALSE) {

          return((x - weighted.mean(x.obs, weights, na.rm = TRUE)) /
                   (n.sd*wtd.sd(x.obs, weights)))

        } else if (center.only == TRUE && scale.only == FALSE) {

          return(x - weighted.mean(x.obs, weights, na.rm = TRUE))

        } else if (center.only == FALSE && scale.only == TRUE) {

          return(x / n.sd*wtd.sd(x.obs, weights))

        } else if (center.only == TRUE && scale.only == TRUE) {

          stop("You cannot set both center.only and scale.only to TRUE.")

        }
      }

    }
  } else {

    # support for weights
    if (is.null(weights)) {

      if (center.only == FALSE && scale.only == FALSE) {

        return((x - mean(x.obs, na.rm = TRUE)) / (n.sd*sd(x.obs, na.rm = TRUE)))

      } else if (center.only == TRUE && scale.only == FALSE) {

        return(x - mean(x.obs, na.rm = TRUE))

      } else if (center.only == FALSE && scale.only == TRUE) {

        return(x / n.sd*sd(x.obs, na.rm = TRUE))

      } else if (center.only == TRUE && scale.only == TRUE) {

        stop("You cannot set both center.only and scale.only to TRUE.")

      }
    } else {

      if (center.only == FALSE && scale.only == FALSE) {

        return((x - weighted.mean(x.obs, weights, na.rm = TRUE)) /
                 (n.sd*wtd.sd(x.obs, weights)))

      } else if (center.only == TRUE && scale.only == FALSE) {

        return(x - weighted.mean(x.obs, weights, na.rm = TRUE))

      } else if (center.only == FALSE && scale.only == TRUE) {

        return(x / n.sd*wtd.sd(x.obs, weights))

      } else if (center.only == TRUE && scale.only == TRUE) {

        stop("You cannot set both center.only and scale.only to TRUE.")

      }
    }

  }

} else { # Now if x isn't a numeric vector, let's see if there's data

  # If data frame provided, check whether it is survey
  if (!is.null(data)) {

    if (is.data.frame(data)) {
      # Store temporary copy
      d <- data
      survey <- FALSE

    } else if (class(data)[1] == "survey.design2" || class(data)[1] == "svyrep.design") {
      # Store design and data frame
      design <- data
      d <- design$variables
      survey <- TRUE

    }
  } else { # Stop the presses
    stop("You must provide either a numeric vector or a data frame/survey design.")
  }

  # Okay, if not a survey and user provided variable names, do this
  if (survey == FALSE && !is.null(x)) {

    if (!is.null(weights)) {
      # If it's the weight column, skip
      wname <- as.character(substitute(weights))
      wname2 <- weights

      suppressWarnings({if (wname %in% names(d)) {

        weights <- d[[wname]]

      } else if (wname2 %in% names(d)) {

        weights <- d[[wname2]]

      }
      })

    }

    # Loop through the variable names
    for (i in x) {

      # Dealing with two-level factors
      if (length(unique(d[[i]])) == 2 && is.factor(d[[i]])) {

        d[[i]] <- as.numeric(d[[i]]) - 1

      } # Now it will follow the logic of any numeric binary var

      # Now just calling the rescale function, basically
      # for binary cases
      if (length(unique(d[[i]])) == 2 && is.numeric(d[[i]])) {

        if (binary.inputs=="0/1") {

          d[[i]] <- (d[[i]] - min(d[[i]])) / (max(d[[i]]) - min(d[[i]]))

        } else if (binary.inputs=="-0.5/0.5") {

          d[[i]] <- (d[[i]] - 0.5)

        } else if (binary.inputs=="center") {

          if (is.null(weights)) {

            d[[i]] <- (d[[i]] - mean(d[[i]], na.rm = TRUE))

          } else {

            d[[i]] <- (d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE))

          }

        } else if (binary.inputs=="full") {

          # support for weights, although clunky
          if (is.null(weights)) {
            if (center.only == FALSE && scale.only == FALSE) {

              d[[i]] <- ((d[[i]] - mean(d[[i]], na.rm = TRUE)) /
                           (n.sd*sd(d[[i]], na.rm = TRUE)))

            } else if (center.only == TRUE && scale.only == FALSE) {

              d[[i]] <- d[[i]] - mean(d[[i]], na.rm = TRUE)

            } else if (center.only == FALSE && scale.only == TRUE) {

              d[[i]] <- d[[i]] / (n.sd*sd(d[[i]], na.rm = TRUE))

            } else if (center.only == TRUE && scale.only == TRUE) {

              stop("You cannot set both center.only and scale.only to TRUE.")

            }
          } else {

            if (center.only == FALSE && scale.only == FALSE) {

              d[[i]] <-
                ((d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)) /
                           (n.sd*wtd.sd(d[[i]], weights)))

            } else if (center.only == TRUE && scale.only == FALSE) {

              d[[i]] <- d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)

            } else if (center.only == FALSE && scale.only == TRUE) {

              d[[i]] <- d[[i]] / (n.sd*wtd.sd(d[[i]], weights))

            } else if (center.only == TRUE && scale.only == TRUE) {

              stop("You cannot set both center.only and scale.only to TRUE.")

            }
          }

        }
      } else if (is.numeric(d[[i]])) {

        # support for weights, though fairly clunky
        if (is.null(weights)) {
          if (center.only == FALSE && scale.only == FALSE) {

            d[[i]] <- ((d[[i]] - mean(d[[i]], na.rm = TRUE)) /
                 (n.sd*sd(d[[i]], na.rm = TRUE)))

          } else if (center.only == TRUE && scale.only == FALSE) {

            d[[i]] <- d[[i]] - mean(d[[i]], na.rm = TRUE)

          } else if (center.only == FALSE && scale.only == TRUE) {

            d[[i]] <- d[[i]] / (n.sd*sd(d[[i]], na.rm = TRUE))

          } else if (center.only == TRUE && scale.only == TRUE) {

            stop("You cannot set both center.only and scale.only to TRUE.")

          }
        } else {

          if (center.only == FALSE && scale.only == FALSE) {

            d[[i]] <- ((d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)) /
                         (n.sd*wtd.sd(d[[i]], weights)))

          } else if (center.only == TRUE && scale.only == FALSE) {

            d[[i]] <- d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)

          } else if (center.only == FALSE && scale.only == TRUE) {

            d[[i]] <- d[[i]] / (n.sd*wtd.sd(d[[i]], weights))

          } else if (center.only == TRUE && scale.only == TRUE) {

            stop("You cannot set both center.only and scale.only to TRUE.")

          }
        }

      }
    }

    return(d) # Return the revised data frame

  } else if (survey == FALSE && is.null(x)) {
    # If no varnames given, do 'em all

    # See if weights variable is a column
    if (!is.null(weights)) {
      # If it's the weight column, skip
      wname <- as.character(substitute(weights))
      wname2 <- weights

      suppressWarnings({if (wname %in% names(d)) {

        weights <- d[[wname]]

      } else if (wname2 %in% names(d)) {

        weights <- d[[wname2]]

      }
      })
    }

    # Looping through every column with rescale
    for (i in seq_len(ncol(d))) {

      skip <- FALSE # For weights handling
      if (!is.null(weights)) {
        # If it's the weight column, skip
        if (names(d)[i] == wname) {
          skip <- TRUE
        }

      }

      if ((!is.numeric(d[[i]]) & length(unique(d[[i]])) != 2) || all(is.na(d[[i]])) || skip == TRUE) {
        # just skip over non-numeric variables except binary factors
        # columns that are all NA will still show up as numeric
        if (all(is.na(d[[i]]))) {

          message <- paste("All values of", names(d)[i], "were NA. Skipping...\n")
          warning(message)

        }

      } else {

      # Dealing with two-level factors
      if (length(unique(d[[i]])) == 2 && is.factor(d[[i]])) {
        d[[i]] <- as.numeric(d[[i]]) - 1
      } # Now it will follow the logic of any numeric binary var

      # for binary cases
      if (length(unique(d[[i]])) == 2) {

        if (binary.inputs=="0/1") {

          d[[i]] <- (d[[i]] - min(d[[i]])) / (max(d[[i]]) - min(d[[i]]))

        } else if (binary.inputs=="-0.5/0.5") {

          d[[i]] <- d[[i]] - 0.5

        } else if (binary.inputs=="center") {

          if (is.null(weights)) {
            d[[i]] <- d[[i]] - mean(d[[i]], na.rm = TRUE)
          } else {
            d[[i]] <- d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)
          }

        } else if (binary.inputs=="full") {

          # support for weights, although clunky
          if (is.null(weights)) {

            if (center.only == FALSE && scale.only == FALSE) {

              d[[i]] <- ((d[[i]] - mean(d[[i]], na.rm = TRUE)) /
                           (n.sd*sd(d[[i]], na.rm = TRUE)))

            } else if (center.only == TRUE && scale.only == FALSE) {

              d[[i]] <- d[[i]] - mean(d[[i]], na.rm = TRUE)

            } else if (center.only == FALSE && scale.only == TRUE) {

              d[[i]] <- d[[i]] / (n.sd*sd(d[[i]], na.rm = TRUE))

            } else if (center.only == TRUE && scale.only == TRUE) {

              stop("You cannot set both center.only and scale.only to TRUE.")

            }
          } else {

            if (center.only == FALSE && scale.only == FALSE) {

              d[[i]] <- ((d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)) /
                           (n.sd*wtd.sd(d[[i]], weights)))

            } else if (center.only == TRUE && scale.only == FALSE) {

              d[[i]] <- d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)

            } else if (center.only == FALSE && scale.only == TRUE) {

              d[[i]] <- d[[i]] / (n.sd*wtd.sd(d[[i]], weights))

            } else if (center.only == TRUE && scale.only == TRUE) {

              stop("You cannot set both center.only and scale.only to TRUE.")

            }
          }

        }

      } else {

        # support for weights, though fairly clunky
        if (is.null(weights)) {
          if (center.only == FALSE && scale.only == FALSE) {

            d[[i]] <- ((d[[i]] - mean(d[[i]], na.rm = TRUE)) /
                         (n.sd*sd(d[[i]], na.rm = TRUE)))

          } else if (center.only == TRUE && scale.only == FALSE) {

            d[[i]] <- d[[i]] - mean(d[[i]], na.rm = TRUE)

          } else if (center.only == FALSE && scale.only == TRUE) {

            d[[i]] <- d[[i]] / (n.sd*sd(d[[i]], na.rm = TRUE))

          } else if (center.only == TRUE && scale.only == TRUE) {

            stop("You cannot set both center.only and scale.only to TRUE.")

          }

        } else {

          if (center.only == FALSE && scale.only == FALSE) {

            d[[i]] <- ((d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)) /
                         (n.sd*wtd.sd(d[[i]], weights)))

          } else if (center.only == TRUE && scale.only == FALSE) {

            d[[i]] <- d[[i]] - weighted.mean(d[[i]], weights, na.rm = TRUE)

          } else if (center.only == FALSE && scale.only == TRUE) {

            d[[i]] <- d[[i]] / (n.sd*wtd.sd(d[[i]], weights))

          } else if (center.only == TRUE && scale.only == TRUE) {

            stop("You cannot set both center.only and scale.only to TRUE.")

          }
        }

      }
      }
    }

    return(d) # Return the revised data frame

  } else if (survey == TRUE && !is.null(x)) { # data is a survey and varnames given

    # Just calling rescale
    for (i in x) {

      if ((!is.numeric(d[[i]]) & length(unique(d[[i]])) != 2) || all(is.na(d[[i]]))) {
        # just skip over non-numeric variables
        # columns that are all NA will still show up as numeric
        if (all(is.na(d[[i]]))) {
          message <- paste("All values of", i, "were NA. Skipping...\n")
          warning(message)
        }
      } else {

      # Dealing with two-level factors
      if (length(unique(d[[i]])) == 2 && is.factor(d[[i]])) {
        d[[i]] <- as.numeric(d[[i]]) - 1
      } # Now it will follow the logic of any numeric binary var

      # for binary cases
      if (length(unique(d[[i]])) == 2) {

        if (binary.inputs == "0/1") {

          d[[i]] <- (d[[i]] - min(d[[i]])) / (max(d[[i]]) - min(d[[i]]))

        } else if (binary.inputs == "-0.5/0.5") {

          d[[i]] <- (d[[i]] - min(d[[i]])) / (max(d[[i]]) - min(d[[i]]))
          d[[i]] <- d[[i]] - 0.5

        } else if (binary.inputs == "center") {

          d[[i]] <- d[[i]] - survey::svymean(as.formula(paste("~", i, sep = "")),
                                            design = design, na.rm = TRUE)[1]

        } else if (binary.inputs == "full") {

          numerator <- d[[i]] - survey::svymean(as.formula(paste("~", i, sep = "")),
                                               design = design, na.rm = TRUE)[1]

          denominator <- n.sd * svysd(as.formula(paste("~",  i, sep = "")),
                                                    design = design, na.rm = TRUE)[1]

          if (center.only == FALSE && scale.only == FALSE) {

            d[[i]] <- numerator/denominator

          } else if (center.only == TRUE && scale.only == FALSE) {

            d[[i]] <- numerator

          } else if (center.only == FALSE && scale.only == TRUE) {

            d[[i]] <- d[[i]]/denominator

          } else if (center.only == TRUE && scale.only == TRUE) {

            stop("You cannot set both center.only and scale.only to TRUE.")

          }

        }
      } else {

        numerator <- d[[i]] - survey::svymean(as.formula(paste("~", i, sep = "")),
                                             design = design, na.rm = TRUE)
        denominator <- n.sd * svysd(as.formula(paste("~",  i, sep = "")),
                                                  design = design, na.rm = TRUE)

        if (center.only == FALSE && scale.only == FALSE) {

          d[[i]] <-  numerator/denominator

        } else if (center.only == TRUE && scale.only == FALSE) {

          d[[i]] <- numerator

        } else if (center.only == FALSE && scale.only == TRUE) {

          d[[i]] <- d[[i]]/denominator

        } else if (center.only == TRUE && scale.only == TRUE) {

          stop("You cannot set both center.only and scale.only to TRUE.")

        }

      }
      }
    }

    # Replace design's data with revised
    design$variables <- d
    # Return the design
    return(design)

  } else if (survey == TRUE && is.null(x)) {

    # Need to avoid the weight variables
    weightvars <- c(design$call["id"], design$call["strata"], design$call["weights"],
      design$call["fpc"])

    for (w in weightvars) {

      if (!is.null(w) && !exists("ws")) {
        # Need to convert the call to character and remove the "~"
        w <- gsub("~", "", as.character(w))
        ws <- c(w)

      } else if (!is.null(w)) {
        # Need to convert the call to character and remove the "~"
        w <- gsub("~", "", as.character(w))
        ws <- c(ws, as.character(w))

      }

    }

    for (i in seq_len(ncol(d))) {

      if (!(names(d)[i] %in% ws)) { # stepping around weight variables

        # I need its actual name for the svymean() commands
        i <- names(d)[i]

        if ((!is.numeric(d[[i]]) & length(unique(d[[i]])) != 2) || all(is.na(d[[i]]))) {
          # just skip over non-numeric variables
          # columns that are all NA will still show up as numeric
          if (all(is.na(d[[i]]))) {

            message <- paste("All values of", i, "were NA. Skipping...\n")
            warning(message)

          }

        } else {

          # Dealing with two-level factors
          if (length(unique(d[[i]])) == 2 && is.factor(d[[i]])) {

            d[[i]] <- as.numeric(d[[i]]) - 1
            design$variables <- d

          } # Now it will follow the logic of any numeric binary var

          # for binary cases
          if (length(unique(d[[i]])) == 2) {

            if (binary.inputs=="0/1") {

              d[[i]] <- (d[[i]] - min(d[[i]])) / (max(d[[i]]) - min(d[[i]]))

            } else if (binary.inputs=="-0.5/0.5") {

              d[[i]] <- d[[i]] - 0.5

            } else if (binary.inputs=="center") {

              d[[i]] <- d[[i]] - survey::svymean(as.formula(paste("~", i, sep = "")),
                                                design = design, na.rm = TRUE)[1]

            } else if (binary.inputs=="full") {

              numerator <- d[[i]] - survey::svymean(as.formula(paste("~", i, sep = "")),
                                                   design = design, na.rm = TRUE)[1]
              denominator <- n.sd * svysd(as.formula(paste("~",  i, sep = "")),
                                                        design = design, na.rm = TRUE)[1]

              if (center.only == FALSE && scale.only == FALSE) {

                d[[i]] <-  numerator/denominator

              } else if (center.only == TRUE && scale.only == FALSE) {

                d[[i]] <- numerator

              } else if (center.only == FALSE && scale.only == TRUE) {

                d[[i]] <- d[[i]]/denominator

              } else if (center.only == TRUE && scale.only == TRUE) {

                stop("You cannot set both center.only and scale.only to TRUE.")

              }

            }
          } else {

            numerator <- d[[i]] - survey::svymean(as.formula(paste("~", i, sep = "")),
                                                           design = design, na.rm = TRUE)
            denominator <- n.sd * svysd(as.formula(paste("~",  i, sep = "")),
                                                      design = design, na.rm = TRUE)

            if (center.only == FALSE && scale.only == FALSE) {

              d[[i]] <-  numerator/denominator

            } else if (center.only == TRUE && scale.only == FALSE) {

              d[[i]] <- numerator

            } else if (center.only == FALSE && scale.only == TRUE) {

              d[[i]] <- d[[i]] / denominator

            } else if (center.only == TRUE && scale.only == TRUE) {

              stop("You cannot set both center.only and scale.only to TRUE.")

            }

          }
        }
      }
    }

    # Replace design's data with revised
    design$variables <- d
    # Return the design
    return(design)

  }

}

}



