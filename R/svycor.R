#' Calculate Pearson correlations with complex survey data
#'
#' \code{svycor} extends the \code{survey} package by calculating correlations
#' with syntax similar to the original package, which for reasons unknown lacks
#' such a function.
#'
#' @param formula A formula (e.g., ~var1+var2) specifying the terms to correlate.
#'
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#'
#' @param na.rm Logical. Should cases with missing values be dropped?
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 2. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired number.
#'
#' @param sig.stats Logical. Perform non-parametric bootstrapping
#'   (using \code{\link[weights]{wtd.cor}}) to generate standard errors and
#'   associated t- and p-values. See details for some considerations when doing
#'   null hypothesis testing with complex survey correlations.
#'
#' @param bootn If \code{sig.stats} is TRUE, this defines the number of bootstraps
#'   to be run to generate the standard errors and p-values. For large values and
#'   large datasets, this can contribute considerably to processing time.
#'
#' @param mean1 If \code{sig.stats} is TRUE, it is important to know whether the
#'   sampling weights should have a mean of 1. That is, should the standard errors
#'   be calculated as if the number of rows in your dataset is the total number of
#'   observations (TRUE) or as if the sum of the weights in your dataset is the
#'   total number of observations (FALSE)?
#'
#' @param ... Additional arguments passed to \code{\link[survey]{svyvar}}.
#'
#' @details
#'   This function extends the \code{survey} package by calculating the correlations
#'   for user-specified variables in survey design and returning a correlation matrix.
#'
#'   Using the \code{\link[weights]{wtd.cor}} function, this function also returns
#'   standard errors and p-values for the correlation terms using a sample-weighted
#'   bootstrapping procedure. While correlations do not require distributional
#'   assumptions, hypothesis testing (i.e., \eqn{r > 0}) does. The appropriate way to
#'   calculate standard errors and use them to define a probability is not straightforward
#'   in this scenario since the weighting causes heteroskedasticity, thereby violating
#'   an assumption inherent in the commonly used methods for converting Pearson's
#'   correlations into t-values. The method provided here is defensible, but if
#'   reporting in scientific publications the method should be spelled out.
#'
#' @return
#'
#'  If significance tests are not requested, there is one returned value:
#'
#'  \item{cors}{The correlation matrix (without rounding)}
#'
#'  If significance tests are requested, the following are also returned:

#'  \item{p.values}{A matrix of p values}
#'  \item{t.values}{A matrix of t values}
#'  \item{std.err}{A matrix of standard errors}
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @family \pkg{survey} package extensions
#' @family survey tools
#'
#' @seealso \code{\link[weights]{wtd.cor}}, \code{\link[survey]{svyvar}}
#'
#' @note This function was designed in part on the procedure recommended by Thomas
#'   Lumley, the author of the survey package, on
#'   \href{http://stackoverflow.com/questions/34418822/pearson-correlation-coefficient-in-rs-survey-package#41031088}{Stack Overflow}. However, he has not reviewed or endorsed this implementation.
#'   All defects are attributed to the author.
#'
#' @examples
#'  library(survey)
#'  data(api)
#'  # Create survey design object
#'  dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat,
#'                      fpc=~fpc)
#'
#'  # Print correlation matrix
#'  svycor(~api00+api99+dnum, design = dstrat)
#'
#'  # Save the results, extract correlation matrix
#'  out <- svycor(~api00+api99+dnum, design = dstrat)
#'  out$cors
#'
#' @importFrom stats cov2cor model.frame na.pass weights
#' @export
#'




svycor <- function(formula, design, na.rm = FALSE,
                   digits = getOption("jtools-digits", default = 2),
                   sig.stats = FALSE,
                   bootn = 1000, mean1 = TRUE, ... ) {

  # If sig.stats == T, Need to get the data in a data.frame-esque format
  # to pass to wtd.cor
  if (inherits(formula,"formula") && sig.stats == TRUE) {

    # A data frame with the selected variables and their non-weighted values
    mf <- model.frame(formula, model.frame(design),na.action = na.pass)

    # Extract the weights for use with wtd.cor
    wts <- weights(design, "sampling")

  } else if (sig.stats == TRUE) {
    stop("To get significance tests, provide the argument as a formula (e.g.,
         ~var1 + var2)")
  }

  # Pass to svyvar
  v <- survey::svyvar(formula, design, na.rm, ...)

  # Convert to matrix
  v <- as.matrix(v)

  # Get correlation matrix (plus some)
  corv <- cov2cor(v)
  corv <- corv[seq_len(nrow(corv)), seq_len(nrow(corv))]

  # Creating return object
  c <- NULL
  # Corr. table goes in cors
  c$cors <- corv
  # Passing sig.stats to print function
  c$sig.stats <- sig.stats
  # Passing digits to print function
  c$digits <- digits

  class(c) <- c("svycor", "matrix")
  if (sig.stats == FALSE) {
    return(c)
  } else {
    # Use wtd.cor
    wcors <- weights::wtd.cor(mf, weight=wts, bootse=TRUE, mean1=mean1,
                              bootn=bootn, bootp=T)

    c$cors <- wcors$correlation
    c$p.values <- wcors$p.value
    c$t.values <- wcors$t.value
    c$std.err <- wcors$std.err

    return(c)

  }

}


#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.svycor <- function(x, ...) {

  if (x$sig.stats == FALSE) {

    # Print the table without so many digits
    print(as.table(round(x$cors, x$digits)))

  } else {

    # Save rounded table
    cm <- round(x$cors, x$digits)

    # Going to put significance stars (*) next to p < .05 coefs
    star <- function(x) {
      if (x < 0.05) {
        x <- "*"
      } else {
        x <- ""
      }
    }

    # Create a matrix of significance stars
    pm <- x$p.values
    for (i in seq_len(nrow(pm))) {
      for (j in seq_len(ncol(pm))) {
        pm[i,j] <- star(pm[i,j])
      }
    }
    # Taking asterisks out of self-correlations
    diag(pm)[] <- ""

    # Combine matrix of estimates and significance stars
    cm[] <- paste(cm[], pm[], sep="")

    print(as.table(cm))

  }

}


