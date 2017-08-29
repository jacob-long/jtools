#' Calculate standard deviations with complex survey data
#'
#' \code{svysd} extends the \code{survey} package by calculating standard
#' deviations with syntax similar to the original package, which provides
#' only a \code{\link[survey]{svyvar}} function.
#'
#' @param formula A formula (e.g., ~var1+var2) specifying the term(s) of interest.
#'
#' @param design The \code{survey.design} or \code{svyrep.design} object.
#'
#' @param na.rm Logical. Should cases with missing values be dropped?
#'
#' @param digits An integer specifying the number of digits past the decimal to
#'   report in the output. Default is 3. You can change the default number of
#'   digits for all jtools functions with
#'   \code{options("jtools-digits" = digits)} where digits is the desired number.
#'
#' @param ... Additional arguments passed to \code{\link[survey]{svyvar}}.
#'
#' @details
#'
#' An alternative is to simply do \code{sqrt(svyvar(~term, design = design))}.
#' However, if printing and sharing the output, this may be misleading since
#' the output will say "variance."
#'
#' @family \pkg{survey} package extensions
#' @family survey tools
#'
#' @seealso \code{\link[survey]{svyvar}}
#'
#' @note This function was designed independent of the \pkg{survey} package and
#'  is neither endorsed nor known to its authors.
#'
#' @examples
#'  library(survey)
#'  data(api)
#'  # Create survey design object
#'  dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat,
#'                      fpc=~fpc)
#'
#'  # Print the standard deviation of some variables
#'  svysd(~api00+ell+meals, design = dstrat)
#'
#' @importFrom stats cov2cor model.frame na.pass weights
#' @export
#'

svysd <- function(formula, design, na.rm = FALSE,
                  digits = getOption("jtools-digits", default = 3), ... ) {

  # Pass to svyvar
  v <- survey::svyvar(formula, design, na.rm, ...)

  # Get terms from formula
  terms <- attr(terms(formula), "term.labels")

  # Convert to matrix
  v <- as.matrix(v)

  sds <- c() # Stores the values extracted next
  # Extract variance for each term from matrix
  for (i in 1:length(terms)) {
    sds <- c(sds, v[i,i])
  }

  # Converting from variance to s.d.
  sds <- sqrt(sds)

  # Returning named vector
  names(sds) <- terms

  # Sending digits to print command
  sds <- structure(sds, digits = digits)

  # Change class
  class(sds) <- "svysd"

  return(sds)

}


#######################################################################
#  PRINT METHOD                                                       #
#######################################################################

#' @export

print.svysd <- function(x, ...) {

  m <- as.matrix(x, ncol = 1)
  rownames(m) <- names(x)
  colnames(m) <- "std. dev."

  m <- round(m, attributes(x)$digits)

  print(m)

}


