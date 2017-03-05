#' Test whether sampling weights are needed
#'
#' Use the DuMouchel-Duncan (1983) test to assess the need for sampling weights
#' in your linear regression analysis.
#'
#' @param model The unweighted linear model (must be \code{lm}) you want to check
#' @param weights The name of the weights column in \code{model}'s data frame
#'   or a vector of weights equal in length to the number of observations
#'   included in \code{model}.
#'
#' @details
#'
#' This is designed to be similar to the \code{wgttest} macro for Stata
#' (\url{http://fmwww.bc.edu/repec/bocode/w/wgttest.html}). This method,
#' advocated for by DuMouchel and Duncan (1983), is fairly straigthforward. To
#' decide whether weights are needed, the weights are added to the linear model
#' as a predictor and interaction with each other predictor. Then, an omnibus
#' test of significance is performed to compare the weights-added model to the
#' original; if insignificant, weights are not significantly related to the
#' result and you can use the more efficient estimation from unweighted OLS.
#'
#' @references
#'
#' DuMouchel, W. H. & Duncan, D.J. (1983). Using sample survey weights in
#'   multiple regression analyses of stratified samples. \emph{Journal of the
#'   American Statistical Association}, \emph{78}. 535-543.
#'
#' Winship, C. & Radbill, L. (1994). Sampling weights and regression
#'   analysis. \emph{Sociological Methods and Research}, \emph{23}, 230-257.
#'
#'
#' @examples
#' # First, let's create some fake sampling weights
#' wts <- runif(50, 0, 5)
#' # Create model
#' fit <- lm(Income ~ Frost + Illiteracy + Murder,
#'           data = as.data.frame(state.x77))
#' # See if the weights change the model
#' wgttest(fit, wts)
#'
#'
#' @importFrom stats anova reformulate
#' @export

wgttest <- function(model, weights) {

  # Need to parse the arguments
  wname <- as.character(substitute(weights))

  call <- getCall(model)
  if (!is.null(call$data)) {
    d <- eval(call$data)
    mframe <- FALSE # telling myself whether I use model.frame
  } else {
    d <- model.frame(model)
    mframe <- TRUE
  }

  # Save response variable
  resp <- as.character(formula(model)[2])

  terms <- attributes(terms(model))$term.labels
  terms <- unname(as.character(terms))
  nterms <- sapply(terms, c, paste(" *", wname), USE.NAMES = F, simplify = F)
  nterms <- sapply(nterms, paste, sep = "", collapse = "")

  newf <- reformulate(nterms, response = resp)

  newmod <- update(model, formula. = newf, data = d)

  print(j_summ(newmod, model.info = F, model.fit = F))
  anova(model, newmod)

}
