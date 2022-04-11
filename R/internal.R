
#### Programming helpers #####################################################

## Quicker way to get last item of vector
last <- function(x) {return(x[length(x)])}
## Just so code reads more clearly when using last(x)
first <- function(x) {return(x[1])}

check_if_zero_base <- function(x) {
  # this is the default tolerance used in all.equal
  tolerance <- .Machine$double.eps^0.5
  # If the absolute deviation between the number and zero is less than
  # the tolerance of the floating point arithmetic, then return TRUE.
  # This means, to me, that I can treat the number as 0 rather than
  # -3.20469e-16 or some such.
  abs(x - 0) < tolerance
}

# This seems to give about a 80%-90% speed boost
check_if_zero <- Vectorize(check_if_zero_base)

# Try to anticipate which S3 will be called (sloop package should have
# something like this when it is released)
# Code adapted from G. Grothendieck's at Stack Overflow:
# https://stackoverflow.com/questions/42738851/r-how-to-find-what-s3-method-
# will-be-called-on-an-object
#' @importFrom utils .S3methods getS3method
find_S3_class <- function(generic, ..., package) {

  # not going to provide function, just function name as character
  # ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for (m in .S3methods(generic, envir = getNamespace(package))) {
    assign(sub(generic, "X", m, fixed = TRUE), "body<-"(f, value = m))
  }

  char_meth <- tryCatch(X(...), error = function(e) {return(NA)})
  if (is.na(char_meth)) {return(char_meth)}
  # Return the stub for dispatch to getS3method as class
  return(reg_match("(?<=\\.).*", char_meth, perl = TRUE))
}

# I'm sure stingr/stringi have this, but I don't want to import them
reg_match <- function(pattern, text, ignore.case = FALSE, perl = FALSE,
                      fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  matches <- gregexpr(pattern, text, ignore.case, perl, fixed, useBytes)
  # If only 1 match, return just the one match rather than a list
  if (length(matches) == 1) {matches <- matches[[1]]}
  regmatches(text, matches, invert)

}


### Handle NA better #########################################################
# Since I accept data from users and the global environment, there may be 
# missing cases including in the raw data that weren't used in the model fit.
# I can't use complete.cases since these data frames may include extra columns.
# Rather than guess at which variables were used to determine missingness
# (straightforward with lm, but could have hidden problems with glms/lmer/etc.)
# I use the model.frame features for doing so.
drop_missing <- function(model, data) {
  na_action <- attr(model.frame(model), "na.action")
  # If no "na.action" attribute, nothing to remove
  if (is.null(na_action)) {
    return(data)
  }
  to_remove <- names(na_action)
  data <- data[rownames(data) %not% to_remove, ]
  return(data)
}

### Formula helpers ##########################################################

any_transforms <- function(formula, rhs.only = TRUE) {
  if (rhs.only == TRUE) {
    any(all_vars(formula) %nin% rownames(attr(terms(formula), "factor")))
  } else {
    any(all.vars(formula) %nin% rownames(attr(terms(formula), "factor")))
  }
}

which_terms <- function(formula, var) {
  terms <- terms(formula)
  factors <- attr(terms, "factors")
  names(factors[var,] %not% 0)
}

original_terms <- function(formula) {
  o <- all.vars(formula)
  names(o) <- rownames(attr(terms(formula), "factors"))
  o
}

# get_response <- function(formula) {
#   original_terms(formula)[attr(terms(formula), "response")]
# }

# Adapted from formula.tools
two_sided <- function(x, ...) {
  # from operator.tools::operators()
  operators <- c("::", ":::", "@", "$", "[", "[[", ":", "+", "-", "*", "/", "^",
                 "%%", "%/%", "<", "<=", ">", ">=", "==", "!=", "%in%", "%!in%",
                 "!", "&", "&&", "|", "||", "~", "<-", "<<-", "=", "?", "%*%",
                 "%x%", "%o%", "%>%", "%<>%", "%T>%")
  is.name(x[[1]]) && deparse(x[[1]]) %in% operators && length(x) == 3
}

# Adapted from formula.tools
one_sided <- function(x, ...) {
  # from operator.tools::operators()
  operators <- c("::", ":::", "@", "$", "[", "[[", ":", "+", "-", "*", "/", "^",
                 "%%", "%/%", "<", "<=", ">", ">=", "==", "!=", "%in%", "%!in%",
                 "!", "&", "&&", "|", "||", "~", "<-", "<<-", "=", "?", "%*%",
                 "%x%", "%o%", "%>%", "%<>%", "%T>%")
  is.name(x[[1]]) && deparse(x[[1]]) %in% operators && length(x) == 2
}

# Adapted from formula.tools
get_lhs <- function(x) {
  if (two_sided(x) == TRUE) {
    x[[2]] 
  } else if(one_sided(x)) {
    NULL   
  } else {
    stop_wrap(x, "does not appear to be a one- or two-sided formula.")
  }
}

is_lhs_transformed <- function(x) {
  final <- as.character(deparse(get_lhs(x)))
  bare_vars <- all.vars(get_lhs(x))
  any(final != bare_vars)
}

# Adapted from formula.tools
get_rhs <- function(x) {
  # from operator.tools::operators()
  operators <- c("::", ":::", "@", "$", "[", "[[", ":", "+", "-", "*", "/", "^",
                 "%%", "%/%", "<", "<=", ">", ">=", "==", "!=", "%in%", "%!in%",
                 "!", "&", "&&", "|", "||", "~", "<-", "<<-", "=", "?", "%*%",
                 "%x%", "%o%", "%>%", "%<>%", "%T>%")
  
  if (as.character(x[[1]]) %nin% operators) {
    stop_wrap(x[[1]], "does not appear to be an operator.")
  }
  
  if (two_sided(x) == TRUE) {x[[3]]} else if (one_sided(x) == TRUE) {x[[2]]}
} 

all_vars <- function(formula) {
  if (two_sided(formula)) {
    c(as.character(deparse(get_lhs(formula))), all.vars(get_rhs(formula)))
  } else if (one_sided(formula)) {
    all.vars(get_rhs(formula))
  }
}

#### Weighted helpers ########################################################

#' @title Weighted standard deviation calculation
#' @description This function calculates standard deviations with weights and
#'  is a counterpart to the built-in `weighted.mean` function.
#' @param x A vector of values for which you want the standard deviation
#' @param weights A vector of weights equal in length to `x`
#' @rdname wtd.sd 
#' @export 

wtd.sd <- function(x, weights) {
  # Get the mean
  xm <- weighted.mean(x, weights, na.rm = TRUE)
  # Squaring the weighted deviations and dividing by weighted N - 1
  variance <- sum((weights * (x - xm)^2) / (sum(weights[!is.na(x)]) - 1), na.rm = TRUE)
  # Standard deviation is sqrt(variance)
  sd <- sqrt(variance)
  # Return the SD
  return(sd)
}

wtd.table <- function(x, weights = NULL, na.rm = TRUE) {

  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }

  if (length(x) != length(weights)) {
    stop("x and weights lengths must be the same")
  }

  if (na.rm) {
    s <- !is.na(x) & !is.na(weights)
    x <- x[s, drop = FALSE]
    weights <- weights[s]
  }

  result <- tapply(weights, x, sum, simplify = TRUE)

  result[is.na(result)] <- 0

  as.table(result)

}

### weighted effects coding ##################################################

#' @importFrom stats contr.treatment

contr.weighted <- function(x, base = 1, weights = NULL) {

  frequencies <- wtd.table(x, weights = weights)
  n.cat <- length(frequencies)

  # If base level is named, get the index
  if (is.character(base)) {
    base <- which(levels(x) == base)
  }

  new.contrasts <- contr.treatment(n.cat, base = base)
  new.contrasts[base, ] <- -1 * frequencies[-base]/frequencies[base]
  colnames(new.contrasts) <- names(frequencies[-base])

  return(new.contrasts)

}

#### Regex helper ############################################################

# Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}

### Hadley update #############################################################
# from https://stackoverflow.com/questions/13690184/update-inside-a-function-
# only-searches-the-global-environment
#' @importFrom stats update.formula

j_update <- function(mod, formula = NULL, data = NULL, offset = NULL,
                     weights = NULL, call.env = parent.frame(), ...) {
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }

  if (!is.null(data)) call$data <- data
  if (!is.null(formula)) call$formula <- update.formula(call$formula, formula)
  env <- attr(term, ".Environment")
  # Jacob add
  # if (!is.null(offset))
  call$offset <- offset
  # if (!is.null(weights))
  call$weights <- weights


  extras <- as.list(match.call())[-1]
  extras <- extras[which(names(extras) %nin% c("mod", "formula", "data",
                                               "offset", "weights",
                                               "call.env"))]
  for (i in seq_along(extras)) {
    if (is.name(extras[[i]])) {
      extras[[i]] <- eval(extras[[i]], envir = call.env)
    }
  }

  existing <- !is.na(match(names(extras), names(call)))
  for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
  if (any(!existing)) {
    call <- c(as.list(call), extras[!existing])
    call <- as.call(call)
  }

  if (is.null(call.env)) {call.env <- parent.frame()}

  eval(call, env, call.env)
}

### coeftest ##################################################################

## Taken from lmtest package with changes

coeftest <- function(x, vcov. = NULL, df = NULL, ...) {
  UseMethod("coeftest")
}

#' @importFrom stats pnorm

coeftest.default <- function(x, vcov. = NULL, df = NULL, ...) {
  ## extract coefficients and standard errors
  est <- coef(x)
  if (is.null(vcov.)) {
    se <- vcov(x)
  } else {
    if (is.function(vcov.)) {
      se <- vcov.(x, ...)
    } else {
      se <- vcov.
    }
  }
  se <- sqrt(diag(se))

  ## match using names and compute t/z statistics
  if (!is.null(names(est)) && !is.null(names(se))) {

    if (length(unique(names(est))) == length(names(est)) &&
       length(unique(names(se))) == length(names(se))) {

      anames <- names(est)[names(est) %in% names(se)]
      est <- est[anames]
      se <- se[anames]

    }
  }
  tval <- as.vector(est)/se

  ## apply central limit theorem
  if (is.null(df)) {

    df <- try(df.residual(x), silent = TRUE)
    if (inherits(df, "try-error")) df <- NULL

  }

  if (is.null(df)) df <- 0

  if (is.finite(df) && df > 0) {

    pval <- 2 * pt(abs(tval), df = df, lower.tail = FALSE)
    cnames <- c("Est.", "S.E.", "t value", "p")
    mthd <- "t"

  } else {

    pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
    cnames <- c("Est.", "S.E.", "z value", "p")
    mthd <- "z"

  }

  rval <- cbind(est, se, tval, pval)
  colnames(rval) <- cnames
  # class(rval) <- "coeftest"
  attr(rval, "method") <- paste(mthd, "test of coefficients")
  ##  dQuote(class(x)[1]), "object", sQuote(deparse(substitute(x))))
  return(rval)

}

coeftest.glm <- function(x, vcov. = NULL, df = Inf, ...) {
  coeftest.default(x, vcov. = vcov., df = df, ...)
}

# coeftest.mlm <- function(x, vcov. = NULL, df = NULL, ...) {
#   ## obtain vcov
#   v <- if (is.null(vcov.)) {
#     vcov(x)
#   } else if (is.function(vcov.)) {
#     vcov.(x)
#   } else {
#     vcov.
#   }
#
#   ## nasty hack: replace coefficients so that their names match the vcov() method
#   x$coefficients <- structure(as.vector(x$coefficients),
#                               .Names = colnames(vcov(x)))
#
#   ## call default method
#   coeftest.default(x, vcov. = v, df = df, ...)
# }
#
# coeftest.survreg <- function(x, vcov. = NULL, df = Inf, ...) {
#
#   if (is.null(vcov.)) {
#     v <- vcov(x)
#   } else {
#     if (is.function(vcov.)) {
#       v <- vcov.(x)
#     } else {
#       v <- vcov.
#     }
#   }
#
#   if (length(x$coefficients) < NROW(x$var)) {
#     x$coefficients <- c(x$coefficients, "Log(scale)" = log(x$scale))
#   }
#
#   coeftest.default(x, vcov. = v, df = df, ...)
#
# }

#### robust predictions #####################################################

#' @importFrom stats vcov model.frame terms delete.response
predict_rob <- function(model, .vcov = vcov(model), newdata = NULL,
                        se.fit = TRUE, dispersion = NULL, terms = NULL,
                        type = c("link", "response", "terms"),
                        na.action = na.pass, ...) {

  if (is.null(newdata)) {newdata <- model.frame(model)}

  tt <- terms(model)
  Terms <- delete.response(tt)
  m <- model.frame(Terms, newdata, na.action = na.action,
                   xlev = model$xlevels)
  m.mat <- model.matrix(Terms, m, contrasts.arg = model$contrasts)
  m.coef <- coef(model)

  # if (inherits(object, "survreg")) {dispersion <- 1}
  # if (is.null(dispersion) || dispersion == 0) {
  #   dispersion <- summary(object, dispersion = dispersion)$dispersion
  # }
  # residual.scale <- as.vector(sqrt(dispersion))
  # pred <- predict.lm(object, newdata, se.fit, scale = residual.scale,
  #                    type = ifelse(type == "link", "response", type),
  #                    terms = terms, na.action = na.action)

  offset <- rep(0, nrow(m.mat))
  if (!is.null(off.num <- attr(tt, "offset"))) {
    for (i in off.num) {
      offset <- offset + eval(attr(tt, "variables")[[i + 1]], newdata)
    }
  }

  if (!is.null(model$call$offset)) {
    offset <- offset + eval(model$call$offset, newdata)
  }

  n <- length(model$residuals)
  p <- model$rank
  p1 <- seq_len(p)
  piv <- if (p) {qr(model)$pivot[p1]}

  if (p < ncol(m.mat) && !(missing(newdata) || is.null(newdata))) {
    warning("prediction from a rank-deficient fit may be misleading")
  }

  fit <- drop(m.mat[, piv, drop = FALSE] %*% m.coef[piv])

  if (!is.null(offset)) {
    fit <- fit + offset
  }

  # fit <- as.vector(m.mat %*% m.coef)
  se.fit <- sqrt(diag(m.mat %*% .vcov %*% t(m.mat)))

  type <- type[1]

  switch(type, response = {
    se.fit <- se.fit * abs(family(model)$mu.eta(fit))
    fit <- family(model)$linkinv(fit)
  }, link = , terms = )

  return(list(fit = fit, se.fit = se.fit))

}

## Kludge to fix glht compatibility
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(generics::tidy, glht)
#' } else {
#'   export(tidy.glht)
#' }
tidy.glht <- function (x, conf.int = FALSE, conf.level = 0.95, ...) {
  if (!conf.int) {
    tibble(lhs = rownames(x$linfct), rhs = x$rhs, estimate = stats::coef(x))
  } else {
    confs <- as.data.frame(confint(x, level = conf.level)$confint)
    tibble(lhs = rownames(x$linfct), rhs = x$rhs, estimate = stats::coef(x),
           conf.low = confs$lwr, conf.high = confs$upr)
  }
}

#' @importFrom tibble tibble as_tibble
#' @importFrom stats confint
#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(generics::tidy, summary.glht)
#' } else {
#'   export(tidy.summary.glht)
#' }
tidy.summary.glht <- function (x, conf.int = FALSE, conf.level = 0.95, ...) {
  lhs_rhs <- tibble(lhs = rownames(x$linfct), rhs = x$rhs)
  coef <- as_tibble(x$test[c("coefficients", "sigma", 
                             "tstat", "pvalues")])
  names(coef) <- c("estimate", "std.error", "statistic", 
                   "p.value")
  out <- as_tibble(cbind(lhs_rhs, coef))
  if (conf.int) {
    confs <- as.data.frame(confint(x, level = conf.level)$confint)
    out$conf.low <- confs$lwr
    out$conf.high <- confs$upr
  }
  out
}