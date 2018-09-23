
#### Programming helpers #####################################################

## Making a "opposite of %in%" or "not %in%" function to simplify code
`%nin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))

## Quicker way to get last item of vector
last <- function(x) {return(x[length(x)])}
## Just so code reads more clearly when using last(x)
first <- function(x) {return(x[1])}

## Print rounded numbers with all requested digits, signed zeroes
num_print <- function(x, digits = getOption("jtools-digits", 2),
                      format = "f") {
  formatC(x, digits = digits, format = "f")
}

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

# Automate the addition of newline characters for long strings
wrap_str <- function(..., sep = "") {
  paste0(strwrap(paste(..., sep = sep), width = 0.95 * getOption("width", 80)),
         collapse = "\n")
}

# Go ahead and wrap the cat function too
cat_wrap <- function(..., brk = "") {
  cat(wrap_str(...), brk, sep = "")
}

# Define orange crayon output
orange <- crayon::make_style("orange")

# Like cat_wrap but for warnings
warn_wrap <- function(..., call. = FALSE, brk = "\n") {
  warning(orange(wrap_str(...)), brk, call. = call.)
}

# Like cat_wrap but for errors
stop_wrap <- function(..., call. = FALSE, brk = "\n") {
  stop(red(wrap_str(...)), brk, call. = call.)
}

# Like cat_wrap but for messages
#' @importFrom crayon cyan
msg_wrap <- function(..., brk = "\n") {
  message(cyan(wrap_str(...)), brk)
}

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

# Get levels if they exist, otherwise unique
ulevels <- function(x) {
  if (!is.null(levels(x))) {
    return(levels(x))
  } else {
    if (!is.numeric(x)) {
      return(unique(x))
    } else {
      return(sort(unique(x)))
    }
  }
}

#### Quantile regression helpers ##############################################

# Get R1 (Koenker & Machado, 1999)
#' @importFrom stats model.response
R1 <- function(model) {
  rho_1 <- model$rho
  null_resids <- model.frame(model)[[1]] - quantile(model.frame(model)[[1]],
                                                  model$tau)

  rho_0 <- sum(null_resids * (model$tau - (null_resids < 0)))

  return(1 - (rho_1 / rho_0))
}

rq_model_matrix <- function(object) {
  mt <- terms(object)
  m <- model.frame(object)
  y <- model.response(m)
  if (object$method == "sfn")
    x <- object$model$x
  else x <- model.matrix(mt, m, contrasts = object$contrasts)
  return(x)
}

rq.fit.br <- function(x, y, tau = 0.5, alpha = 0.1, ci = FALSE,
                      iid = TRUE, interp = TRUE, tcrit = TRUE, ...) {

  rq.fit.br(x, y, tau = tau, alpha = alpha, ci = ci, iid = iid,
            interp = interp, tcrit = tcrit)

}

#### Weighted helpers ########################################################

wtd.sd <- function(x, w) {
  # Get the mean
  xm <- weighted.mean(x, w, na.rm = TRUE)
  # Squaring the weighted deviations and dividing by weighted N - 1
  variance <- sum((w * (x - xm)^2) / (sum(w) - 1), na.rm = TRUE)
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

#### Regex helper ############################################################

# Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}

#### ncvTest #################################################################

## Taken from car package with modifications so it doesn't break j_summ
ncvTest <- function(model, ...){
  UseMethod("ncvTest")
}

ncvTest.lm <- function(model, var.formula, ...) {
  # data <- getCall(model)$data
  # model <- if (!is.null(data)){
  #   data <- eval(data, envir=environment(formula(model)))
  #   update(model, formula(model), na.action="na.exclude", data=data)
  # }
  # else update(model, formula(model), na.action="na.exclude")
  sumry <- summary(model)
  residuals <- residuals(model, type = "pearson")
  S.sq <- stats::df.residual(model) * (sumry$sigma)^2 / sum(!is.na(residuals))
  .U <- (residuals^2) / S.sq
  if (missing(var.formula)) {
    mod <- lm(.U ~ fitted.values(model))
    varnames <- "fitted.values"
    var.formula <- ~ fitted.values
    df <- 1
  }
  else {
    form <- as.formula(paste0(".U ~ ", as.character(var.formula)[[2]]))
    mod <- if (!is.null(data)) {
      data$.U <- .U
      lm(form, data = data)
    }
    else lm(form)
    df <- sum(!is.na(coefficients(mod))) - 1
  }
  SS <- anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS/2
  result <- list(formula = var.formula, formula.name = "Variance",
                 ChiSquare = Chisq, Df = df,
                 p = stats::pchisq(Chisq, df, lower.tail = FALSE),
                 test = "Non-constant Variance Score Test")
  class(result) <- "chisqTest"
  result
}

# print.chisqTest <- function(x, ...) {
#   title <- if (!is.null(x$test)) x$test else "Chisquare Test"
#   cat(title,"\n")
#   if (!is.null(x$formula)) cat(x$formula.name,
#                                "formula:", as.character(x$formula), "\n")
#   cat("Chisquare =", x$ChiSquare,"   Df =", x$Df,
#       "    p =", x$p, "\n")
#   invisible(x)
# }

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



### cut2 ######################################################################

## Taken from Hmisc package to avoid importing for a minor feature
## Added "levels.median"
#' @importFrom stats approx
#'
cut2 <- function(x, cuts, m = 150, g, levels.mean = FALSE,
                 levels.median = FALSE, digits,
                 minmax = TRUE, oneval = TRUE, onlycuts = FALSE) {
  method <- 1
  x.unique <- sort(unique(c(x[!is.na(x)], if (!missing(cuts)) cuts)))
  min.dif <- min(diff(x.unique))/2
  min.dif.factor <- 1
  if (missing(digits))
    digits <- if (levels.mean)
      5
  else 3
  oldopt <- options("digits")
  options(digits = digits)
  on.exit(options(oldopt))
  xlab <- attr(x, "label")
  if (missing(cuts)) {
    nnm <- sum(!is.na(x))
    if (missing(g))
      g <- max(1, floor(nnm/m))
    if (g < 1)
      stop("g must be >=1, m must be positive")
    options(digits = 15)
    n <- table(x)
    xx <- as.double(names(n))
    options(digits = digits)
    cum <- cumsum(n)
    m <- length(xx)
    y <- as.integer(ifelse(is.na(x), NA, 1))
    labs <- character(g)
    cuts <- approx(cum, xx, xout = (1:g) * nnm/g, method = "constant",
                   rule = 2, f = 1)$y
    cuts[length(cuts)] <- max(xx)
    lower <- xx[1]
    upper <- 1e+45
    up <- low <- double(g)
    i <- 0
    for (j in 1:g) {
      cj <- if (method == 1 || j == 1)
        cuts[j]
      else {
        if (i == 0)
          stop("program logic error")
        s <- if (is.na(lower))
          FALSE
        else xx >= lower
        cum.used <- if (all(s))
          0
        else max(cum[!s])
        if (j == m)
          max(xx)
        else if (sum(s) < 2)
          max(xx)
        else approx(cum[s] - cum.used, xx[s], xout = (nnm -
                                                        cum.used)/(g - j + 1),
                    method = "constant",
                    rule = 2, f = 1)$y
      }
      if (cj == upper)
        next
      i <- i + 1
      upper <- cj
      y[x >= (lower - min.dif.factor * min.dif)] <- i
      low[i] <- lower
      lower <- if (j == g)
        upper
      else min(xx[xx > upper])
      if (is.na(lower))
        lower <- upper
      up[i] <- lower
    }
    low <- low[1:i]
    up <- up[1:i]
    variation <- logical(i)
    for (ii in 1:i) {
      r <- range(x[y == ii], na.rm = TRUE)
      variation[ii] <- diff(r) > 0
    }
    if (onlycuts)
      return(unique(c(low, max(xx))))
    flow <- format(low)
    fup <- format(up)
    bb <- c(rep(")", i - 1), "]")
    labs <- ifelse(low == up | (oneval & !variation), flow,
                   paste("[", flow, ",", fup, bb, sep = ""))
    ss <- y == 0 & !is.na(y)
    if (any(ss))
      stop_wrap("categorization error in cut2.  Values of x not appearing in
                any interval:", paste(format(x[ss], digits = 12),
                                        collapse = " "),
                 "Lower endpoints:", paste(format(low, digits = 12),
                                             collapse = " "),
                "\nUpper endpoints:", paste(format(up, digits = 12),
                                            collapse = " "))
    y <- structure(y, class = "factor", levels = labs)
  }
  else {
    if (minmax) {
      r <- range(x, na.rm = TRUE)
      if (r[1] < cuts[1])
        cuts <- c(r[1], cuts)
      if (r[2] > max(cuts))
        cuts <- c(cuts, r[2])
    }
    l <- length(cuts)
    k2 <- cuts - min.dif
    k2[l] <- cuts[l]
    y <- cut(x, k2)
    if (!levels.mean) {
      brack <- rep(")", l - 1)
      brack[l - 1] <- "]"
      fmt <- format(cuts)
      labs <- paste("[", fmt[1:(l - 1)], ",", fmt[2:l],
                    brack, sep = "")
      if (oneval) {
        nu <- table(cut(x.unique, k2))
        if (length(nu) != length(levels(y)))
          stop("program logic error")
        levels(y) <- ifelse(nu == 1, c(fmt[1:(l - 2)],
                                       fmt[l]), labs)
      }
      else levels(y) <- labs
    }
  }
  if (levels.mean) {
    means <- tapply(x, y, function(w) mean(w, na.rm = TRUE))
    levels(y) <- format(means)
  } else if (levels.median) {
    medians <- tapply(x, y, function(w) median(w, na.rm = TRUE))
    levels(y) <- format(medians)
  }
  attr(y, "class") <- "factor"
  # if (length(xlab))
  #   label(y) <- xlab
  y
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

### coeftest ##################################################################

## Taken from lmtest package with changes

coeftest <- function(x, vcov. = NULL, df = NULL, ...) {
  UseMethod("coeftest")
}

#' @importFrom stats pnorm

coeftest.default <- function(x, vcov. = NULL, df = NULL, ...) {
  ## use S4 methods if loaded
  if (requireNamespace("stats4", quietly = TRUE)) {
    coef0 <- stats4::coef
    vcov0 <- stats4::vcov
  } else {
    coef0 <- coef
    vcov0 <- vcov
  }

  ## extract coefficients and standard errors
  est <- coef0(x)
  if (is.null(vcov.)) {
    se <- vcov0(x)
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
  class(rval) <- "coeftest"
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
