
#### Programming helpers #####################################################

## Making a "opposite of %in%" or "not %in%" function to simplify code
`%nin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))

## Quicker way to get last item of vector
last <- function(x) {return(x[length(x)])}
## Just so code reads more clearly when using last(x)
first <- function(x) {return(x[1])}

#### summ helpers ############################################################

## Automates the adding of the significance stars to the output
## Also rounds the digits and spits out a table from a matrix
## Since it's doing double duty, also can skip the p vals if requested

add_stars <- function(table, digits, p_vals) {

  if (p_vals == TRUE) { # Only do this if showing p values

    # Grab the unrounded p values
    pvals <- table[,"p"]

    # Create a NA-filled vector to speed up the loop
    sigstars <- rep(NA, times = nrow(table))

    # Add the stars
    for (y in 1:length(pvals)) {

      if (pvals[y] > 0.1) {
        sigstars[y] <- ""
      } else if (pvals[y] <= 0.1 & pvals[y] > 0.05) {
        sigstars[y] <- "."
      } else if (pvals[y] > 0.01 & pvals[y] <= 0.05) {
        sigstars[y] <- "*"
      } else if (pvals[y] > 0.001 & pvals[y] <= 0.01) {
        sigstars[y] <- "**"
      } else if (pvals[y] <= 0.001) {
        sigstars[y] <- "***"
      }

    }

  }

  # Round the values
  table <- round_df_char(table, digits)

  # Can't do this in the first conditional because of the need to round
  if (p_vals == TRUE) {
    # Get the colnames so I can fix them after cbinding
    tnames <- colnames(table)
    # put in the stars
    table <- cbind(table, sigstars)
    # Makes the name for the stars column empty
    colnames(table) <- c(tnames, "")
  }

  #table <- as.table(table)

  return(table)

}

## Creates clean data frames for printing. Aligns decimal points,
## padding extra space with " " (or another value), and rounds values.
## Outputs a data.frame of character vectors containing the corrected
## values.

round_df_char <- function(df, digits, pad = " ") {
  nas <- is.na(df)
  if (!is.data.frame(df)) {
    # Fixes a sneaky error
    df <- as.data.frame.matrix(df, stringsAsFactors = FALSE)

  }

  rn <- rownames(df)
  cn <- colnames(df)
  df <- as.data.frame(lapply(df, function(col) {
    if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
      as.numeric(as.character(col))
    } else {
      col
    }
  }), stringsAsFactors = FALSE)

  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  # Using a format function here to force trailing zeroes to be printed
  # "formatC" allows signed zeros (e.g., "-0.00")
  df <- as.data.frame(lapply(df, formatC, digits = digits, format = "f"),
                      stringsAsFactors = FALSE)

  # Convert missings to blank character
  if (any(nas)) {
    df[nas] <- ""
  }

  # Here's where we align the the decimals, thanks to Noah for the magic.
  for (i in which(nums)) {
    if (any(grepl(".", df[[i]], fixed = TRUE))) {

      s <- strsplit(df[[i]], ".", fixed = TRUE)
      lengths <- lengths(s)
      digits.r.of.. <- sapply(seq_along(s), function(x) {

        if (lengths[x] > 1) {
          nchar(s[[x]][lengths[x]])
        } else {
          0
        }
      })

      df[[i]] <- sapply(seq_along(df[[i]]), function(x) {
        if (df[[i]][x] == "") {
          ""
        } else if (lengths[x] <= 1) {
          paste0(c(df[[i]][x],
                   rep(".", pad == 0),
                   rep(pad, max(digits.r.of..) -
                         digits.r.of..[x] + as.numeric(pad != 0))),
                 collapse = "")
        } else {
          paste0(c(df[[i]][x], rep(pad, max(digits.r.of..) - digits.r.of..[x])),
                    collapse = "")
        }
      })
    }
  }

  if (length(rn) > 0) rownames(df) <- rn
  if (length(cn) > 0) names(df) <- cn

  return(df)
}

do_robust <- function(model, robust, cluster, data) {

  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("When using robust SEs, you need to have the \'sandwich\'",
         " package.\n Please install it or set robust to FALSE.",
         call. = FALSE)
  }

  if (robust == TRUE) {
    robust <- "HC3"
  }

  if (is.character(cluster)) {

    call <- getCall(model)
    if (is.null(data)) {
      d <- eval(call$data, envir = environment(formula(model)))
    } else {
      d <- data
    }

    cluster <- d[[cluster]]
    use_cluster <- TRUE

  } else if (length(cluster) > 1) {

    if (!is.factor(cluster) & !is.numeric(cluster)) {

      warning("Invalid cluster input. Either use the name of the variable",
              " in the input data frame\n or provide a numeric/factor vector.",
              " Cluster is not being used in the reported SEs.")
      cluster <- NULL
      use_cluster <- FALSE

    } else {

      use_cluster <- TRUE

    }

  } else {

    use_cluster <- FALSE

  }

  if (robust %in% c("HC4", "HC4m", "HC5") & is.null(cluster)) {
    # vcovCL only goes up to HC3
    coefs <- sandwich::vcovHC(model, type = robust)

  } else if (robust %in% c("HC4", "HC4m", "HC5") & !is.null(cluster)) {

    stop("If using cluster-robust SEs, robust.type must be HC3 or lower.")

  } else {

    coefs <- sandwich::vcovCL(model, cluster = cluster, type = robust)

  }

  vcov <- coefs
  coefs <- coeftest(model, coefs)
  ses <- coefs[,2]
  ts <- coefs[,3]
  ps <- coefs[,4]

  list(coefs = coefs, ses = ses, ts = ts, ps = ps, use_cluster = use_cluster,
       robust = robust, cluster = cluster, vcov = vcov)

}

part_corr <- function(ts, df.int, rsq, robust, n) {

  # Need df
  ## Using length of t-value vector to get the p for DF calculation
  p.df <- length(ts) - df.int # If intercept, don't include it
  df.resid <- n - p.df - 1

  partial_corrs <- ts / sqrt(ts^2 + df.resid)
  if (df.int == 1) {
    partial_corrs[1] <- NA # Intercept partial corr. isn't interpretable
  }

  semipart_corrs <- (ts * sqrt(1 - rsq))/sqrt(df.resid)
  if (df.int == 1) {
    semipart_corrs[1] <- NA # Intercept partial corr. isn't interpretable
  }

  if (!identical(FALSE, robust)) {

    warning("Partial/semipartial correlations calculated based on robust",
            " t-statistics.\n See summ.lm documentation for cautions on",
            " \ninterpreting partial and semipartial correlations alongside",
            " robust standard errors.")

  }

  list(partial_corrs = partial_corrs, semipart_corrs = semipart_corrs)

}

dep_checks <- function(dots) {

  scale <- NULL
  transform.response <- NULL
  robust <- NULL

  if ("standardize" %in% names(dots)) {
    warning("The standardize argument is deprecated. Please use 'scale'",
            " instead.")
    scale <- dots$standardize
  }

  if ("standardize.response" %in% names(dots)) {
    warning("The standardize.response argument is deprecated. Please use",
            " 'transform.response' instead.")
    transform.response <- dots$standardize.response
  }

  if ("scale.response" %in% names(dots)) {
    warning("The scale.response argument is deprecated. Please use",
            " 'transform.response' instead.")
    transform.response <- dots$scale.response
  }

  if ("center.response" %in% names(dots)) {
    warning("The center.response argument is deprecated. Please use",
            " 'transform.response' instead.")
    transform.response <- dots$center.response
  }

  if ("robust.type" %in% names(dots)) {
    warning("The robust.type argument is deprecated. Please specify the type",
            " as the value for the 'robust' argument instead.")
    robust <- dots$robust.type
  }

  list(scale = scale, transform.response = transform.response, robust = robust)

}

scale_statement <- function(scale, center, transform.response, n.sd) {
  part_1 <- "Continuous "
  part_2 <- ifelse(transform.response, "variables", "predictors")
  part_3 <- " are mean-centered"
  part_4 <- if (scale) { " and scaled by " } else { NULL }
  part_5 <- if (scale) { paste(n.sd, "s.d") } else { NULL }

  if (scale == FALSE & center == FALSE) {
    return(NULL)
  } else {
    paste0(part_1, part_2, part_3, part_4, part_5, ".")
  }
}

### pseudo-R2 ################################################################

## This is taken from pscl package, I don't want to list it as import for
## this alone. The return object needs tweaking for me anyway
pR2Work <- function(llh, llhNull, n) {
  McFadden <- 1 - llh / llhNull
  G2 <- -2 * (llhNull - llh)
  r2ML <- 1 - exp(-G2 / n)
  r2ML.max <- 1 - exp(llhNull * 2 / n)
  r2CU <- r2ML / r2ML.max
  out <- NULL
  out$llh <- llh
  out$llhNull <- llhNull
  out$G2 <- G2
  out$McFadden <- McFadden
  out$r2ML <- r2ML
  out$r2CU <- r2CU
  out
}

## Namespace issues require me to define pR2 here
pR2 <- function(object) {

  llh <- getLL(object)

  frame <- model.frame(object)

  .weights <- model.weights(frame)
  .offset <- model.offset(frame)

  objectNull <- j_update(object, ~ 1, weights = .weights, offset = .offset)

  llhNull <- getLL(objectNull)
  n <- dim(object$model)[1]
  pR2Work(llh,llhNull,n)

}

# Enabling support for quasi families
getLL <- function(object) {

  fam <- family(object)
  link <- fam$link
  fam <- fam$family
  quasis <- c("quasibinomial","quasipoisson","quasi")
  if (fam %nin% quasis) {
    return(logLik(object))
  } else {
    if (fam == "quasipoisson") {
      logLik(j_update(object, family = poisson(link = link)))
    } else if (fam == "quasibinomial") {
      logLik(j_update(object, family = binomial(link = link)))
    } else {
      NA
    }
  }

}

### Have to specify data differently than pscl to fix namespace issues
# pR2 <- function(object) {
#   llh <- suppressWarnings(logLik(object))
#   if (class(object)[1] %in% c("svyglm","svrepglm")) {
#     objectNull <- suppressWarnings(update(object, ~ 1,
#                                           design = object$survey.design))
#   } else {
#   objectNull <- suppressWarnings(update(object, ~ 1,
#                                         data = model.frame(object)))
#   }
#   llhNull <- logLik(objectNull)
#   n <- dim(object$model)[1]
#   pR2Work(llh,llhNull,n)
# }

#### Weighted SD ############################################################
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
                     weights = NULL, ...) {
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



  eval(call, env, parent.frame())
}

### cut2 ######################################################################

## Taken from Hmisc package to avoid importing for a minor feature
#' @importFrom stats approx
#'
cut2 <- function(x, cuts, m = 150, g, levels.mean = FALSE, digits,
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
                                                        cum.used)/(g - j + 1), method = "constant",
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
      stop(paste("categorization error in cut2.  Values of x not appearing in any interval:\n",
                 paste(format(x[ss], digits = 12), collapse = " "),
                 "\nLower endpoints:", paste(format(low, digits = 12),
                                             collapse = " "), "\nUpper endpoints:", paste(format(up,
                                                                                                 digits = 12), collapse = " ")))
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
  }
  attr(y, "class") <- "factor"
  # if (length(xlab))
  #   label(y) <- xlab
  y
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

#### merMod prediction #######################################################

#' @importFrom stats vcov model.frame terms delete.response
predict_mer <- function(model, newdata = NULL, use_re_var = TRUE,
                        se.fit = TRUE, dispersion = NULL, terms = NULL,
                        allow.new.levels = TRUE,
                        type = c("link", "response", "terms"),
                        na.action = na.pass, re.form = NULL,
                        boot = FALSE, sims = 100, ...) {

  if (is.null(newdata) && is.null(re.form)) {
    ## raw predict() call, just return fitted values
    ##   (inverse-link if appropriate)
    if (lme4::isLMM(model) || lme4::isNLMM(model)) {
      ## make sure we do *NOT* have NAs in fitted object
      fit <- na.omit(fitted(model))
    } else { ## inverse-link
      fit <-  switch(type, response = model@resp$mu, ## == fitted(object),
                      link = model@resp$eta)
      if (is.null(nm <- rownames(model.frame(model)))) {
        nm <- seq_along(fit)
      }
      names(fit) <- nm
    }
    fit.na.action <- NULL

    # Need this for SEs
    X <- lme4::getME(model, "X")
    X.col.dropped <- attr(X, "col.dropped")

    ## flow jumps to end for na.predict
  } else { ## newdata and/or re.form

    fit.na.action <- attr(model@frame, "na.action")  ## original NA action

    nobs <- if (is.null(newdata)) nrow(model@frame) else nrow(newdata)
    fit <- rep(0,nobs)

    X <- lme4::getME(model, "X")
    X.col.dropped <- attr(X, "col.dropped")
    ## modified from predict.glm ...
    if (is.null(newdata)) {
      ## Use original model 'X' matrix and offset
      ## orig. offset: will be zero if there are no matches ...
      offset <- model.offset(model.frame(model))
      if (is.null(offset)) offset <- 0
    } else {  ## new data specified
      ## evaluate new fixed effect
      RHS <- formula(substitute(~R,
                list(R = RHSForm(formula(model, fixed.only = TRUE)))))
      Terms <- terms(model, fixed.only = TRUE)
      mf <- model.frame(model, fixed.only = TRUE)
      isFac <- vapply(mf, is.factor, FUN.VALUE = TRUE)
      ## ignore response variable
      isFac[attr(Terms,"response")] <- FALSE
      orig_levs <- if (length(isFac) == 0) NULL else lapply(mf[isFac], levels)

      mfnew <- suppressWarnings(
        model.frame(delete.response(Terms), newdata,
                    na.action = na.action, xlev = orig_levs))

      X <- model.matrix(RHS, data = mfnew,
                        contrasts.arg = attr(X,"contrasts"))
      ## hack to remove unused interaction levels?
      ## X <- X[,colnames(X0)]

      offset <- 0 # rep(0, nrow(X))
      tt <- terms(model)
      if (!is.null(off.num <- attr(tt, "offset"))) {
        for (i in off.num)
          offset <- offset + eval(attr(tt,"variables")[[i + 1]], newdata)
      }
      fit.na.action <- attr(mfnew, "na.action")
      ## only need to drop if new data specified ...
      if (is.numeric(X.col.dropped) && length(X.col.dropped) > 0) {
        X <- X[, -X.col.dropped, drop = FALSE]
      }

      fit <- drop(X %*% lme4::fixef(model))
      fit <- fit + offset

    }

  }  ## newdata/newparams/re.form

  if (se.fit == TRUE & boot == FALSE) {
    .vcov <- as.matrix(vcov(model))
    se.fit <- sqrt(diag(X %*% .vcov %*% t(X)))

    if (lme4::isGLMM(model)) {
      switch(type, response = {
        se.fit <- se.fit * abs(family(model)$mu.eta(fit))
        fit <- model@resp$family$linkinv(fit)
      }, link = , terms = )
    } else {
      se.fit <- se.fit * abs(family(model)$mu.eta(fit))
    }

    re_vcov <- lme4::VarCorr(model)
    # Sum each random intercept variance
    re_variances <- sum(sapply(re_vcov, function(x) {x[1]}))

    if (use_re_var == TRUE) {
      se.fit <- se.fit + re_variances
    }

  } else if (se.fit == TRUE & boot == TRUE) {

    bootfun <- function(model) {
      drop( X  %*% lme4::fixef(model) )
      }
    bo <- lme4::bootMer(model, FUN = bootfun, nsim = sims, .progress = "txt")
    fit <- sapply(as.data.frame(bo$t), median)
    upper <- sapply(as.data.frame(bo$t), quantile, probs = .975)
    lower <- sapply(as.data.frame(bo$t), quantile, probs = .025)

    if (lme4::isGLMM(model)) {
      switch(type, response = {
        upper <- upper * abs(family(model)$mu.eta(fit))
        lower <- lower * abs(family(model)$mu.eta(fit))
        fit <- model@resp$family$linkinv(fit)
      }, link = , terms = )
    } else {
      upper <- upper * abs(family(model)$mu.eta(fit))
      lower <- lower * abs(family(model)$mu.eta(fit))
    }

  }

  type <- type[1]
  if (!noReForm(re.form)) {
    if (is.null(re.form))
      re.form <- reOnly(formula(model)) # RE formula only
    rfd <- if (is.null(newdata)) {model@frame} else {newdata}
    newRE <- mkNewReTrms(model, rfd, re.form, na.action = na.action,
                         allow.new.levels = allow.new.levels)
    REvals <- base::drop(as(newRE$b %*% newRE$Zt, "matrix"))
    fit <- fit + REvals
  }

  if (se.fit == FALSE & lme4::isGLMM(model)) {
    switch(type, response = {
      fit <- model@resp$family$linkinv(fit)
    }, link = , terms = )
  }

  if (se.fit == TRUE & boot == FALSE) {
    return(list(fit = fit, se.fit = se.fit))
  } else if (se.fit == TRUE & boot == TRUE) {
    return(list(fit = fit, upper = upper, lower = lower))
  } else {
    return(fit)
  }


}
