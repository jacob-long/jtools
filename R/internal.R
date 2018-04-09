
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
warn_wrap <- function(..., call. = TRUE, brk = "\n") {
  warning(orange(wrap_str(...)), brk, call. = call.)
}

# Like cat_wrap but for errors
stop_wrap <- function(..., call. = TRUE, brk = "\n") {
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
    for (y in seq_along(pvals)) {

      if (is.na(pvals[y]) || pvals[y] > 0.1) {
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

  return(table)

}

## Creates clean data frames for printing. Aligns decimal points,
## padding extra space with " " (or another value), and rounds values.
## Outputs a data.frame of character vectors containing the corrected
## values.

round_df_char <- function(df, digits, pad = " ", na_vals = NA) {

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
  df <- as.data.frame(lapply(df, num_print, digits = digits),
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

  # Insert NA placeholders
  df[df == ""] <- na_vals

  return(df)
}

## This function gets the robust SEs

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

## Put together the coefficient table

create_table <- function(params, which.cols, ivs) {

  if (any(which.cols %nin% names(params))) {
    stop_wrap("One of the requested columns does not exist.")
  }

  coefs <- params[["Est."]]

  params <- params[unlist(which.cols)]

  mat <- matrix(nrow = length(ivs), ncol = length(which.cols))
  rownames(mat) <- ivs
  colnames(mat) <- which.cols

  # Put each column in the return matrix
  for (i in seq_along(params)) {

    # Handle rank-deficient models
    if (length(coefs) > length(params[[i]])) {
      # Creating a vector the length of ucoefs (which has the NAs)
      temp_vec <- rep(NA, times = length(coefs))
      # Now I replace only at indices where ucoefs is non-missing
      temp_vec[which(!is.na(coefs))] <- params[[i]]
      # Now replace params[[i]] with the vector that includes the missings
      params[[i]] <- temp_vec
    }

    mat[,i] <- params[[i]]

  }

  return(mat)

}

## Decide which columns will be included in the output

which_columns <- function(which.cols, confint, ci.labs, vifs, pvals, t.col,
                          exp = NULL, others = NULL) {

  if (!is.null(which.cols)) {
    return(which.cols)
  } else {
    if (is.null(exp) || exp == FALSE) {
      cols <- c("Est.")
    } else {
      cols <- c("exp(Est.)")
    }
    if (confint == TRUE) {
      cols <- c(cols, ci.labs)
    } else {
      cols <- c(cols, "S.E.")
    }
    cols <- c(cols, t.col)
    if (pvals == TRUE) {cols <- c(cols, "p")}
    if (vifs == TRUE) {cols <- c(cols, "VIF")}
    if (!is.null(others)) {cols <- c(cols, others)}
    return(cols)
  }

}

## Partial and semipartial correlation calculations

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

## Checking for presence of deprecated 

dep_checks <- function(dots) {

  scale <-  transform.response <- robust <- odds.ratio <- NULL

  if ("standardize" %in% names(dots)) {
    warn_wrap("The standardize argument is deprecated. Please use 'scale'
              instead.", call. = FALSE)
    scale <- dots$standardize
  }

  if ("standardize.response" %in% names(dots)) {
    warn_wrap("The standardize.response argument is deprecated. Please use
              'transform.response' instead.", call. = FALSE)
    transform.response <- dots$standardize.response
  }

  if ("scale.response" %in% names(dots)) {
    warn_wrap("The scale.response argument is deprecated. Please use
              'transform.response' instead.", call. = FALSE)
    transform.response <- dots$scale.response
  }

  if ("center.response" %in% names(dots)) {
    warn_wrap("The center.response argument is deprecated. Please use
              'transform.response' instead.", call. = FALSE)
    transform.response <- dots$center.response
  }

  if ("robust.type" %in% names(dots)) {
    warn_wrap("The robust.type argument is deprecated. Please specify the type
               as the value for the 'robust' argument instead.", call. = FALSE)
    robust <- dots$robust.type
  }

  if ("odds.ratio" %in% names(dots)) {
    warn_wrap("The odds.ratio argument is deprecated. Use 'exp' instead.",
              call. = FALSE)
    robust <- dots$robust.type
  }

  list(scale = scale, transform.response = transform.response, robust = robust,
       exp = odds.ratio)

}

## Form the sentence that says what happened with variable transformations

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

## Adapted from car::vif
vif <- function(mod, ...) {

  if (any(is.na(coef(mod)))) {
    stop_wrap("VIFs cannot be calculated because there are aliased
              coefficients in the model.")
  }

  v <- vcov(mod)
  assign <- attr(model.matrix(mod), "assign")

  if ("(Intercept)" %in% names(coefficients(mod))) {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else {
    warning("No intercept: VIFs may not be sensible.")
  }

  terms <- labels(terms(mod))
  n.terms <- length(terms)

  if (n.terms < 2) {
    stop_wrap("VIFS cannot be calculated because the model contains fewer
              than 2 terms.")
  }

  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")

  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) *
      det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- result[, 1]
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    expand_fun <- function(x) {
      if ("(Intercept)" %in% names(coefficients(mod))) {
        rep(x, times = table(attr(model.matrix(mod), "assign")[-1]))
      } else {
        rep(x, times = table(attr(model.matrix(mod), "assign")))
      }
    }
    result <- apply(result, MARGIN = 2, FUN = expand_fun)
  }

  return(result)

}

## Print the model info output

print_mod_info <- function(missing, n, dv, type) {
  if (is.null(missing) || missing == 0) {
    cat(underline("MODEL INFO:"), "\n",
        italic("Observations:"), " ",  n, "\n",
        italic("Dependent Variable:"), " ", dv, "\n", sep = "")
  } else {
    cat(underline("MODEL INFO:"), "\n",
        italic("Observations:"), " ", n, " (", missing,
        " missing obs. deleted)", "\n",
        italic("Dependent Variable:"), " ", dv, "\n", sep = "")
  }
  cat(italic("Type:"), type, "\n\n")
}

## Print model fit info

print_mod_fit <- function(stats) {
  cat(underline("MODEL FIT:"), "\n", sep = "")
  cat(stats, "\n\n")
}

## Print line about standard errors

print_se_info <- function(robust, use_cluster, ols = FALSE) {

  if (identical(FALSE, robust)) {

    cat(italic("Standard errors:",  ifelse(ols, "OLS", "MLE")), "\n", sep = "")

  } else {

    if (robust == TRUE) {robust <- "HC3"}

    cat(italic("Standard errors:"), sep = "")

    if (use_cluster == FALSE) {

      cat(" Robust, ", italic("type = "), robust, "\n", sep = "")

    } else if (use_cluster == TRUE) {

      cat(" Cluster-robust, ", italic("type = "), robust, "\n", sep = "")

    }

  }

}

## Create confidence interval column labels

make_ci_labs <- function(ci.width) {

  alpha <- (1 - ci.width) / 2

  lci_lab <- 0 + alpha
  lci_lab <- paste(round(lci_lab * 100, 1), "%", sep = "")

  uci_lab <- 1 - alpha
  uci_lab <- paste(round(uci_lab * 100, 1), "%", sep = "")

  list(lci = lci_lab, uci = uci_lab)

}

### pseudo-R2 ################################################################

## This is taken from pscl package, I don't want to list it as import for
## this alone. The return object needs tweaking for me anyway
pR2Work <- function(llh, llhNull, n, object = NULL, objectNull = NULL) {
  McFadden <- as.numeric(1 - llh / llhNull)
  G2 <- as.numeric(-2 * (llhNull - llh))
  r2ML <- as.numeric(1 - exp(-G2 / n))
  r2ML.max <- as.numeric(1 - exp(llhNull * 2 / n))
  r2CU <- r2ML / r2ML.max

  out <- NULL
  out$llh <- llh
  out$llhNull <- llhNull

  out$G2 <- G2
  out$McFadden <- McFadden
  out$r2ML <- r2ML
  out$r2CU <- r2CU

  if (!is.null(object)) {
    the_aov <- anova(objectNull, object, test = "Chisq")

    out$chisq <- the_aov$Deviance[2]
    out$chisq_df <- the_aov$Df[2]
    out$chisq_p <- the_aov$`Pr(>Chi)`[2]
  }

  out
}

pR2 <- function(object) {

  llh <- getLL(object)

  if (family(object)$family %in% c("quasibinomial","quasipoisson")) {
    msg_wrap("Note: Pseudo-R2 for quasibinomial/quasipoisson families is
             calculated by refitting the fitted and null models as
             binomial/poisson.")
  }

  frame <- model.frame(object)

  .weights <- model.weights(frame)
  .offset <- model.offset(frame)

  dv <- names(frame)[1]
  form <- as.formula(paste(paste0("`", dv, "`", "~ 1")))
  objectNull <- j_update(object, formula = form, weights = .weights,
                         offset = .offset, data = frame)

  llhNull <- getLL(objectNull)
  n <- dim(object$model)[1]
  pR2Work(llh, llhNull, n, object, objectNull)

}

# Enabling support for quasi families
#' @importFrom stats poisson binomial family
getLL <- function(object) {

  fam <- family(object)
  link <- fam$link
  fam <- fam$family
  quasis <- c("quasibinomial","quasipoisson","quasi")
  if (fam %nin% quasis) {
    return(logLik(object))
  } else {
    if (fam == "quasipoisson") {
      poisson_family <- poisson(link = link)
      logLik(j_update(object, family = poisson_family))
    } else if (fam == "quasibinomial") {
      binom_family <- binomial(link = link)
      logLik(j_update(object, family = binom_family))
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
