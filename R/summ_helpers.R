#### summ helpers ############################################################

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

#' @title Calculate robust standard errors and produce coefficient tables
#' @description This function wraps around several \pkg{sandwich}
#'  and \pkg{lmtest} functions to calculate robust standard errors and returns 
#'  them in a useful format. 
#' @param model A regression model, preferably of class `lm` or `glm`
#' @param type One of `"HC3"`, `"const"`, `"HC"`, `"HC0"`, `"HC1"`,
#'  `"HC2"`, `"HC4"`, `"HC4m"`, `"HC5"`. See [sandwich::vcovHC()] for some 
#'  more details on these choices. Note that some of these do not work for
#'  clustered standard errors (see sandwich::vcovCL()]). 
#' @param data The data used to fit the model. Default is to just get the 
#'  `model.frame` from `model`. 
#' @param cluster If you want clustered standard errors, either a string naming
#'  the column in `data` that represents the clusters or a vector of clusters
#'  that is the same length as the number of rows in `data`.
#' @param vcov You may provide the variance-covariance matrix yourself and this
#'  function will just calculate standard errors, etc. based on that. Default
#'  is NULL.
#' @return 
#' A list with the following:
#'
#'   * `coefs`: a coefficient table with the estimates, standard errors,
#'    t-statistics, and p-values from `lmtest`. 
#'   * `ses`: The standard errors from `coefs`.
#'   * `ts`: The t-statistics from `coefs`. 
#'   * `ps`: The p-values from `coefs`. 
#'   * `type`: The argument to `robust`.
#'   * `use_cluster`: `TRUE` or `FALSE` indicator of whether clusters were used.
#'   * `cluster`: The clusters or name of cluster variable used, if any.
#'   * `vcov`: The robust variance-covariance matrix.
#'   
#' @export
#' @rdname get_robust_se
get_robust_se <- function(model, type = "HC3", cluster = NULL, 
                          data = model.frame(model), vcov = NULL) {
  
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop_wrap("When using robust SEs you need to have the \'sandwich\' 
              package.", call. = FALSE)
  }
  
  if (type == TRUE) {
    type <- "HC3"
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
    
    if (!is.factor(cluster) && !is.numeric(cluster)) {
      
      warn_wrap("Invalid cluster input. Either use the name of the variable
                in the input data frame or provide a numeric/factor vector.
                Cluster is not being used in the reported SEs.")
      cluster <- NULL
      use_cluster <- FALSE
      
    } else {
      use_cluster <- TRUE   
    }
    
  } else {
    use_cluster <- FALSE
  }
  
  if (is.null(vcov) && type %in% c("HC4", "HC4m", "HC5") && is.null(cluster)) {
    # vcovCL only goes up to HC3
    coefs <- sandwich::vcovHC(model, type = type)
    
  } else if (is.null(vcov) && type %in% c("HC4", "HC4m", "HC5") &&
             !is.null(cluster)) {
    
    stop_wrap("If using cluster-robust SEs, robust.type must be HC3 or lower.")
    
  } else if (is.null(vcov)) {
    
    coefs <- sandwich::vcovCL(model, cluster = cluster, type = type)
    
  } else {coefs <- vcov}
  
  vcov <- coefs
  coefs <- test_coefs(model, coefs)
  ses <- coefs[,2]
  ts <- coefs[,3]
  ps <- coefs[,4]
  
  out <- list(coefs = coefs, ses = ses, ts = ts, ps = ps,
              use_cluster = use_cluster, type = type, cluster = cluster,
              vcov = vcov)
  class(out) <- "robust_info"
  return(out)
}

#' @export
print.robust_info <- function(x, ...) {
  print(md_table(x$coefs))
}

do_robust <- function(model, robust, cluster, data, vcov = NULL) {
  out <-
    get_robust_se(model = model, type = robust, cluster = cluster, data = data,
                  vcov = vcov)
  names(out)[names(out) == "type"] <- "robust"
  return(out)
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

which_columns <- function(which.cols, margins = FALSE,
                          confint, ci.labs, vifs, pvals, t.col,
                          exp = NULL, df = FALSE, others = NULL) {
  
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
    if (margins == TRUE) {
      cols <- c(cols, "A.M.E.")
    }
    cols <- c(cols, t.col)
    if (df == TRUE) {cols <- c(cols, "d.f.")}
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

## Checking for presence of deprecated arguments
dep_checks <- function(dots) {
  
  scale <-  transform.response <- robust <- exp <- NULL
  
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
    exp <- dots$odds.ratio
  }

  if ("model.check" %in% names(dots)) {
    warn_wrap("The model.check argument is deprecated and has been removed.",
              call. = FALSE)
  }
  
  if ("stars" %in% names(dots)) {
    warn_wrap("The 'stars' argument is deprecated and has been removed.")
  }
  
  list(scale = scale, transform.response = transform.response, robust = robust,
       exp = exp)
  
}

## Form the sentence that says what happened with variable transformations

scale_statement <- function(scale, center, transform.response, n.sd, 
                            scale.only = FALSE) {
  part_1 <- "Continuous "
  part_2 <- ifelse(transform.response, "variables", "predictors")
  part_3 <- if (!scale.only) {" are mean-centered"} else {NULL}
  part_4 <- if (scale && !scale.only) { 
    " and scaled by " 
  } else if (scale) {
    " are scaled by "
  } else { NULL }
  part_5 <- if (scale) { paste(n.sd, "s.d") } else { NULL }
  if (!transform.response) {
    part_5 <- 
      paste0(part_5, ". The outcome variable remains in its original units")
  }
  
  if (scale == FALSE && center == FALSE) {
    return(NULL)
  } else {
    paste0(part_1, part_2, part_3, part_4, part_5, ".")
  }
}

## Adapted from car::vif
vif <- function(mod, vcov = NULL, mod.matrix = NULL, ...) {
  
  if (any(is.na(coef(mod)))) {
    stop_wrap("VIFs cannot be calculated because there are aliased
              coefficients in the model.")
  }
  
  if (is.null(vcov)) {
    v <- vcov(mod)
  } else {v <- vcov}
  assign <- if (is.null(mod.matrix)) {
    attr(model.matrix(mod), "assign")
  } else {
    attr(mod.matrix, "assign")
  }
  
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

## Take model info and save as list

mod_info_list <- function(missing, n, dv, type) {
  out <- list(dv = dv, n = n, type = type)
  if (!is.null(missing) && missing != 0) {
    out$missing <- missing
  }
  return(out)
}

## Print model fit info

print_mod_fit <- function(stats) {
  cat(underline("MODEL FIT:"), "\n", sep = "")
  cat(stats, "\n\n")
}

## Print line about standard errors

print_se_info <- function(robust, use_cluster, manual = NULL, vcov = NULL, ...) {
  
  if (identical(FALSE, robust) && is.null(vcov)) {
    
    cat(italic("Standard errors:",  ifelse(is.null(manual),
                                           no = manual, yes = "MLE")),
        "\n", sep = "")
    
  } else if (is.null(vcov)) {
    
    if (robust == TRUE) {robust <- "HC3"}
    
    cat(italic("Standard errors:"), sep = "")
    
    if (use_cluster == FALSE) {
      
      cat(" Robust, ", italic("type = "), robust, "\n", sep = "")
      
    } else if (use_cluster == TRUE) {
      
      cat(" Cluster-robust, ", italic("type = "), robust, "\n", sep = "")
      
    }
    
  } else {
    "User-specified"
  }
  
}

## Save SE info as string

get_se_info <- function(robust, use_cluster, manual = NULL, vcov = NULL, ...) {
  
  if (identical(FALSE, robust) && is.null(vcov)) {
    
    ifelse(is.null(manual), no = manual, yes = "MLE")
    
  } else if (is.null(vcov)) {
    
    if (robust == TRUE) {robust <- "HC3"}
    
    if (use_cluster == FALSE) {
      
      paste0("Robust, type = ", robust)
      
    } else if (use_cluster == TRUE) {
      
      paste0("Cluster-robust, type = ", robust)
      
    }
    
  } else {
    "User-specified"
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

format_percentiles <- function(probs, digits) {
  paste0(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
         "%")
}

confint_linear <- function(object, parm, level = 0.95, cov = NULL,
                            df.residual = NULL, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- format_percentiles(a, 3)
  if (is.null(df.residual)) {
    fac <- qnorm(a)
  } else {
    fac <- qt(a, df.residual)
  }
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  if (is.null(vcov)) {
    ses <- sqrt(diag(vcov(object)))[parm]
  } else {
    ses <- sqrt(diag(cov))[parm]
  }
  ci[] <- cf[parm] + ses %o% fac
  ci
}

mod_rank <- function(model) {
  preds <- length(attr(terms(model), "order"))
  int <- attr(terms(model), "intercept")
  return(preds + int)
}

# hux_theme <- function(table, align_body = "right", caption = NULL,
#                       use_colnames = TRUE, width = .2,
#                       latex = knitr::is_latex_output()) {
#   # table <- huxtable::theme_striped(table, header_row = FALSE,
#   #                                  header_col = FALSE)
#   if (latex) { # don't shade odd rows if latex
#     table <- huxtable::set_background_color(table, huxtable::odds,
#                                           huxtable::everywhere,
#                                           grDevices::grey(0.9))
#   }

#   if (use_colnames == TRUE) {
#     table <- huxtable::add_colnames(table)
#     table <- huxtable::set_bold(table, row = 1, col = 1:ncol(table),
#                                 value = TRUE)
#     table <- huxtable::set_align(table, row = 1, col = 1:ncol(table), "center")
#     table <- huxtable::set_bottom_border(table, row = 1, col = 1:ncol(table), 1)
#   }

#   body_rows <- (1 + use_colnames):nrow(table)
#   table <- huxtable::set_bold(table, row = body_rows, col = 1, value = TRUE)
#   table <- huxtable::set_align(table, row = body_rows, col = 2:ncol(table),
#                                align_body)
#   table <- huxtable::set_align(table, row = body_rows, col = 1, "left")

#   if (!is.null(caption)) {
#     table <- huxtable::insert_row(table,
#                                   c(caption, rep(NA, times = ncol(table) - 1)),
#                                   after = 0)
#     table <- huxtable::set_colspan(table, row = 1, col = 1, ncol(table))
#     table <- huxtable::set_background_color(table, 1, huxtable::everywhere,
#                                             "white")
#     table <- huxtable::set_bold(table, row = 1, col = 1:ncol(table),
#                                 value = TRUE)
#     table <- huxtable::set_align(table, row = 1, col = 1:ncol(table), "center")
#     table <- huxtable::set_bottom_border(table, row = 1, col = 1:ncol(table), 1)
#   }

#   table <- huxtable::set_position(table,
#                                   ifelse(latex, no = "left", yes = "center"))
#   # table <- huxtable::set_width(table, width)

#   return(table)
# }

#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
to_kable <- function(t, html = !knitr::is_latex_output(), caption = NULL,
                     cols = NULL, footnote = NULL, row.names = FALSE,
                     col.names = NA, escape = knitr::is_latex_output(),
                     format = NULL) {
  
  # I've already converted numeric values to character, so I need to
  # tell kable to right-align them like it would do for numbers
  if (row.names == TRUE) {
    numerics <- names(t) %nin% ""
    align <- ifelse(numerics, yes = "r", no = "l")
  } else if (ncol(t) == 2) {
    align <- c("l", "r")
  } else {
    align <- NULL
  }
  
  # format <- ifelse(html, yes = "html", no = "latex")
  t %<>% knitr::kable(format = format, row.names = row.names,
                      col.names = col.names, escape = escape,
                      booktabs = TRUE, align = align)
  
  if (length(caption) > 0) { # I'm getting a character(0) object here sometimes
    head <- cols
    names(head) <- caption
    t %<>% kableExtra::add_header_above(header = head)
  }
  
  # center <- if (bold) {"left"} else {"center"}
  
  t %<>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center",
      latex_options = c("hold_position", "striped")
    )
  
  if (html == TRUE) {
    t %<>% kableExtra::column_spec(1, bold = TRUE)
  }
  
  if (length(footnote) > 0) {
    t %<>% kableExtra::footnote(general = footnote, 
                                general_title = "",
                                threeparttable = TRUE)
  }
  
  return(t)
  
}

kableExtra_latex_deps <- list(
  list(name = "booktabs"),
  list(name = "longtable"),
  list(name = "array"),
  list(name = "multirow"),
  list(name = "xcolor", options = "table"),
  list(name = "wrapfig"),
  list(name = "float"),
  list(name = "colortbl"),
  list(name = "pdflscape"),
  list(name = "tabu"),
  list(name = "threeparttable"),
  list(name = "threeparttablex"),
  list(name = "ulem", options = "ulem"),
  list(name = "makecell")
)

## Pandoc converts single asterisks to a black dot
escape_stars <- function(t) {
  if (any(colnames(t) == "")) {
    t[,which(colnames(t) == "")] <- 
      gsub("^\\*$", "\\\\*", t[,which(colnames(t) =="")])
  }
  return(t)
}
