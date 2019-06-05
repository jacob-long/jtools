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
  # Create new environment
  e <- new.env()
  # Add everything from the model's data to this environment
  lapply(names(frame), function(x, env, f) {env[[x]] <- f[[x]]}, env = e,
         f = frame)
  # Add the offset to the environment
  e$`.offset` <- .offset
  # Add the weights to the environment
  e$`.weights` <- .weights
  # Add the environment to the formula
  environment(form) <- e
  
  # Get the model's original call
  call <- getCall(object)
  # Replace that call's formula with this new one that includes the modified
  # environment. Then set the `data` arg of the call to NULL so it looks only
  # in the new, modified environment
  call$formula <- form
  call$data <- NULL
  # Conditionally add the names of the offset and weights args
  if (!is.null(.offset)) {
    call$offset <- quote(.offset)
  }
  if (!is.null(.weights)) {
    call$weights <- quote(.weights)
  }
  if ("start" %in% names(call)) {
    call$start <- NULL
  } 
  # Update the model
  objectNull <- try(eval(call))
  if ("try-error" %in% class(objectNull)) {
    out <- as.list(rep(NA, 9))
    names(out) <- c("llh", "llhNull", "G2", "McFadden", "r2ML", "r2CU", 
                    "chisq", "chisq_df", "chisq_p")
    warn_wrap("Something went wrong when calculating the pseudo R-squared. 
              Returning NA instead.")
    return(out)
  }
  
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
