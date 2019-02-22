#' @title Deprecated interaction functions
#' @description These functions are now part of the \pkg{interactions}
#'  package. 
#' @param ... arguments are ignored
#' @rdname interactions_deprecated
#' @rawNamespace
#' if (!nzchar(system.file(package = "interactions"))) {
#'   export(interact_plot)
#' }
interact_plot <- function(...) {
  if (!nzchar(system.file(package = "interactions"))) {
    stop_wrap("This function has been moved to the interactions package.
              Please download it and amend your R scripts as necessary.")
  } else {
    do.call(get("interact_plot", asNamespace("interactions")), 
            as.list(match.call(get("interact_plot",
                                   asNamespace("interactions"))))[-1])
  }
}

#' @rdname interactions_deprecated
#' @rawNamespace
#' if (!nzchar(system.file(package = "interactions"))) {
#'   export(cat_plot)
#' }
cat_plot <- function(...) {
  if (!nzchar(system.file(package = "interactions"))) {
    stop_wrap("This function has been moved to the interactions package.
              Please download it and amend your R scripts as necessary.")
  } else {
    do.call(get("cat_plot", asNamespace("interactions")), 
            as.list(match.call(get("cat_plot",
                                   asNamespace("interactions"))))[-1])
  }
}

#' @rdname interactions_deprecated
#' @rawNamespace
#' if (!nzchar(system.file(package = "interactions"))) {
#'   export(sim_slopes)
#' }
sim_slopes <- function(...) {
  if (!nzchar(system.file(package = "interactions"))) {
    stop_wrap("This function has been moved to the interactions package.
              Please download it and amend your R scripts as necessary.")
  } else {
    do.call(get("sim_slopes", asNamespace("interactions")), 
            as.list(match.call(get("sim_slopes",
                                   asNamespace("interactions"))))[-1])
  }
}
#' @rdname interactions_deprecated
#' @rawNamespace
#' if (!nzchar(system.file(package = "interactions"))) {
#'   export(johnson_neyman)
#' }
johnson_neyman <- function(...) {
  if (!nzchar(system.file(package = "interactions"))) {
    stop_wrap("This function has been moved to the interactions package.
              Please download it and amend your R scripts as necessary.")
  } else {
    do.call(get("johnson_neyman", asNamespace("interactions")), 
            as.list(match.call(get("johnson_neyman",
                                   asNamespace("interactions"))))[-1])
  }
}

#' @rdname interactions_deprecated
#' @rawNamespace
#' if (!nzchar(system.file(package = "interactions"))) {
#'   export(probe_interaction)
#' }
probe_interaction <- function(...) {
  if (!nzchar(system.file(package = "interactions"))) {
    stop_wrap("This function has been moved to the interactions package.
              Please download it and amend your R scripts as necessary.")
  } else {
    do.call(get("probe_interaction", asNamespace("interactions")), 
            as.list(match.call(get("probe_interaction",
                                   asNamespace("interactions"))))[-1])
  }
}