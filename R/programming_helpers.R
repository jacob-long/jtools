## Making a "opposite of %in%" or "not %in%" function to simplify code
#' @title Not `%in%`
#' @description This function does the very opposite of `%in%`
#' @param x An object
#' @param table The object you want see if `x` is not in
#' @return A logical vector
#' @rdname nin
#' @family subsetters
#' @export 
`%nin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))

#' @export
#' @rdname subsetters
#' @family subsetters
`%not%` <- function(x, y) {
  UseMethod("%not%")
}

#' @export
#' @rdname subsetters
#' @family subsetters
#' @usage x \%not\% y <- value
`%not%<-` <- function(x, y, value) {
  UseMethod("%not%<-")
}

#' @export
#' @rdname subsetters
#' @family subsetters
`%just%` <- function(x, y) {
  UseMethod("%just%")
}

#' @export
#' @rdname subsetters
#' @family subsetters
#' @usage x \%just\% y <- value
`%just%<-` <- function(x, y, value) {
  UseMethod("%just%<-")
}


# Automates my most common use of %nin%
#' @title Subsetting operators
#' @description `%just%` and `%not%` are subsetting convenience functions 
#'  for situations when you would do `x[x %in% y]` or `x[x %nin% y]`. See
#'  details for behavior when `x` is a data frame or matrix.
#' @param x Object to subset
#' @param y List of items to include if they are/aren't in `x`
#' @param value The object(s) to assign to the subsetted `x`
#' @details 
#'  The behavior of `%not%` and `%just%` are different when you're subsetting 
#'  data frames or matrices. The subset `y` in this case is interpreted as
#'  **column** names or indices. 
#'
#'  You can also make assignments to the subset in the same way you could if
#'  subsetting with brackets. 
#'
#' @return All of `x` that are in `y` (`%just%`) or all of `x` that are not in
#'  `y` (`%not%`).
#' @examples 
#' 
#'  x <- 1:5
#'  y <- 3:8
#'  
#'  x %just% y # 3 4 5
#'  x %not% y # 1 2
#'
#'  # Assignment works too
#'  x %just% y <- NA # 1 2 NA NA NA
#'  x %not% y <- NA # NA NA 3 4 5
#'  
#'  mtcars %just% c("mpg", "qsec", "cyl") # keeps only columns with those names
#'  mtcars %not% 1:5 # drops columns 1 through 5
#'
#'  # Assignment works for data frames as well
#'  mtcars %just% c("mpg", "qsec") <- gscale(mtcars, c("mpg", "qsec"))
#'  mtcars %not% c("mpg", "qsec") <- gscale(mtcars %not% c("mpg", "qsec"))
#'  
#'  
#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%.default` <- function(x, y) {
  x[x %nin% y]
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%<-.default` <- function(x, y, value) {
  x[x %nin% y] <- value
  x
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%.data.frame` <- function(x, y) {
  if (is.character(y)) {
    x[names(x) %not% y]
  } else {
    x[seq_along(x) %not% y]
  }
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%<-.data.frame` <- function(x, y, value) {
  if (is.character(y)) {
    x[names(x) %not% y] <- value
  } else {
    x[seq_along(x) %not% y] <- value
  }
  x
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%.matrix` <- function(x, y) {
  if (is.character(y)) {
    x[, colnames(x) %not% y]
  } else {
    x[, seq_len(ncol(x)) %not% y]
  }
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%<-.matrix` <- function(x, y, value) {
  if (is.character(y)) {
    x[, colnames(x) %not% y] <- value
  } else {
    x[, seq_len(ncol(x)) %not% y] <- value
  }
  x
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%.list` <- function(x, y) {
  if (is.character(y)) {
    y <- which(names(x) %in% y)
  }
  `%not%.data.frame`(x, y)
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%not%<-.list` <- function(x, y, value) {
  if (is.character(y)) {
    y <- which(names(x) %in% y)
  }
  `%not%<-.data.frame`(x, y, value)
}

#' @rdname subsetters
#' @family subsetters
#' @export
# Automates my most common use of %in%
`%just%.default` <- function(x, y) {
  x[unlist(lapply(y, function(y, x) which(x == y), x = x))]
}

#' @rdname subsetters
#' @family subsetters
#' @export
# Automates my most common use of %in%
`%just%<-.default` <- function(x, y, value) {
  x[unlist(lapply(y, function(y, x) which(x == y), x = x))] <- value
  x
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%just%.data.frame` <- function(x, y) {
  if (is.character(y)) {
    x[y %just% names(x)]
  } else {
    x[y %just% seq_along(x)]
  }
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%just%<-.data.frame` <- function(x, y, value) {
  if (is.character(y)) {
    x[names(x) %just% y] <- value
  } else {
    x[seq_along(x) %just% y] <- value
  }
  x
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%just%.matrix` <- function(x, y) {
  if (is.character(y)) {
    x[, colnames(x) %just% y]
  } else {
    x[, seq_len(ncol(x)) %just% y]
  }
}

#' @rdname subsetters
#' @family subsetters
#' @export 

`%just%<-.matrix` <- function(x, y, value) {
  if (is.character(y)) {
    x[, colnames(x) %just% y] <- value
  } else {
    x[, seq_len(ncol(x)) %just% y] <- value
  }
  x
}

#' @rdname subsetters
#' @family subsetters
#' @export
# Automates my most common use of %in%
`%just%.list` <- function(x, y) {
  if (is.character(y)) {
    y <- which(names(x) %in% y)
  }
  out <- `%just%.data.frame`(x, y)
  out
}

#' @rdname subsetters
#' @family subsetters
#' @export
# Automates my most common use of %in%
`%just%<-.list` <- function(x, y, value) {
  if (is.character(y)) {
    y <- which(names(x) %in% y)
  }
  `%just%<-.data.frame`(x, y, value)
}


## Print rounded numbers with all requested digits, signed zeroes
#' @title Numbering printing with signed zeroes and trailing zeroes
#' @description This function will print exactly the amount of digits requested
#'  as well as signed zeroes when appropriate (e.g, `-0.00`).
#' @param x The number(s) to print
#' @param digits Number of digits past the decimal to print
#' @param format equal to `"d"` (for integers),
#'  `"f"`, `"e"`, `"E"`, `"g"`, `"G"`, `"fg"` (for reals). Default is `"f"`
#' @export
num_print <- function(x, digits = getOption("jtools-digits", 2),
                      format = "f") {
  sapply(x, function(x) {if (is.numeric(x)) {
    formatC(x, digits = digits, format = format)
  } else {
    x
  }}, USE.NAMES = FALSE)
}

#### wrap helpers ############################################################

# Let's only do colors if not in RStudio
is.rstudio = function() {
  .Platform$GUI == "RStudio"
}
# Dummy function to return whatever its given
give_back <- function(x) {
  return(x)
}
c_orange <- function(...) {
  if (!is.rstudio()) {
    orange <- crayon::make_style("orange") 
    orange(...)
  } else {
    give_back(...)
  }
}
c_red <- function(...) {
  if (!is.rstudio()) {
    crayon::red(...)
  } else {
    give_back(...)
  }
}
c_cyan <- function(...) {
  if (!is.rstudio()) {
    crayon::cyan(...)
  } else {
    give_back(...)
  }
}

dot_processor <- function(...) {
  dots <- list(...)
  out <- list(unnamed = NULL, named = NULL)
  if (!is.null(names(dots))) {
    out$unnamed <- dots %just% ""
    out$named <- dots %not% ""
    lapply(out, function(x) if (length(x) == 0) NULL else x)
  } else {
    out$unnamed <- dots
    out
  }
}

#  Automate the addition of newline characters for long strings
#' @title `cat`, `message`, `warning`, and `stop` wrapped to fit the console's
#'  width.
#' @description These are convenience functions that format printed output to
#'  fit the width of the user's console.
#' @param ... Objects to print. For `stop_wrap()`, `warn_wrap()`, and 
#'  `msg_wrap()`, any named objects are instead diverted to the `...` argument 
#'  of [rlang::abort()], [rlang::warn()], and [rlang::inform()],
#'  respectively.
#' @param sep Separator between `...`, Default: ''
#' @param brk What should the last character of the message/warning/error be?
#'  Default is `"\n"`, meaning the console output ends with a new line.
#' @param call. Here for legacy reasons. It is ignored.
#' @param call The actual calling environment to report in the error message.
#'  By default, `rlang::caller_env()`.
#' @inheritParams rlang::abort
#' @details 
#'  The point of these functions is to allow you to print
#'  output/messages/warnings/errors to the console without having to figure out
#'  where to place newline characters. These functions get the width of the
#'  console from the `"width"` option, which in most editors adjusts dynamically
#'  as you resize.
#'  
#'  So instead of writing a warning like this:
#'  
#'  ```
#'  warning("I have to give you this very important message that may be too\n",
#'          "wide for your screen")
#'  ```
#'  
#'  You can do it like this:
#'  
#'  ```
#'  warn_wrap("I have to give you this very important message that may be
#'            too wide for your screen")
#'  ```
#'  
#'  And the function will automatically insert line breaks to fit the console.
#'  As a note, it will also ignore any newlines you insert. This means you can
#'  make your own fit your editor's screen and indent in the middle of a string
#'  without that formatting being carried over into the output.
#'  
#'  
#' @rdname wrap_str
#' @export 

wrap_str <- function(..., sep = "") {
  # Using gsub to delete double spaces after periods when there was originally
  # a linebreak after the period.
  gsub("  ", " ",
       paste0(
         strwrap(paste(..., sep = sep), width = 0.95 * getOption("width", 80)),
         collapse = "\n"
       ),
       fixed = TRUE
      )
}

# Go ahead and wrap the cat function too
#' @rdname wrap_str
#' @export 
cat_wrap <- function(..., brk = "") {
  cat(wrap_str(...), brk, sep = "")
}

# Like cat_wrap but for warnings
#' @rdname wrap_str
#' @export 
warn_wrap <- function(..., brk = "\n", class = NULL, call. = FALSE) {
  dots <- dot_processor(...)
  wrapped <- c_orange(do.call(wrap_str, as.list(c(dots$unnamed, brk))))
  warn_args <- as.list(c(message = wrapped, class = class, dots$named))
  do.call(rlang::warn, warn_args)
}

# Like cat_wrap but for errors
#' @rdname wrap_str
#' @importFrom crayon red
#' @export 
stop_wrap <- function(...,  brk = "\n",
                      trace = rlang::trace_back(bottom = rlang::caller_env()),
                      class = NULL, call = rlang::caller_env()) {
  dots <- dot_processor(...)
  wrapped <- c_red(do.call(wrap_str, as.list(c(dots$unnamed, brk))))
  abort_args <- list(message = wrapped, class = class, dots$named,
                     trace = trace, call = call)
  abort_args <- abort_args[!sapply(abort_args, is.null)]
  do.call(rlang::abort, abort_args)
}

# Like cat_wrap but for messages
#' @importFrom crayon cyan
#' @rdname wrap_str
#' @export 
msg_wrap <- function(..., class = NULL, brk = "\n") {
  dots <- dot_processor(...)
  wrapped <- c_cyan(do.call(wrap_str, as.list(c(dots$unnamed, brk))))
  inform_args <- as.list(c(message = wrapped, class = class, dots$named))
  do.call(rlang::inform, inform_args)
}
