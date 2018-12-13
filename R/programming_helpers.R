## Making a "opposite of %in%" or "not %in%" function to simplify code
#' @title Not `%in%``
#' @description This function does the very opposite of `%in%`
#' @param x An object
#' @param table The object you want see if `x` is not in
#' @return A logical vector
#' @rdname nin
#' @export 
`%nin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))
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
  formatC(x, digits = digits, format = "f")
}

# Automate the addition of newline characters for long strings
#' @title `cat`, `message`, `warning`, and `stop` wrapped to fit the console's
#'  width.
#' @description These are convenience functions that format printed output to
#'  fit the width of the user's console.
#' @param ... Objects to print
#' @param sep Separator between `...`, Default: ''
#' @rdname wrap_str
#' @export 

wrap_str <- function(..., sep = "") {
  paste0(strwrap(paste(..., sep = sep), width = 0.95 * getOption("width", 80)),
         collapse = "\n")
}

# Go ahead and wrap the cat function too
#' @rdname wrap_str
#' @export 
cat_wrap <- function(..., brk = "") {
  cat(wrap_str(...), brk, sep = "")
}

# Define orange crayon output
orange <- crayon::make_style("orange")

# Like cat_wrap but for warnings
#' @rdname wrap_str
#' @export 
warn_wrap <- function(..., brk = "\n", call. = FALSE) {
  warning(orange(wrap_str(...)), brk, call. = FALSE)
}

# Like cat_wrap but for errors
#' @rdname wrap_str
#' @export 
stop_wrap <- function(...,  brk = "\n", call. = FALSE) {
  stop(red(wrap_str(...)), brk, call. = FALSE)
}

# Like cat_wrap but for messages
#' @importFrom crayon cyan
#' @rdname wrap_str
#' @export 
msg_wrap <- function(..., brk = "\n") {
  message(cyan(wrap_str(...)), brk)
}
