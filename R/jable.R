
#' @title Print attractive data frames in the console 
#' @description This function takes data frame input and prints to the console
#'   as an ASCII/markdown table for better readability.
#' @param x A data frame or matrix.
#' @param format The style, which can be one of the following: "multiline", 
#'  "grid", "simple" (also "pandoc"), "rmarkdown" (also "markdown").
#'  Default: "markdown"
#' @param digits How many digits to print for numbers.
#'  Default: 2
#' @param sig.digits Should each number be printed with `digits` number of 
#'  digits or only when there are at least that many significant digits? Default
#'  is TRUE, meaning only print `digits` number of *significant* digits.
#' @param align Column alignment: a character vector consisting of ‘'l'’
#'  (left), ‘'c'’ (center) and/or ‘'r'’ (right). By default or if
#'  ‘align = NULL’, numeric columns are right-aligned, and other
#'  columns are left-aligned. 
#' @param row.names if FALSE, row names are suppressed. A character vector of
#'  row names can also be specified here. By default, row names are included if 
#'  rownames(t) is neither NULL nor identical to 1:nrow(x).
#' @param col.names a character vector of column names to be used in the table
#' @inheritParams knitr::kable
#' @importFrom pander pandoc.table.return
#' @rdname md_table
#' @export 

md_table <- function(x, format = getOption("md_table_format", "grid"),
                     digits = getOption("jtools-digits", 2), sig.digits = TRUE,
                     row.names = rownames(x), col.names = colnames(x),
                     align = NULL) {
  if (is.null(align)) {
    align <- sapply(1:ncol(x), function(y) {is.numeric(x[,y])})
    align <- ifelse(align, yes = "right", no = "left")
    if (!is.null(row.names) && row.names != 1:length(row.names)) {
      align <- c("left", align)
    }
  } else {
    align <- switch(align, 
                    "l" = "left",
                    "c" = "center", 
                    "r" = "right",
                    align)
  }
  if (sig.digits == FALSE) {
    x <- round_df_char(x, digits = digits)
  }
  format <- switch(format, 
                   "markdown" = "rmarkdown",
                   "pandoc" = "simple",
                   format)
  out <- pandoc.table.return(x, style = format, row.names = row.names, 
                             col.names = col.names, format.args = format.args,
                             justify = align, plain.ascii = TRUE,
                             split.tables = getOption("width", 80),
                             missing = "")
  out <- gsub("Table: Table continues below", "", out, fixed = TRUE)
  out <- gsub("\n\n*", "\n", out)
  out <- gsub("\n$", "", out)
  out <- gsub("^\n", "", out)
  class(out) <- "md_table"
  return(out)
}

#' @export
print.md_table <- function(x, ...) {
  cat(x, sep = "\n")
}

