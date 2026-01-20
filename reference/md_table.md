# Print attractive data frames in the console

This function takes data frame input and prints to the console as an
ASCII/markdown table for better readability.

## Usage

``` r
md_table(
  x,
  format = getOption("md_table_format", "grid"),
  digits = getOption("jtools-digits", 2),
  sig.digits = TRUE,
  row.names = rownames(x),
  col.names = colnames(x),
  align = NULL
)
```

## Arguments

- x:

  A data frame or matrix.

- format:

  The style, which can be one of the following: "multiline", "grid",
  "simple" (also "pandoc"), "rmarkdown" (also "markdown"). Default:
  "markdown"

- digits:

  How many digits to print for numbers. Default: 2

- sig.digits:

  Should each number be printed with `digits` number of digits or only
  when there are at least that many significant digits? Default is TRUE,
  meaning only print `digits` number of *significant* digits.

- row.names:

  if FALSE, row names are suppressed. A character vector of row names
  can also be specified here. By default, row names are included if
  rownames(t) is neither NULL nor identical to 1:nrow(x).

- col.names:

  a character vector of column names to be used in the table

- align:

  Column alignment: a character vector consisting of ‘'l'’ (left), ‘'c'’
  (center) and/or ‘'r'’ (right). By default or if ‘align = NULL’,
  numeric columns are right-aligned, and other columns are left-aligned.
