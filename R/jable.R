jable <- function (x, format, digits = getOption("digits"), row.names = NA, 
          col.names = NA, align, caption = NULL, format.args = list(), 
          escape = TRUE, ...)  {
  
  if (format != "latex" && !missing(align) && length(align) == 1L) { 
    align = strsplit(align, "")[[1]]
  }
  if (!is.matrix(x)) 
    x = as.data.frame(x)
  if (identical(col.names, NA)) 
    col.names = colnames(x)
  m = ncol(x)
  isn = if (is.matrix(x)) 
    rep(is.numeric(x), m)
  else sapply(x, is.numeric)
  if (missing(align) || (format == "latex" && is.null(align))) 
    align = ifelse(isn, "r", "l")
  digits = rep(digits, length.out = m)
  for (j in seq_len(m)) {
    if (knitr:::is_numeric(x[, j])) 
      x[, j] = round(x[, j], digits[j])
  }
  if (any(isn)) {
    if (is.matrix(x)) {
      if (is.table(x) && length(dim(x)) == 2) 
        class(x) = "matrix"
      x = format_matrix(x, format.args)
    }
    else x[, isn] = format_args(x[, isn], format.args)
  }
  if (is.na(row.names)) 
    row.names = has_rownames(x)
  if (!is.null(align)) 
    align = rep(align, length.out = m)
  if (row.names) {
    x = cbind(` ` = rownames(x), x)
    if (!is.null(col.names)) 
      col.names = c(" ", col.names)
    if (!is.null(align)) 
      align = c("l", align)
  }
  n = nrow(x)
  x = replace_na(to_character(as.matrix(x)), is.na(x))
  if (!is.matrix(x)) 
    x = matrix(x, nrow = n)
  x = trimws(x)
  colnames(x) = col.names
  if (format != "latex" && length(align) && !all(align %in% 
                                                 c("l", "r", "c"))) 
    stop("'align' must be a character vector of possible values 'l', 'r', and 'c'")
  attr(x, "align") = align
  apply(x, c(1,2), paste, "\u00A0") # pad to the right for right alignment
  res = do.call(paste("jable", format, sep = "_"),
                list(x = x, caption = caption, escape = escape, ...))
  # structure(res, format = format, class = "knitr_kable")
}

jable_markdown <- function (x, padding = 1, ...){
  if (is.null(colnames(x))) {
    warning("The table should have a header (column names)")
    colnames(x) = rep("", ncol(x))
  }
  res = jable_mark(x, c(NA, "-", NA), "|", padding, align.fun = function(s, 
                                                                         a) {
    if (is.null(a)) 
      return(s)
    r = c(l = "^.", c = "^.|.$", r = ".$")
    for (i in seq_along(s)) {
      s[i] = gsub(r[a[i]], ":", s[i])
    }
    s
  }, ...)
  sprintf("|%s|", res)
}

jable_mark <- function(x, sep.row = c("=", "=", "="), sep.col = "  ", padding = 0, 
          align.fun = function(s, a) s, rownames.name = "", ...) {
  if (sep.col == "|") 
    for (j in seq_len(ncol(x))) {
      x[, j] = gsub("\\|", "&#124;", x[, j])
    }
  l = if (prod(dim(x)) > 0) 
    apply(x, 2, function(z) max(nchar(z, type = "width"), 
                                na.rm = TRUE))
  cn = colnames(x)
  if (length(cn) > 0) {
    cn[is.na(cn)] = "NA"
    if (sep.col == "|") 
      cn = gsub("\\|", "&#124;", cn)
    if (grepl("^\\s*$", cn[1L])) 
      cn[1L] = rownames.name
    l = pmax(if (length(l) == 0) 
      0
      else l, nchar(cn, type = "width"))
  }
  align = attr(x, "align")
  padding = padding * if (length(align) == 0) {
    2
  } else {
    ifelse(align == "c", 2, 1)
  }
  l = pmax(l + padding, 3)
  s = unlist(lapply(l, function(i) paste(rep(sep.row[2], i), 
                                         collapse = "")))
  res = rbind(if (!is.na(sep.row[1])) 
    s, cn, align.fun(s, align), x, if (!is.na(sep.row[3])) 
      s)
  apply(mat_pad(res, l, align), 1, paste, collapse = sep.col)
}

jable_pandoc <- function(x, caption = NULL, padding = 1, ...) {
  tab <- if (ncol(x) == 1) {
    jable_markdown(x, padding = padding, ...)
  } else {
    jable_mark(x, c(NA, "-", if (knitr:::is_blank(colnames(x))) "-" else NA), 
                  padding = padding, ...)
  }
  tab
}

has_rownames <- function(x) {
  !is.null(rownames(x)) && !identical(rownames(x), as.character(seq_len(NROW(x))))
}

mat_pad <- function(m, width, align = NULL) {
  n = nrow(m)
  p = ncol(m)
  res = matrix("", nrow = n, ncol = p)
  if (n * p == 0) 
    return(res)
  stopifnot(p == length(width))
  side = rep("both", p)
  if (!is.null(align)) 
    side = c(l = "right", c = "both", r = "left")[align]
  apply(m, 2, function(x) max(nchar(x, "width") - nchar(x, 
                                                        "chars")))
  matrix(pad_width(c(m), rep(width, each = n), rep(side, each = n)), 
         ncol = p)
}

pad_width <- function(x, width, side) {
  if (!all(side %in% c("left", "right", "both"))) 
    stop("'side' must be 'left', 'right', or 'both'")
  w = width - nchar(x, "width")
  w1 = floor(w/2)
  s1 = v_spaces(w * (side == "left") + w1 * (side == "both"))
  s2 = v_spaces(w * (side == "right") + (w - w1) * (side == 
                                                      "both"))
  paste0(s1, x, s2)
}

replace_na <- function(x, which = is.na(x), to = getOption("knitr.kable.NA")) {
  if (is.null(to)) 
    return(x)
  x[which] = to
  x
}

to_character <- function(x) {
  if (is.character(x)) 
    return(x)
  x2 = as.character(x)
  dim(x2) = dim(x)
  dimnames(x2) = dimnames(x)
  x2
}

is_blank <- function(x) {
  if (length(x)) 
    all(grepl("^\\s*$", x))
  else TRUE
}

spaces <- function(n = 1, char = " ") {
  if (n <= 0) 
    return("")
  if (n == 1) 
    return(char)
  paste(rep(char, n), collapse = "")
}

v_spaces <- function(n) {
  unlist(lapply(n, spaces))
}

format_args <- function (x, args = list()) {
  args$x = x
  args$trim = TRUE
  replace_na(do.call(format, args), is.na(x))
}