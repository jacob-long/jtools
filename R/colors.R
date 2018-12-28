#' @title Color palettes in `jtools` functions
#'
#' @description `jtools` combines several options into the `color.class`
#'   argument in plotting functions.
#'
#' @details
#'   The argument to `color.class` in functions like `effect_plot`,
#'   `plot_coefs`, and others is very flexible but may also
#'   cause confusion.
#'
#'   If you provide an argument of length 1, it is assumed that you are naming
#'   a palette. `jtools` provides 6 color palettes design for qualitative data.
#'   4 of the 6 are based on Paul Tol's suggestions (see references) and are
#'   meant to both optimize your ability to quickly differentiate the colors
#'   and to be distinguishable to colorblind people.
#'   These are called
#'   `"Qual1"`, `"Qual2"`, `"Qual3"`, `"CUD"`, `"CUD Bright"`, and `"Rainbow"`.
#'   Each of the "Qual" schemes comes from Paul Tol.
#'   "Rainbow" is Paul Tol's compromise rainbow color scheme that is fairly
#'   differentiable for colorblind people and when rendered in grayscale.
#'   `"CUD Bright"` is a brightened and reordered version of Okabe and Ito's
#'   suggestions for 'Color Universal Design' while `"CUD"` is their exact
#'   scheme (see references). `"CUD Bright"` is the default for qualitative
#'   scales in `jtools` functions.
#'
#'   You may also provide any color palette supported by `RColorBrewer`.
#'   See all of those options at [RColorBrewer::brewer.pal()]'s documentation.
#'   If you provide one of `RColorBrewer`'s sequential palettes, like "Blues",
#'   `jtools` automatically requests one more color than needed from
#'   `brewer.pal` and then drops the lightest color. My experience is that
#'   those scales tend to give one color that is too light to easily
#'   differentiate against a white background.
#'   
#'   For **gradients**, you can use any of the `RColorBrewer` sequential palette
#'   names and get comparable results on a continuous scale. There are also some
#'   `jtools`-specific gradient schemes: `"blue"`, `"blue2"`, `"green"`, 
#'   `"red"`, `"purple"`, `"seagreen"`. 
#'   If you want something a little non-standard, I'd suggest taking a look 
#'   at `"blue2"` or `"seagreen"`.
#'
#'   Lastly, you may provide colors by name. This must be a vector of the
#'   same length as whatever it is the colors will correspond to. The format
#'   must be one understood by `ggplot2`'s manual scale functions. This
#'   basically means it needs to be in hex format (e.g., "#000000") or
#'   one of the many names R understands (e.g., "red"; use `colors()` to
#'   see all of those options).
#'
#' @references
#'
#' Paul Tol's site is what is used to derive 4 of the 6 `jtools`-specific
#' qualitative palettes: \url{https://personal.sron.nl/~pault/}
#'
#' Okabe and Ito's palette inspired "CUD Bright", though "CUD Bright" is not
#' exactly the same. "CUD" is the same.
#' See \url{http://jfly.iam.u-tokyo.ac.jp/color/} for more.
#'
#' @rdname jtools_colors
#' @name jtools_colors
NULL

#' @title Get colors for plotting functions
#' @description This is a helper function that provides hex color codes for 
#'  `jtools`, `interactions`, and perhaps other packages.
#' @param color.class The name of the desired color class or a vector of colors.
#'  See details of [jtools_colors].
#' @param num_colors How many colors should be returned? Default is 1.
#' @param reverse Should the colors be returned in reverse order, compared to
#'  normal? Default is FALSE.
#' @param gradient Return endpoints for a gradient? Default is FALSE. If TRUE,
#'  `num_colors` is ignored.
#' @importFrom grDevices rgb
#' @export
get_colors <- function(color.class, num_colors = 1, reverse = FALSE,
                       gradient = FALSE) {

  # Save all valid color brewer names
  ## Sequential palettes get different treatment
  sequentials <-
    c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
      "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
      "YlOrBr", "YlOrRd")
  div_or_qual <-
    c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
      "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
      "Set2", "Set3")
  brewers <- c(sequentials, div_or_qual)
  viridis <- c("viridis", "magma", "A", "inferno", "B", "plasma", "C",
               "D", "cividis", "E")

  if (length(color.class) == 1 && color.class %in% c(brewers, viridis) &
      gradient == FALSE) {

    if (color.class %in% sequentials) {
      colors <- RColorBrewer::brewer.pal(num_colors + 1, color.class)[-1]
      colors <- colors[seq_len(num_colors)]
      if (reverse == TRUE) {colors <- rev(colors)}
      return(colors)
    } else if (color.class %in% viridis) {
      colors <- rev(scales::viridis_pal(option = color.class)(num_colors))
      if (reverse == TRUE) {colors <- rev(colors)}
      return(colors)
    } else {
      colors <- RColorBrewer::brewer.pal(num_colors, color.class)
      if (reverse == TRUE) {colors <- rev(colors)}
      return(colors)
    }
    
  } else if (length(color.class) == 1 && color.class %in% viridis) {
    colors <- rev(scales::viridis_pal(option = color.class)(num_colors))
    if (reverse == TRUE) {colors <- rev(colors)}
    return(colors)
  }

  mine <- c("CUD", "CUD Bright", "Rainbow", "Qual1", "Qual3")
  if (length(color.class) == 1 & color.class %in% mine) {

    cb14 <- c("#4477AA", "#EE6677", "#228833", "#CC8844")
    cb16 <- c("#4477AA", "#66CCEE", "#228833", "#CC8844", "#EE6677",
              "#AA3377")

    cb22 <- c("#4477AA", "#CC6677")
    cb21 <- cb22[1]
    cb23 <- c("#4477AA", "#DDCC77", "#CC6677")
    cb24 <- c("#4477AA", "#117733", "#DDCC77", "#CC6677")
    cb25 <- c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
    cb26 <- c("#332288", "#88CCEE", "#117733", "#DDCC77",
              "#CC6677", "#AA4499")
    cb27 <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77",
              "#CC6677", "#AA4499")
    cb28 <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77",
              "#CC6677")
    cb29 <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
              "#DDCC77", "#CC6677")
    cb2 <- list(cb21, cb22, cb23, cb24, cb25, cb26, cb27, cb28, cb29)

    cb3 <- c("#3366AA", "#EE3333", "#11AA99", "#992288", "#66AA55",
             "#EE7722", "#CCCC55", "#FFEE33")

    cud <- c("#D55E00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#E69F00",
             "#F0E442", "#999999")

    # cudb2 <- c("#ff3200", "#5ec1ff", "#009E73", "#fc82be", "#0072B2", "#ffa00a",
    #          "#F0E442", "#999999")

    cudb <- c("#49b7fc", "#ff7b00", "#17d898", "#ff0083", "#0015ff", "#e5d200",
              "#999999")

    rainbow_col <- function(x) {
      r_num <- .472 - .567*x + 4.05*(x^2)
      r_den <- 1 + 8.72*x - 19.17*(x^2) + 14.1*(x^3)
      r <- r_num/r_den

      g <- .108932 - 1.22635*x + 27.284*(x^2) - 98.577*(x^3) +
        163.3*(x^4) - 131.395*(x^5) + 40.634*(x^6)

      b <- 1.97 + 3.54*x - 68.5*(x^2) + 243*(x^3) - 297*(x^4) + 125*(x^5)
      b <- 1/b

      rgb <- c("r" = r, "g" = g, "b" = b)
      return(rgb)

    }

    rainbow_cols <- function(y) {
      y <- seq(from = 0, to = 1, length = y)
      rgbs <- lapply(y, rainbow_col)
      hexs <- sapply(rgbs, function(x) {
        rgb(x[1], x[2], x[3])
      })
      return(hexs)
    }

    if (color.class == "Qual2") {

      if (num_colors <= 4) {
        colors <- cb14[seq_len(num_colors)]
      } else if (num_colors %in% c(5, 6)) {
        colors <- cb16[seq_len(num_colors)]
      } else {
        stop_wrap("Only 6 or fewer colors supported with Qual1 color.class.")
      }

    } else if (color.class == "Qual3") {

      colors <- unlist(cb2[num_colors])

    } else if (color.class == "Qual1") {

      colors <- cb3[seq_len(num_colors)]

    } else if (color.class == "CUD Bright") {

      colors <- cudb[seq_len(num_colors)]

    } else if (color.class == "CUD") {

      colors <- cud[seq_len(num_colors)]

    } else if (color.class == "Rainbow") {

      colors <- rainbow_cols(num_colors)

    } else if (num_colors == 1) {

      return(color.class)

    } else {
      stop_wrap("color.class not found.")
    }

    if (reverse == TRUE) {colors <- rev(colors)}
    return(colors)

  }
  
  if (gradient == TRUE) {
    if (length(color.class) == 1) {
      if (color.class == "Blues") {
        colors <- c("#C6DBEF", "#08519C")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "Oranges") {
        colors <- c("#FDD0A2", "#7F2704")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "Greens") {
        colors <- c("#C7E9C0", "#00441B")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "Reds") {
        colors <- c("#FCBBA1", "#67000D")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "Purples") {
        colors <- c("#DADAEB", "#3F007D")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "Greys") {
        colors <- c("#D9D9D9", "#000000")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "blue") {
        # colors <- c("#99bbff", "#003399")
        colors <- c("#9ed2fa", "#06477a")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "green") {
        colors <- c("#33ff33", "#004d00")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "red") {
        colors <- c("#ff9999", "#800000")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "purple") {
        colors <- c("#cc99ff", "#330066")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "seagreen") {
        colors <- c("#85e0e0", "#004d4d")
        if (reverse) {rev(colors)}
        return(colors)
      } else if (color.class == "blue2") {
        colors <- c("#C2CEFA", "#33559B")
        if (reverse) {rev(colors)}
        return(colors)
      } else {
        colors <- RColorBrewer::brewer.pal(num_colors, color.class)
        if (reverse) {rev(colors)}
        return(colors)
      }
    }
  }

  if (length(color.class) >= num_colors) {
    colors <- color.class[seq_len(num_colors)]
    if (reverse == TRUE) {colors <- rev(colors)}
    return(colors)
  } else if (length(color.class) == 2 && gradient == TRUE) {
    colors <- color.class
    if (reverse == TRUE) {colors <- rev(colors)}
    return(colors)
  } else {
    stop_wrap("Provided colors vector is not as long as the number of colors
              needed.")
  }

}
