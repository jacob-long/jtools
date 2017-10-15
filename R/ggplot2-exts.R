#' @export
#' @rdname ggplot2-exts
#' @import ggplot2
draw_key_vpath_h <- function(data, params, size) {
  grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = grid::gpar(
                       col = alpha(data$colour, data$alpha),
                       lwd = data$size * .pt,
                       lty = data$linetype,
                       lineend = "butt"
                     ),
                     arrow = params$arrow
  )
}

#' @export
#' @rdname ggplot2-exts
#' @import ggplot2

draw_key_pointrange_h <- function(data, params, size) {
  grid::grobTree(
    draw_key_vpath_h(data, params, size),
    draw_key_point(transform(data, size = data$size * 4), params)
  )
}

#' @export
#' @rdname ggplot2-exts

geom_pointrange_h <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", ..., fatten = 4,
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointrangeh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fatten = fatten,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-exts
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
GeomPointrangeh <-
  ggplot2::ggproto("GeomPointrangeh", ggplot2::Geom,
    default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1,
                               shape = 19, fill = NA, alpha = NA, stroke = 1),
    draw_key = draw_key_pointrange_h, required_aes = c("x", "y", "xmin", "xmax"),
    draw_panel = function(data, panel_scales, coord, fatten = 4) {
     if (is.null(data$y)) {
      return(GeomLinerange$draw_panel(data, panel_scales, coord))
     }
     ggname("geom_pointrange_h",
       grid::gTree(children =
          grid::gList(GeomLinerangeh$draw_panel(data, panel_scales, coord),
       GeomPoint$draw_panel(transform(data, size = size * fatten),
                            panel_scales, coord))))
    }
  )

#' @title ggplot2 extensions
#'
#' @description These are lightly documented tweaks to ggplot2 geoms for
#'  jtools functions.
#'
#' @param mapping Set of aesthetic mappings created by [aes()] or
#'   [aes_()]. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this
#'    layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param fatten A multiplicative factor used to increase the size of the
#'   middle bar in \code{geom_crossbar()} and the middle point in
#'   \code{geom_pointrange()}.
#' @param params Additional parameters to the `geom` and `stat`.
#' @param size size.
#' @param height Dodging height, when different to the height of the individual
#'   elements. This is useful when you want to align narrow geoms with taller
#'   geoms.
#' @param ... other arguments passed on to layer. These are often aesthetics,
#'  used to set an aesthetic to a fixed value, like color = "red" or size = 3.
#'  They may also be parameters to the paired geom/stat.
#'
#' @export
#' @rdname ggplot2-exts

geom_linerange_h <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLinerangeh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-exts
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
GeomLinerangeh <-
  ggplot2::ggproto("GeomLinerangeh", ggplot2::Geom,
                   default_aes = ggplot2::aes(colour = "black", size = 0.5,
                                              linetype = 1,
                                              alpha = NA),
                   draw_key = draw_key_vpath_h, required_aes = c("y", "xmin",
                                                                 "xmax"),
                   draw_panel = function(data, panel_scales, coord) {
                     data <- transform(data, yend = y, x = xmin, xend = xmax)
                     ggname("geom_linerange_h",
                            GeomSegment$draw_panel(data, panel_scales, coord))
                   }
  )

## ggplot2 uses this, so I have to
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#### from ggstance ############################################################

#' @rdname ggplot2-exts
#' @export
position_dodgev <- function(height = NULL) {
  ggplot2::ggproto(NULL, PositionDodgev, height = height)
}

#' @rdname ggplot2-exts
#' @format NULL
#' @usage NULL
#' @export
PositionDodgev <-
  ggplot2::ggproto("PositionDodgev", ggplot2::Position, required_aes = "y",
                   height = NULL, setup_params = function(self, data) {
     if (is.null(data$ymin) && is.null(data$ymax) && is.null(self$height)) {
       warning("Height not defined. Set with `position_dodge(height = ?)`",
               call. = FALSE)
       }
       list(height = self$height)
     },
     compute_panel = function(data, params, scales) {
       collidev(data, params$height, "position_dodgev", pos_dodgev,
                check.height = FALSE)
     }
)

pos_dodgev <- function(df, height) {
  n <- length(unique(df$group))
  if (n == 1) return(df)

  if (!all(c("ymin", "ymax") %in% names(df))) {
    df$ymin <- df$y
    df$ymax <- df$y
  }

  d_height <- max(df$ymax - df$ymin)

  # df <- data.frame(n = c(2:5, 10, 26), div = c(4, 3, 2.666666,  2.5, 2.2, 2.1))
  # ggplot(df, aes(n, div)) + geom_point()

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate ymin and lmax
  df$y <- df$y + height * ((groupidx - 0.5) / n - .5)
  df$ymin <- df$y - d_height / n / 2
  df$ymax <- df$y + d_height / n / 2

  df
}

collidev <- function(data, height = NULL, name, strategy, ...,
                     check.height = TRUE, reverse = FALSE) {
  # Determine height
  if (!is.null(height)) {
    # Width set manually
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y - height / 2
      data$ymax <- data$y + height / 2
    }
  } else {
    if (!(all(c("ymin", "ymax") %in% names(data)))) {
      data$ymin <- data$y
      data$ymax <- data$y
    }

    # Width determined from data, must be floating point constant
    heights <- unique(data$ymax - data$ymin)
    heights <- heights[!is.na(heights)]

    #   # Suppress warning message since it's not reliable
    #     if (!zero_range(range(heights))) {
    #       warning(name, " requires constant height: output may be incorrect",
    #         call. = FALSE)
    #     }
    height <- heights[1]
  }

  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  if (reverse) {
    data <- data[order(data$ymin, data$group), ]
  } else {
    data <- data[order(data$ymin, -data$group), ]
  }


  # Check for overlap
  intervals <- as.numeric(t(unique(data[c("ymin", "ymax")])))
  intervals <- intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning(name, " requires non-overlapping y intervals", call. = FALSE)
    # This is where the algorithm from [L. Wilkinson. Dot plots.
    # The American Statistician, 1999.] should be used
  }

  if (!is.null(data$xmax)) {
    plyr::ddply(data, "ymin", strategy, ..., height = height)
  } else if (!is.null(data$x)) {
    data$xmax <- data$x
    data <- plyr::ddply(data, "ymin", strategy, ..., height = height)
    data$x <- data$xmax
    data
  } else {
    stop("Neither x nor xmax defined")
  }
}
