#' Format ggplot2 figures in APA style
#'
#' \code{theme_apa()} is designed to work like any other complete theme from
#'   \code{\link[ggplot2]{ggplot}}. To the extent possible, it aligns with
#'   the (vague) APA figure guidelines.
#'
#' @param legend.pos One of `"right"`, `"left"`, `"top"`, `"bottom"`,
#'   `"topleft"`, `"topright"`, `"topmiddle"`, `"bottomleft"`,
#'    `"bottomright"`, or `"bottommiddle"`.
#'   Positions the legend, which will layer on top of any geoms, on the plane.
#'
#' @param legend.use.title Logical. Specify whether to include a legend title.
#'   Defaults to \code{FALSE}.
#'
#' @param legend.font.size Integer indicating the font size of the labels in the
#'   legend. Default and APA-recommended is 12, but if there are many labels it
#'   may be necessary to choose a smaller size.
#'
#' @param x.font.size Font size of x-axis label.
#'
#' @param y.font.size Font size of x-axis label.
#'
#' @param facet.title.size Font size of facet labels.
#'
#' @param remove.x.gridlines Should the coordinate grid on the x-axis (vertical
#'   lines) be removed? Default is TRUE.
#'
#' @param remove.y.gridlines Should the coordinate grid on the y-axis
#'   (horizontal lines) be removed? Default is TRUE.
#'
#' @details This function applies a theme to \code{ggplot2} figures with a
#'   style that is roughly in line with APA guidelines. Users may need to
#'   perform further operations for their specific use cases.
#'
#'   There are some things to keep in mind about APA style figures:
#'   \itemize{
#'    \item Main titles should be written in the word processor or typesetter
#'    rather than on the plot image itself.
#'    \item In some cases, users can forgo a legend in favor of describing the
#'    figure in a caption (also written in the word processor/typesetter).
#'    \item Legends are typically embedded on the coordinate plane of the figure
#'    rather than next to it, as is default in \code{ggplot2}.
#'    \item Use of color is generally discouraged since most of the applications
#'    for which APA figures are needed involve eventual publication in non-color
#'    print media.
#'    \item There are no hard and fast rules on font size, though APA recommends
#'    choosing between 8 and 14-point. Fonts in figures should be sans serif.
#'   }
#'
#'   Because APA style calls for positioning legends on the plane itself, this
#'   function includes options for choosing a position--top left, top right, bottom
#'   left, bottom right--to place the legend. \code{ggplot2} provides no obvious
#'   way to automatically choose a position that overlaps least with the geoms (the
#'   plotted data), so users will need to choose one.
#'
#'   Facetting is supported, but APA guidelines are considerably less clear for
#'   such situations.
#'
#'   This theme was created with inspiration from Rudolf Cardinal's
#'   \href{https://web.archive.org/web/20220616072522/http://egret.psychol.cam.ac.uk/statistics/R/graphs2.html}{code}, 
#'   which required updating for newer versions of \code{ggplot2} and 
#'   adaptations for APA style.
#'
#' @author Jacob Long \email{jacob.long@@sc.edu}
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{theme}}
#'
#' @references
#'
#' American Psychological Association. (2010). \emph{Publication manual of the American
#' Psychological Association, Sixth Edition}. Washington, DC: American Psychological
#'  Association.
#'
#' Nicol, A.A.M. & Pexman, P.M. (2010). \emph{Displaying your findings: A practical
#'  guide for creating figures, posters, and presentations, Sixth Edition}. Washington,
#'  D.C.: American Psychological Association.
#'
#' @examples
#' # Create plot with ggplot2
#' library(ggplot2)
#' plot <- ggplot(mpg, aes(cty, hwy)) +
#'   geom_jitter()
#'
#' # Add APA theme with defaults
#' plot + theme_apa()
#'
#'
#' @export theme_apa

theme_apa <- function(legend.pos = "right", legend.use.title = FALSE,
                      legend.font.size = 12, x.font.size = 12, y.font.size = 12,
                      facet.title.size = 12, remove.y.gridlines = TRUE,
                      remove.x.gridlines = TRUE) {

  # Specifying parameters, using theme_bw() as starting point
  plot <- ggplot2::theme_bw() + ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = 14),
    axis.title.x = ggplot2::element_text(size = x.font.size),
    axis.title.y = ggplot2::element_text(size = y.font.size,
                                         angle = 90),
    legend.text = ggplot2::element_text(size = legend.font.size),
    legend.key.size = ggplot2::unit(1.5, "lines"),
    # switch off the rectangle around symbols
    legend.key = ggplot2::element_blank(),
    legend.key.width = grid::unit(2, "lines"),
    strip.text.x = ggplot2::element_text(size = facet.title.size), # facet labs
    strip.text.y = ggplot2::element_text(size = facet.title.size),
    # facet titles
    strip.background = ggplot2::element_rect(colour = NA, fill = NA),
    panel.background = ggplot2::element_rect(fill = "white"),
    plot.title.position = "panel",
    # complete = TRUE
  )

  # Choose legend position. APA figures generally include legends that
  # are embedded on the plane, so there is no efficient way to have it
  # automatically placed correctly
  if (legend.pos == "topleft") {
    # manually position the legend (numbers being from 0,0 at bottom left of
    # whole plot to 1,1 at top right)
    plot <- plot + ggplot2::theme(legend.position = c(.05, .95),
                                  legend.justification = c(.05, .95))
  } else if (legend.pos == "topright") {
    plot <- plot + ggplot2::theme(legend.position = c(.95, .95),
                                  legend.justification = c(.95, .95))
  } else if (legend.pos == "topmiddle") {
    plot <- plot + ggplot2::theme(legend.position = c(.50, .95),
                                  legend.justification = c(.50, .95))
  } else if (legend.pos == "bottomleft") {
    plot <- plot + ggplot2::theme(legend.position = c(.05, .05),
                                  legend.justification = c(.05, .05))
  } else if (legend.pos == "bottomright") {
    plot <- plot + ggplot2::theme(legend.position = c(.95, .05),
                                  legend.justification = c(.95, .05))
  } else if (legend.pos == "bottommiddle") {
    plot <- plot + ggplot2::theme(legend.position = c(.50, .05),
                                  legend.justification = c(.50, .05))
  } else if (legend.pos == "none") {
    plot <- plot + ggplot2::theme(legend.position = "none")
  } else {
    plot <- plot + ggplot2::theme(legend.position = legend.pos)
  }

  # Should legend have title? If so, format it correctly
  if (legend.use.title == FALSE) {
    # switch off the legend title
    plot <- plot +
      ggplot2::theme(legend.title = ggplot2::element_blank())

  } else {
    plot <- plot +
      ggplot2::theme(legend.title =
                       ggplot2::element_text(size = 12, face = "bold"))
  }

  if (remove.y.gridlines == TRUE) {
    plot <- plot + drop_y_gridlines()
  } else {
    plot <- plot + add_y_gridlines()
  }

  if (remove.x.gridlines == TRUE) {
    plot <- plot + drop_x_gridlines()
  } else {
    plot <- plot + add_x_gridlines()
  }

  return(plot)

}


#' @title Add and remove gridlines
#'
#' @description These are convenience wrappers for editing [ggplot2::theme()]'s
#'  `panel.grid.major` and `panel.grid.minor` parameters with sensible
#'  defaults.
#'
#' @param x Apply changes to the x axis?
#' @param y Apply changes to the y axis?
#' @param minor Add minor gridlines in addition to major?
#' @param minor.only Remove only the minor gridlines?
#'
#' @importFrom ggplot2 theme element_line
#' @export
#' @rdname gridlines

add_gridlines <- function(x = TRUE, y = TRUE, minor = TRUE) {

  plot <- theme()

  if (y == TRUE) {
    plot <- plot + theme(panel.grid.major.y = element_line(colour = "grey92"))
    if (minor == TRUE) {
      plot <-
        plot + theme(panel.grid.minor.y = element_line(colour = "grey92",
                                                       size = .25))
    }
  }

  if (x == TRUE) {
    plot <- plot + theme(panel.grid.major.x = element_line(colour = "grey92"))
    if (minor == TRUE) {
      plot <-
        plot + theme(panel.grid.minor.x = element_line(colour = "grey92",
                                                       size = .25))
    }
  }

  return(plot)

}

#' @export
#' @rdname gridlines

add_x_gridlines <- function(minor = TRUE) {
  add_gridlines(x = TRUE, y = FALSE, minor = minor)
}

#' @export
#' @rdname gridlines

add_y_gridlines <- function(minor = TRUE) {
  add_gridlines(x = FALSE, y = TRUE, minor = minor)
}

#' @export
#' @rdname gridlines

drop_gridlines <- function(x = TRUE, y = TRUE, minor.only = FALSE) {

  plot <- ggplot2::theme()

  if (y == TRUE) {
    plot <- plot + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    if (minor.only == FALSE) {
      plot <-
        plot + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    }
  }

  if (x == TRUE) {
    plot <- plot + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    if (minor.only == FALSE) {
      plot <-
        plot + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    }
  }

  return(plot)

}

#' @export
#' @rdname gridlines

drop_x_gridlines <- function(minor.only = FALSE) {
  drop_gridlines(x = TRUE, y = FALSE, minor.only = minor.only)
}

#' @export
#' @rdname gridlines

drop_y_gridlines <- function(minor.only = FALSE) {
  drop_gridlines(x = FALSE, y = TRUE, minor.only = minor.only)
}
