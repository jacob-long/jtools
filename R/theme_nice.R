#' A nice, flexible `ggplot2` theme
#'
#' `theme_nice` is designed to work like any other complete theme from
#'   \code{\link[ggplot2]{ggplot}}. It has a nice appearance.
#'
#' @param legend.pos One of `"right"`, `"left"`, `"top"`, `"bottom"` (outside
#'   the plotting area), `"topleft"`, `"topright"`, `"topmiddle"`,
#'   `"bottomleft"`, `"bottomright"`, or `"bottommiddle"` (inside the plotting
#'   area).
#' 
#' @param style One of `"white"`, `"light"`, `"dark_blue"`, or `"dark_gray"`.
#'  `"white"` sets the background to white, `"light"` to light gray,
#'  `"dark_gray"` to dark gray, `"dark_blue"` to dark blue. 
#' 
#' @inheritParams ggplot2::theme_grey
#'
#' @author Jacob Long <\email{long.1377@@osu.edu}>
#'
#' @examples
#' # Create plot with ggplot2
#' library(ggplot2)
#' plot <- ggplot(mpg, aes(cty, hwy)) +
#'   geom_jitter() + theme_nice()
#'
#'
#' @export 

theme_nice <- function(legend.pos = "right",
                       style = c("white", "light", "dark_blue", "dark_gray"),
                       base_size = 11, base_family = "", 
                       base_line_size = base_size/22, 
                       base_rect_size = base_size/22) {
  
  text_and_line_color <- switch(style[1], 
                                white = "#34495e",
                                light = "#34495e",
                                dark_blue = "#ecf0f1",
                                dark_gray = "#ecf0f1",
                                light_dark = "#34495e")
  bg_color <- switch(style[1], 
                     white = "#FFFFFF",
                     light = "#ecf0f1",
                     dark_blue = "#34495e",
                     dark_gray = "#3c3c3c",
                     light_dark = "#3c3c3c")
  
  all_bg <- switch(style[1], 
                   white = "#FFFFFF",
                   light = "#ecf0f1",
                   dark_blue = "#34495e",
                   dark_gray = "#3c3c3c",
                   light_dark = "#dddddd")

  grid_color <- switch(style[1], 
                       white = "#e8e8e8",
                       light = "#bdc3c7",
                       dark_blue = "#46627f",
                       dark_gray = "#777777",
                       light_dark = "#777777")
  
  # Specifying parameters, using theme_bw() as starting point
  plot <- theme_minimal(base_size = base_size,  base_family = base_family,
                        base_line_size = base_line_size, 
                        base_rect_size = base_rect_size) %+replace%
    ggplot2::theme(
      line = element_line(colour = text_and_line_color, size = .5, linetype = 1,
                          lineend = "butt", arrow = FALSE),
      text = element_text(colour = text_and_line_color, face = "plain", family = "sans", 
                          size = 11, hjust = .5, vjust = .5, lineheight = 1,
                          margin = margin(5,5,5,5, "pt"), angle = 0,
                          debug = FALSE),
      rect = element_rect(colour = text_and_line_color, fill = bg_color, size = .5,
                          linetype = 1),
      plot.background = element_rect(fill = all_bg),
      plot.title = element_text(face='bold',
                                margin = margin(10, 5, 10, 5, "pt"),
                                hjust = 0, size = 14),
      panel.background = element_rect(fill = bg_color, colour = bg_color,
                                      size = 0),
      panel.border = element_blank(),
      axis.title = element_text(face='bold'),
      legend.title = element_text(face='bold'),
      strip.text = element_text(face='bold'),
      # axis.line = element_blank(),
      legend.key.width = ggplot2::unit(1.5, "lines"),
      legend.key = element_rect(colour = bg_color),
      axis.line.x.top = element_blank(),
      axis.line.y.right = element_blank(),
      axis.line.x.bottom = element_line(colour = text_and_line_color,
                                        linetype = "solid"),
      axis.line.y.left = element_line(colour = text_and_line_color,
                                      linetype = "solid"),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(linetype='longdash', colour = grid_color),
                                      # colour = "#bdc3c7"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(colour = text_and_line_color),
      axis.text.y = element_text(colour = text_and_line_color)
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
  
  
  return(plot)
  
}