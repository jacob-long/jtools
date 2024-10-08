% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_apa.R
\name{theme_apa}
\alias{theme_apa}
\title{Format ggplot2 figures in APA style}
\usage{
theme_apa(
  legend.pos = "right",
  legend.use.title = FALSE,
  legend.font.size = 12,
  x.font.size = 12,
  y.font.size = 12,
  facet.title.size = 12,
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE
)
}
\arguments{
\item{legend.pos}{One of \code{"right"}, \code{"left"}, \code{"top"}, \code{"bottom"},
\code{"topleft"}, \code{"topright"}, \code{"topmiddle"}, \code{"bottomleft"},
\code{"bottomright"}, or \code{"bottommiddle"}.
Positions the legend, which will layer on top of any geoms, on the plane.}

\item{legend.use.title}{Logical. Specify whether to include a legend title.
Defaults to \code{FALSE}.}

\item{legend.font.size}{Integer indicating the font size of the labels in the
legend. Default and APA-recommended is 12, but if there are many labels it
may be necessary to choose a smaller size.}

\item{x.font.size}{Font size of x-axis label.}

\item{y.font.size}{Font size of x-axis label.}

\item{facet.title.size}{Font size of facet labels.}

\item{remove.y.gridlines}{Should the coordinate grid on the y-axis
(horizontal lines) be removed? Default is TRUE.}

\item{remove.x.gridlines}{Should the coordinate grid on the x-axis (vertical
lines) be removed? Default is TRUE.}
}
\description{
\code{theme_apa()} is designed to work like any other complete theme from
\code{\link[ggplot2]{ggplot}}. To the extent possible, it aligns with
the (vague) APA figure guidelines.
}
\details{
This function applies a theme to \code{ggplot2} figures with a
style that is roughly in line with APA guidelines. Users may need to
perform further operations for their specific use cases.

There are some things to keep in mind about APA style figures:
\itemize{
\item Main titles should be written in the word processor or typesetter
rather than on the plot image itself.
\item In some cases, users can forgo a legend in favor of describing the
figure in a caption (also written in the word processor/typesetter).
\item Legends are typically embedded on the coordinate plane of the figure
rather than next to it, as is default in \code{ggplot2}.
\item Use of color is generally discouraged since most of the applications
for which APA figures are needed involve eventual publication in non-color
print media.
\item There are no hard and fast rules on font size, though APA recommends
choosing between 8 and 14-point. Fonts in figures should be sans serif.
}

Because APA style calls for positioning legends on the plane itself, this
function includes options for choosing a position--top left, top right, bottom
left, bottom right--to place the legend. \code{ggplot2} provides no obvious
way to automatically choose a position that overlaps least with the geoms (the
plotted data), so users will need to choose one.

Facetting is supported, but APA guidelines are considerably less clear for
such situations.

This theme was created with inspiration from Rudolf Cardinal's
\href{https://web.archive.org/web/20220616072522/http://egret.psychol.cam.ac.uk/statistics/R/graphs2.html}{code},
which required updating for newer versions of \code{ggplot2} and
adaptations for APA style.
}
\examples{
# Create plot with ggplot2
library(ggplot2)
plot <- ggplot(mpg, aes(cty, hwy)) +
  geom_jitter()

# Add APA theme with defaults
plot + theme_apa()


}
\references{
American Psychological Association. (2010). \emph{Publication manual of the American
Psychological Association, Sixth Edition}. Washington, DC: American Psychological
Association.

Nicol, A.A.M. & Pexman, P.M. (2010). \emph{Displaying your findings: A practical
guide for creating figures, posters, and presentations, Sixth Edition}. Washington,
D.C.: American Psychological Association.
}
\seealso{
\code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{theme}}
}
\author{
Jacob Long \email{jacob.long@sc.edu}
}
