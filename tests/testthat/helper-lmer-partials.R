compare_lmer_partial_svg <- function(old, new) {
  reference <- xml2::read_xml(old, options = "NONET")
  actual <- xml2::read_xml(new, options = "NONET")
  circles <- function(svg) xml2::xml_find_all(svg, "//*[local-name()='circle']")
  before <- circles(reference)
  after <- circles(actual)
  if (!length(before) || length(before) != length(after)) return(FALSE)

  # The same lmer fit can move a rendered residual by one SVG rounding unit.
  cy <- xml2::xml_attr(before, "cy")
  delta <- abs(as.numeric(cy) - as.numeric(xml2::xml_attr(after, "cy")))
  if (!all(is.finite(delta) & delta <= 0.01 + 1e-12)) return(FALSE)
  xml2::xml_set_attr(after, "cy", cy)
  identical(as.character(reference), as.character(actual))
}

expect_lmer_partial_plot <- function(plot) {
  testthat::local_edition(3)
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  vdiffr::write_svg(plot, path, title = "lmer-partials")
  testthat::expect_snapshot_file(path, name = "lmer-partials.svg",
                                 compare = compare_lmer_partial_svg)
}
