# Get colors for plotting functions

This is a helper function that provides hex color codes for `jtools`,
`interactions`, and perhaps other packages.

## Usage

``` r
get_colors(colors, num.colors = 1, reverse = FALSE, gradient = FALSE)
```

## Arguments

- colors:

  The name of the desired color class or a vector of colors. See details
  of [jtools_colors](jtools_colors.md).

- num.colors:

  How many colors should be returned? Default is 1.

- reverse:

  Should the colors be returned in reverse order, compared to normal?
  Default is FALSE.

- gradient:

  Return endpoints for a gradient? Default is FALSE. If TRUE,
  `num.colors` is ignored.
