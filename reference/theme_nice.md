# A nice, flexible `ggplot2` theme

`theme_nice` is designed to work like any other complete theme from
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html). It has
a nice appearance.

## Usage

``` r
theme_nice(
  legend.pos = "right",
  style = c("white", "light", "dark_blue", "dark_gray"),
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
)
```

## Arguments

- legend.pos:

  One of `"right"`, `"left"`, `"top"`, `"bottom"` (outside the plotting
  area), `"topleft"`, `"topright"`, `"topmiddle"`, `"bottomleft"`,
  `"bottomright"`, or `"bottommiddle"` (inside the plotting area).

- style:

  One of `"white"`, `"light"`, `"dark_blue"`, or `"dark_gray"`.
  `"white"` sets the background to white, `"light"` to light gray,
  `"dark_gray"` to dark gray, `"dark_blue"` to dark blue.

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- base_line_size:

  base size for line elements

- base_rect_size:

  base size for rect elements

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
# Create plot with ggplot2
library(ggplot2)
plot <- ggplot(mpg, aes(cty, hwy)) +
  geom_jitter() + theme_nice()

```
