# Add and remove gridlines

These are convenience wrappers for editing
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)'s
`panel.grid.major` and `panel.grid.minor` parameters with sensible
defaults.

## Usage

``` r
add_gridlines(x = TRUE, y = TRUE, minor = TRUE)

add_x_gridlines(minor = TRUE)

add_y_gridlines(minor = TRUE)

drop_gridlines(x = TRUE, y = TRUE, minor.only = FALSE)

drop_x_gridlines(minor.only = FALSE)

drop_y_gridlines(minor.only = FALSE)
```

## Arguments

- x:

  Apply changes to the x axis?

- y:

  Apply changes to the y axis?

- minor:

  Add minor gridlines in addition to major?

- minor.only:

  Remove only the minor gridlines?
