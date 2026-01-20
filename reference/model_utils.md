# Utility functions for generating model predictions

These functions get information and data from regression models.

## Usage

``` r
get_offset_name(model)

get_weights(model, data)

get_data(model, formula = NULL, warn = TRUE, ...)

get_response_name(model, ...)
```

## Arguments

- model:

  The model (e.g., `lm`, `glm`, `merMod`, `svyglm`)

- data:

  For `get_weights()`, the data used to fit the model.

- formula:

  The formula for `model`, if desired. Otherwise
  [`get_formula()`](get_formula.md) is called.

- warn:

  For `get_data()`, should there be a warning when
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) won't work
  because of variable transformations? Default is TRUE but this may not
  be desired when `get_data()` is used inside of another function or
  used multiple times.

- ...:

  Arguments passed to [`get_formula()`](get_formula.md)

## Value

- `get_data()`: The data used to fit the model.

- `get_response_name()`: The name of the response variable.

- `get_offset_name()`: The name of the offset variable.

- `get_weights()`: A list with `weights_name`, the name of the weighting
  variable, and `weights`, the weights themselves (or all 1 when there
  are no weights).
