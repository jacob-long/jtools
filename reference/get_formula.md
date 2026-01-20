# Retrieve formulas from model objects

This function is primarily an internal helper function in `jtools` and
related packages to standardize the different types of formula objects
used by different types of models.

## Usage

``` r
get_formula(model, ...)

# Default S3 method
get_formula(model, ...)

# S3 method for class 'brmsfit'
get_formula(model, resp = NULL, dpar = NULL, ...)

# S3 method for class 'panelmodel'
get_formula(model, ...)
```

## Arguments

- model:

  The fitted model object.

- ...:

  Ignored.

- resp:

  For `brmsfit` objects, the response variable for which the formula is
  desired. `brmsfit` objects may have multiple formulas, so this selects
  a particular one. If `NULL`, the first formula is chosen (unless
  `dpar` is specified).

- dpar:

  For `brmsfit` objects, the distributional variable for which the
  formula is desired. If `NULL`, no distributional parameter is used. If
  there are multiple responses with distributional parameters, then
  `resp` should be specified or else the first formula will be used by
  default.

## Value

A `formula` object.

## Examples

``` r
data(mtcars)
fit <- lm(mpg ~ cyl, data = mtcars)
get_formula(fit)
#> mpg ~ cyl
#> <environment: 0x557344fa0310>
```
