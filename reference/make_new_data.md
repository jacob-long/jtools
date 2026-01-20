# Make new data for generating predicted data from regression models.

This is a convenience function that helps automate the process of
generating predicted data from regression model from a predictor(s). It
is designed to give you the data frame for the `predict` method's
`newdata` argument.

## Usage

``` r
make_new_data(
  model,
  pred,
  pred.values = NULL,
  at = NULL,
  data = NULL,
  center = TRUE,
  set.offset = NULL,
  num.preds = 100,
  ...
)
```

## Arguments

- model:

  The model (e.g., `lm`, `glm`, `merMod`, `svyglm`)

- pred:

  The name of the focal predictor as a string. This is the variable for
  which, if you are plotting, you'd likely have along the x-axis (with
  the dependent variable as the y-axis).

- pred.values:

  The values of `pred` you want to include. Default is NULL, which means
  a sequence of equi-spaced values over the range of a numeric predictor
  or each level of a non-numeric predictor.

- at:

  If you want to manually set the values of other variables in the
  model, do so by providing a named list where the names are the
  variables and the list values are vectors of the values. This can be
  useful especially when you are exploring interactions or other
  conditional predictions.

- data:

  The data frame used in fitting the model. Default is NULL, in which
  case the data will be retrieved via `model.frame` or, if there are
  variable transformations in the formula, by looking in the environment
  for the data.

- center:

  Set numeric covariates to their mean? Default is TRUE. You may also
  just provide a vector of names (as strings) of covariates to center.
  Note that for `svyglm` models, the survey-weighted means are used. For
  models with weights, these are weighted means.

- set.offset:

  If the model has an offset, the value to use for the offset variable.
  Default is NULL, in which case the median of the offset variable is
  used.

- num.preds:

  The number of predictions to generate. Default is 100. Ignored if
  `pred.values` is not `NULL`.

- ...:

  Extra arguments passed to [`get_formula()`](get_formula.md)

## Value

A data frame.

## Details

Please bear in mind that this does not generate the predictions. You
will need to do that with a `predict` function for your model or another
interface, such as the `prediction` package's titular function.

## Examples

``` r
fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))
# Basic use
new_data <- make_new_data(fit, pred = "Frost")
# Set covariate to specific value
new_data <- make_new_data(fit, pred = "Frost", at = list(Murder = 5))
# Set covariate to several specific values
new_data <- make_new_data(fit, pred = "Frost", at = list(Murder = c(5, 10, 15)))

```
