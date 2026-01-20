# Adjust observed data for partial residuals plots

This function is designed to facilitate the creation of partial residual
plots, in which you can plot observed data alongside model predictions.
The difference is instead of the *actual* observed data, the outcome
variable is adjusted for the effects of the covariates.

## Usage

``` r
partialize(model, ...)

# Default S3 method
partialize(
  model,
  vars = NULL,
  data = NULL,
  at = NULL,
  center = TRUE,
  scale = c("response", "link"),
  set.offset = 1,
  ...
)
```

## Arguments

- model:

  A regression model.

- ...:

  Ignored.

- vars:

  The variable(s) to *not* adjust for, as a string (or vector of
  strings). If I want to show the effect of `x` adjusting for the effect
  of `z`, then I would make `"x"` the `vars` argument.

- data:

  Optionally, provide the data used to fit the model (or some other data
  frame with the same variables). Otherwise, it will be retrieved from
  the model or the global environment.

- at:

  If you want to manually set the values of other variables in the
  model, do so by providing a named list where the names are the
  variables and the list values are vectors of the values. This can be
  useful especially when you are exploring interactions or other
  conditional predictions.

- center:

  Set numeric covariates to their mean? Default is TRUE. You may also
  just provide a vector of names (as strings) of covariates to center.
  Note that for `svyglm` models, the survey-weighted means are used. For
  models with weights, these are weighted means.

- scale:

  For GLMs, should the outcome variable be returned on the link scale or
  response scale? Default is `"response"`.

- set.offset:

  For models with an offset (e.g., Poisson models), sets an offset for
  the predicted values. All predicted values will have the same offset.
  By default, this is set to 1, which makes the predicted values a
  proportion. See details for more about offset support.

## Value

`data` plus the residualized outcome variable.

## Details

The main use for working with partial residuals rather than the observed
values is to explore patterns in the model fit with respect to one or
more variables while "controlling out" the effects of others. Plotting a
predicted line along with observed data may make a very well-fitting
model look as if it is a poor fit if a lot of variation is accounted for
by variables other than the one on the x-axis.

I advise consulting Fox and Weisberg (available free) for more details
on what partial residuals are. This function is designed to produce data
in a similar format to `effects::Effect()` when that function has
`residuals` set to `TRUE` and is plotted. I wanted a more modular
function to produce the data separately. To be clear, the developers of
the `effects` package have nothing to do with this function;
\`partialize“ is merely designed to replicate some of that
functionality.

## References

Fox, J., & Weisberg, S. (2018). Visualizing fit and lack of fit in
complex regression models with predictor effect plots and partial
residuals. *Journal of Statistical Software*, *87*(9), 1–27.
https://doi.org/10.18637/jss.v087.i09
