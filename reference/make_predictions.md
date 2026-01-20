# Generate predicted data for plotting results of regression models

This is an alternate interface to the underlying tools that make up
[`effect_plot()`](effect_plot.md) as well as
`interactions::interact_plot()` and `interactions::cat_plot()` from the
`interactions` package. `make_predictions()` creates the data to be
plotted and adds information to the original data to make it more
amenable for plotting with the predicted data.

## Usage

``` r
make_predictions(model, ...)

# Default S3 method
make_predictions(
  model,
  pred,
  pred.values = NULL,
  at = NULL,
  data = NULL,
  center = TRUE,
  interval = TRUE,
  int.type = c("confidence", "prediction"),
  int.width = 0.95,
  outcome.scale = "response",
  robust = FALSE,
  cluster = NULL,
  vcov = NULL,
  set.offset = NULL,
  new_data = NULL,
  return.orig.data = FALSE,
  partial.residuals = FALSE,
  ...
)
```

## Arguments

- model:

  The model (e.g., `lm`, `glm`, `merMod`, `svyglm`)

- ...:

  Ignored.

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

  Optional, default is NULL. You may provide the data used to fit the
  model. This can be a better way to get mean values for centering and
  can be crucial for models with variable transformations in the formula
  (e.g., `log(x)`) or polynomial terms (e.g., `poly(x, 2)`). You will
  see a warning if the function detects problems that would likely be
  solved by providing the data with this argument and the function will
  attempt to retrieve the original data from the global environment.

- center:

  Set numeric covariates to their mean? Default is TRUE. You may also
  just provide a vector of names (as strings) of covariates to center.
  Note that for `svyglm` models, the survey-weighted means are used. For
  models with weights, these are weighted means.

- interval:

  Logical. If `TRUE`, plots confidence/prediction intervals around the
  line using
  [`geom_ribbon`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html).

- int.type:

  Type of interval to plot. Options are "confidence" or "prediction".
  Default is confidence interval.

- int.width:

  How large should the interval be, relative to the standard error? The
  default, .95, corresponds to roughly 1.96 standard errors and a .05
  alpha level for values outside the range. In other words, for a
  confidence interval, .95 is analogous to a 95% confidence interval.

- outcome.scale:

  For nonlinear models (i.e., GLMs), should the outcome variable be
  plotted on the link scale (e.g., log odds for logit models) or the
  original scale (e.g., predicted probabilities for logit models)? The
  default is `"response"`, which is the original scale. For the link
  scale, which will show straight lines rather than curves, use
  `"link"`.

- robust:

  Should robust standard errors be used to find confidence intervals for
  supported models? Default is FALSE, but you should specify the type of
  sandwich standard errors if you'd like to use them (i.e., `"HC0"`,
  `"HC1"`, and so on). If `TRUE`, defaults to `"HC3"` standard errors.

- cluster:

  For clustered standard errors, provide the column name of the cluster
  variable in the input data frame (as a string). Alternately, provide a
  vector of clusters.

- vcov:

  Optional. You may supply the variance-covariance matrix of the
  coefficients yourself. This is useful if you are using some method for
  robust standard error calculation not supported by the sandwich
  package.

- set.offset:

  For models with an offset (e.g., Poisson models), sets an offset for
  the predicted values. All predicted values will have the same offset.
  By default, this is set to 1, which makes the predicted values a
  proportion. See details for more about offset support.

- new_data:

  If you would prefer to generate your own hypothetical (or not
  hypothetical) data rather than have the function make a call to
  [`make_new_data()`](make_new_data.md), you can provide it.

- return.orig.data:

  Instead of returning a just the predicted data frame, should the
  original data be returned as well? If so, then a list will be return
  with both the predicted data (as the first element) and the original
  data (as the second element). Default is FALSE.

- partial.residuals:

  If `return.orig.data` is TRUE, should the observed dependent variable
  be replaced with the partial residual? This makes a call to
  [`partialize()`](partialize.md), where you can find more details.
