# Scale variables in fitted regression models

`scale_mod` (previously known as `scale_lm`) takes fitted regression
models and scales all predictors by dividing each by 1 or 2 standard
deviations (as chosen by the user).

## Usage

``` r
scale_mod(model, ...)

# Default S3 method
scale_mod(
  model,
  binary.inputs = "0/1",
  n.sd = 1,
  center = TRUE,
  scale.response = FALSE,
  center.only = FALSE,
  scale.only = FALSE,
  data = NULL,
  vars = NULL,
  apply.weighted.contrasts = getOption("jtools-weighted.contrasts", FALSE),
  ...
)
```

## Arguments

- model:

  A regression model of type `lm`, `glm`,
  [`svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html), or
  [lme4::merMod](https://rdrr.io/pkg/lme4/man/merMod-class.html). Other
  model types may work as well but are not tested.

- ...:

  Arguments passed on to [`gscale()`](gscale.md).

- binary.inputs:

  Options for binary variables. Default is `"0/1"`; `"0/1"` keeps
  original scale; `"-0.5,0.5"` rescales 0 as -0.5 and 1 as 0.5; `center`
  subtracts the mean; and `full` treats them like other continuous
  variables.

- n.sd:

  How many standard deviations should you divide by for standardization?
  Default is 1, though some prefer 2.

- center:

  Default is `TRUE`. If `TRUE`, the predictors are also mean-centered.
  For binary predictors, the `binary.inputs` argument supersedes this
  one.

- scale.response:

  Should the response variable also be rescaled? Default is `FALSE`.

- center.only:

  Rather than actually scale predictors, just mean-center them.

- scale.only:

  A logical value indicating whether you would like to scale the values,
  but not mean-center them.

- data:

  If you provide the data used to fit the model here, that data frame is
  used to re-fit the model instead of the
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html) of
  the model. This is particularly useful if you have variable
  transformations or polynomial terms specified in the formula.

- vars:

  A character vector of variable names that you want to be scaled. If
  NULL, the default, it is all predictors.

- apply.weighted.contrasts:

  Factor variables cannot be scaled, but you can set the contrasts such
  that the intercept in a regression model will reflect the true mean
  (assuming all other variables are centered). If set to TRUE, the
  argument will apply weighted effects coding to all factors. This is
  similar to the R default effects coding, but weights according to how
  many observations are at each level. An adapted version of
  `contr.wec()` from the `wec` package is used to do this. See that
  package's documentation and/or Grotenhuis et al. (2016) for more info.

## Value

The functions returns a re-fitted model object, inheriting from
whichever class was supplied.

## Details

This function will scale all continuous variables in a regression model
for ease of interpretation, especially for those models that have
interaction terms. It can also mean-center all of them as well, if
requested.

The scaling happens on the input data, not the terms themselves. That
means interaction terms are still properly calculated because they are
the product of standardized predictors, not a standardized product of
predictors.

This function re-estimates the model, so for large models one should
expect a runtime equal to the first run.

## References

Bauer, D. J., & Curran, P. J. (2005). Probing interactions in fixed and
multilevel regression: Inferential and graphical techniques.
*Multivariate Behavioral Research*, *40*(3), 373-400.

Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). *Applied
multiple regression/correlation analyses for the behavioral sciences*
(3rd ed.). Mahwah, NJ: Lawrence Erlbaum Associates, Inc.

## See also

[`sim_slopes`](interactions_deprecated.md) performs a simple slopes
analysis.

[`interact_plot`](interactions_deprecated.md) creates attractive,
user-configurable plots of interaction models.

standardization, scaling, and centering tools [`center()`](center.md),
[`center_mod()`](center_mod.md), [`gscale()`](gscale.md),
[`standardize()`](standardize.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
fit <- lm(formula = Murder ~ Income * Illiteracy,
          data = as.data.frame(state.x77))
fit_scale <- scale_mod(fit)
fit_scale <- scale_mod(fit, center = TRUE)

# With weights
fitw <- lm(formula = Murder ~ Income * Illiteracy,
           data = as.data.frame(state.x77),
           weights = Population)
fitw_scale <- scale_mod(fitw)
fitw_scale <- scale_mod(fitw, center = TRUE, binary.input = "0/1")

# With svyglm
if (requireNamespace("survey")) {
library(survey)
data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
regmodel <- svyglm(api00~ell*meals,design=dstrat)
regmodel_scale <- scale_mod(regmodel)
regmodel_scale <- scale_mod(regmodel, binary.input = "0/1")
}
```
