# Center variables in fitted regression models

`center_mod` (previously known as `center_lm`) takes fitted regression
models and mean-centers the continuous variables in the model to aid
interpretation, especially in the case of models with interactions. It
is a wrapper to [`scale_mod`](scale_mod.md).

## Usage

``` r
center_mod(
  model,
  binary.inputs = "0/1",
  center.response = FALSE,
  data = NULL,
  apply.weighted.contrasts = getOption("jtools-weighted.contrasts", FALSE),
  ...
)
```

## Arguments

- model:

  A regression model of type `lm`, `glm`, or
  [`svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html); others may
  work as well but have not been tested.

- binary.inputs:

  Options for binary variables. Default is `0/1`; `0/1` keeps original
  scale; `-0.5,0.5` rescales 0 as -0.5 and 1 as 0.5; `center` subtracts
  the mean; and `full` treats them like other continuous variables.

- center.response:

  Should the response variable also be centered? Default is `FALSE`.

- data:

  If you provide the data used to fit the model here, that data frame is
  used to re-fit the model instead of the
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html) of
  the model. This is particularly useful if you have variable
  transformations or polynomial terms specified in the formula.

- apply.weighted.contrasts:

  Factor variables cannot be scaled, but you can set the contrasts such
  that the intercept in a regression model will reflect the true mean
  (assuming all other variables are centered). If set to TRUE, the
  argument will apply weighted effects coding to all factors. This is
  similar to the R default effects coding, but weights according to how
  many observations are at each level. An adapted version of
  `contr.wec()` from the `wec` package is used to do this. See that
  package's documentation and/or Grotenhuis et al. (2016) for more info.

- ...:

  Arguments passed on to [`gscale()`](gscale.md).

## Value

The functions returns a `lm` or `glm` object, inheriting from whichever
class was supplied.

## Details

This function will mean-center all continuous variables in a regression
model for ease of interpretation, especially for those models that have
interaction terms. The mean for `svyglm` objects is calculated using
`svymean`, so reflects the survey-weighted mean. The weight variables in
`svyglm` are not centered, nor are they in other `lm` family models.

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
[`gscale()`](gscale.md), [`scale_mod()`](scale_mod.md),
[`standardize()`](standardize.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
fit <- lm(formula = Murder ~ Income * Illiteracy,
          data = as.data.frame(state.x77))
fit_center <- center_mod(fit)

# With weights
fitw <- lm(formula = Murder ~ Income * Illiteracy,
           data = as.data.frame(state.x77),
           weights = Population)
fitw_center <- center_mod(fitw)

# With svyglm
if (requireNamespace("survey")) {
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                    data = apistrat, fpc =~ fpc)
regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)
regmodel_center <- center_mod(regmodel)
}
#> Loading required namespace: survey
#> Loading required package: grid
#> Loading required package: Matrix
#> Loading required package: survival
#> 
#> Attaching package: ‘survey’
#> The following object is masked from ‘package:graphics’:
#> 
#>     dotchart
```
