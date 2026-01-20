# Complex survey regression summaries with options

[`summ()`](summ.md) prints output for a regression model in a fashion
similar to [`summary()`](https://rdrr.io/r/base/summary.html), but
formatted differently with more options.

## Usage

``` r
# S3 method for class 'svyglm'
summ(
  model,
  scale = FALSE,
  confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", 0.95),
  digits = getOption("jtools-digits", default = 2),
  pvals = getOption("summ-pvals", TRUE),
  n.sd = 1,
  center = FALSE,
  transform.response = FALSE,
  scale.only = FALSE,
  exp = FALSE,
  vifs = getOption("summ-vifs", FALSE),
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE),
  model.coefs = getOption("summ-model.coefs", TRUE),
  which.cols = NULL,
  ...
)
```

## Arguments

- model:

  A `svyglm` object.

- scale:

  If `TRUE`, reports standardized regression coefficients by scaling and
  mean-centering input data (the latter can be changed via the
  `scale.only` argument). Default is `FALSE`.

- confint:

  Show confidence intervals instead of standard errors? Default is
  `FALSE`.

- ci.width:

  A number between 0 and 1 that signifies the width of the desired
  confidence interval. Default is `.95`, which corresponds to a 95%
  confidence interval. Ignored if `confint = FALSE`.

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 2. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

- pvals:

  Show p values? If `FALSE`, these are not printed. Default is `TRUE`.

- n.sd:

  If `scale = TRUE`, how many standard deviations should predictors be
  divided by? Default is 1, though some suggest 2.

- center:

  If you want coefficients for mean-centered variables but don't want to
  standardize, set this to `TRUE`. Note that setting this to false does
  not affect whether `scale` mean-centers variables. Use `scale.only`
  for that.

- transform.response:

  Should scaling/centering apply to response variable? Default is
  `FALSE`.

- scale.only:

  If you want to scale but not center, set this to `TRUE`. Note that for
  legacy reasons, setting `scale = TRUE` and `center = FALSE` will not
  achieve the same effect. Default is `FALSE`.

- exp:

  If `TRUE`, reports exponentiated coefficients with confidence
  intervals for exponential models like logit and Poisson models. This
  quantity is known as an odds ratio for binary outcomes and incidence
  rate ratio for count models.

- vifs:

  If `TRUE`, adds a column to output with variance inflation factors
  (VIF). Default is `FALSE`.

- model.info:

  Toggles printing of basic information on sample size, name of DV, and
  number of predictors.

- model.fit:

  Toggles printing of model fit statistics.

- model.coefs:

  Toggles printing of model coefficents.

- which.cols:

  Developmental feature. By providing columns by name, you can
  add/remove/reorder requested columns in the output. Not fully
  supported, for now.

- ...:

  Among other things, arguments are passed to
  [`scale_mod()`](scale_mod.md) or [`center_mod()`](center_mod.md) when
  `center` or `scale` is `TRUE`.

## Value

If saved, users can access most of the items that are returned in the
output (and without rounding).

- coeftable:

  The outputted table of variables and coefficients

- model:

  The model for which statistics are displayed. This would be most
  useful in cases in which `scale = TRUE`.

Much other information can be accessed as attributes.

## Details

By default, this function will print the following items to the console:

- The sample size

- The name of the outcome variable

- The (Pseudo-)R-squared value and AIC.

- A table with regression coefficients, standard errors, t values, and p
  values.

The `scale` and `center` options are performed via refitting the model
with [`scale_mod()`](scale_mod.md) and [`center_mod()`](center_mod.md),
respectively. Each of those in turn uses [`gscale()`](gscale.md) for the
mean-centering and scaling. These functions can handle `svyglm` objects
correctly by calling
[`svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html) and
[`svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html) to
compute means and standard deviations. Weights are not altered. The fact
that the model is refit means the runtime will be similar to the
original time it took to fit the model.

## See also

[`scale_mod()`](scale_mod.md) can simply perform the standardization if
preferred.

[`gscale()`](gscale.md) does the heavy lifting for mean-centering and
scaling behind the scenes.

Other summ: [`summ.glm()`](summ.glm.md), [`summ.lm()`](summ.lm.md),
[`summ.merMod()`](summ.merMod.md), [`summ.rq()`](summ.rq.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
if (requireNamespace("survey")) {
  library(survey)
  data(api)
  dstrat <- svydesign(id = ~1, strata =~ stype, weights =~ pw,
                      data = apistrat, fpc =~ fpc)
  regmodel <- svyglm(api00 ~ ell * meals, design = dstrat)

  summ(regmodel)
}
#> MODEL INFO:
#> Observations: 200
#> Dependent Variable: api00
#> Type: Survey-weighted linear regression 
#> 
#> MODEL FIT:
#> R² = 0.66
#> Adj. R² = 0.66 
#> 
#> Standard errors: Robust
#> --------------------------------------------------
#>                       Est.    S.E.   t val.      p
#> ----------------- -------- ------- -------- ------
#> (Intercept)         836.62   10.99    76.10   0.00
#> ell                  -1.69    1.01    -1.67   0.10
#> meals                -3.32    0.28   -11.99   0.00
#> ell:meals             0.02    0.01     1.42   0.16
#> --------------------------------------------------
#> 
#> Estimated dispersion parameter = 5118.85 
```
