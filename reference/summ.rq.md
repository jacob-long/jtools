# Quantile regression summaries with options

[`summ()`](summ.md) prints output for a regression model in a fashion
similar to [`summary()`](https://rdrr.io/r/base/summary.html), but
formatted differently with more options.

## Usage

``` r
# S3 method for class 'rq'
summ(
  model,
  scale = FALSE,
  confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", 0.95),
  se = c("nid", "rank", "iid", "ker", "boot"),
  boot.sims = 1000,
  boot.method = "xy",
  vifs = getOption("summ-vifs", FALSE),
  digits = getOption("jtools-digits", 2),
  pvals = getOption("summ-pvals", TRUE),
  n.sd = 1,
  center = FALSE,
  transform.response = FALSE,
  data = NULL,
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE),
  model.coefs = getOption("summ-model.coefs", TRUE),
  which.cols = NULL,
  ...
)
```

## Arguments

- model:

  A `rq` model. At this time, `rqs` models (multiple `tau` parameters)
  are not supported.

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

- se:

  One of "nid", "rank", "iid", "ker", or "boot". "nid" is default. See
  [`quantreg::summary.rq()`](https://rdrr.io/pkg/quantreg/man/summary.rq.html)
  documentation for more about these options.

- boot.sims:

  If `se = "boot"`, the number of bootstrap replications to perform.
  This is passed as the `R` argument to `boot.rq`

- boot.method:

  If `se = "boot"`, the type of bootstrapping method to use. Default is
  "xy", but see
  [`quantreg::boot.rq()`](https://rdrr.io/pkg/quantreg/man/boot.rq.html)
  for more options.

- vifs:

  If `TRUE`, adds a column to output with variance inflation factors
  (VIF). Default is `FALSE`.

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

- data:

  If you provide the data used to fit the model here, that data frame is
  used to re-fit the model (if `scale` is `TRUE`) instead of the
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html) of
  the model. This is particularly useful if you have variable
  transformations or polynomial terms specified in the formula.

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

## Details

This method implements most of the things I think most users would
asking `summary.rq` for. `hs`, `U`, and `gamma` are ignored.

Note that when using `se = "rank"`, there are no standard errors, test
statistics, or p values calculated.

About the R1 fit statistic: Described in Koenker & Machado (1999), this
offers an interpretation similar to R-squared in OLS regression. While
you could calculate R-squared for these models, it goes against the
underlying theoretical rationale for them. Koenker himself is not a big
fan of R1 either, but it's something. See Koenker & Machado (1999) for
more info.

## References

Koenker, R., & Machado, J. A. F. (1999). Goodness of fit and related
inference processes for quantile regression. *Journal of the American
Statistical Association*, *94*, 1296–1310.
https://doi.org/10.1080/01621459.1999.10473882

## See also

Other summ: [`summ.glm()`](summ.glm.md), [`summ.lm()`](summ.lm.md),
[`summ.merMod()`](summ.merMod.md), [`summ.svyglm()`](summ.svyglm.md)

## Examples

``` r
if (requireNamespace("quantreg")) {
 library(quantreg)
 data(engel)
 fitrq <- rq(income ~ foodexp, data = engel, tau = 0.5)
 summ(fitrq)
}
#> Loading required package: SparseM
#> 
#> Attaching package: ‘SparseM’
#> The following object is masked from ‘package:Matrix’:
#> 
#>     det
#> 
#> Attaching package: ‘quantreg’
#> The following object is masked from ‘package:survival’:
#> 
#>     untangle.specials
#> MODEL INFO:
#> Observations: 235
#> Dependent Variable: income
#> Type: Quantile regression
#>   Quantile (tau): 0.5
#>   Method: Barrodale-Roberts 
#> 
#> MODEL FIT:
#> R¹(0.5) = 0.64 
#> 
#> Standard errors:Sandwich (Huber)
#> --------------------------------------------------
#>                       Est.    S.E.   t val.      p
#> ----------------- -------- ------- -------- ------
#> (Intercept)         -14.96   28.69    -0.52   0.60
#> foodexp               1.55    0.06    26.66   0.00
#> --------------------------------------------------

```
