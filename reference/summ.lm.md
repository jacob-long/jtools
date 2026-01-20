# Linear regression summaries with options

[`summ()`](summ.md) prints output for a regression model in a fashion
similar to [`summary()`](https://rdrr.io/r/base/summary.html), but
formatted differently with more options.

## Usage

``` r
# S3 method for class 'lm'
summ(
  model,
  scale = FALSE,
  confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", 0.95),
  robust = getOption("summ-robust", FALSE),
  cluster = NULL,
  vifs = getOption("summ-vifs", FALSE),
  digits = getOption("jtools-digits", 2),
  pvals = getOption("summ-pvals", TRUE),
  n.sd = 1,
  center = FALSE,
  transform.response = FALSE,
  scale.only = FALSE,
  data = NULL,
  part.corr = FALSE,
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE),
  model.coefs = getOption("summ-model.coefs", TRUE),
  which.cols = NULL,
  vcov = NULL,
  ...
)
```

## Arguments

- model:

  A `lm` object.

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

- robust:

  If not `FALSE`, reports heteroskedasticity-robust standard errors
  instead of conventional SEs. These are also known as Huber-White
  standard errors. There are several options provided by
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html):
  `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`.

  Default is `FALSE`.

  This requires the `sandwich` package to compute the standard errors.

- cluster:

  For clustered standard errors, provide the column name of the cluster
  variable in the input data frame (as a string). Alternately, provide a
  vector of clusters. Note that you must set `robust` to either "HC1",
  "HC2", or "HC3" in order to have clustered standard errors ("HC4" and
  "HC5" are not supported.

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

- scale.only:

  If you want to scale but not center, set this to `TRUE`. Note that for
  legacy reasons, setting `scale = TRUE` and `center = FALSE` will not
  achieve the same effect. Default is `FALSE`.

- data:

  If you provide the data used to fit the model here, that data frame is
  used to re-fit the model (if `scale` is `TRUE`) instead of the
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html) of
  the model. This is particularly useful if you have variable
  transformations or polynomial terms specified in the formula.

- part.corr:

  Print partial (labeled "partial.r") and semipartial (labeled "part.r")
  correlations with the table? Default is `FALSE`. See details about
  these quantities when robust standard errors are used.

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

- vcov:

  You may provide your own variance-covariance matrix for the regression
  coefficients if you want to calculate standard errors in some way not
  accommodated by the `robust` and `cluster` options.

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

- The R-squared value plus adjusted R-squared

- A table with regression coefficients, standard errors, t-values, and p
  values.

There are several options available for `robust`. The heavy lifting is
done by
[`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html),
where those are better described. Put simply, you may choose from
`"HC0"` to `"HC5"`. Based on the recommendation of the developers of
sandwich, the default is set to `"HC3"`. Stata's default is `"HC1"`, so
that choice may be better if the goal is to replicate Stata's output.
Any option that is understood by `vcovHC()` will be accepted.
Cluster-robust standard errors are computed if `cluster` is set to the
name of the input data's cluster variable or is a vector of clusters.

The `scale` and `center` options are performed via refitting the model
with [`scale_mod()`](scale_mod.md) and [`center_mod()`](center_mod.md),
respectively. Each of those in turn uses [`gscale()`](gscale.md) for the
mean-centering and scaling.

If using `part.corr = TRUE`, then you will get these two common effect
size metrics on the far right two columns of the output table. However,
it should be noted that these do not go hand in hand with robust
standard error estimators. The standard error of the coefficient doesn't
change the point estimate, just the uncertainty. However, this function
uses *t*-statistics in its calculation of the partial and semipartial
correlation. This provides what amounts to a heteroskedasticity-adjusted
set of estimates, but I am unaware of any statistical publication that
validates this type of use. Please use these as a heuristic when used
alongside robust standard errors; do not report the "robust" partial and
semipartial correlations in publications.

## References

King, G., & Roberts, M. E. (2015). How robust standard errors expose
methodological problems they do not fix, and what to do about it.
*Political Analysis*, *23*(2), 159–179.
[doi:10.1093/pan/mpu015](https://doi.org/10.1093/pan/mpu015)

Lumley, T., Diehr, P., Emerson, S., & Chen, L. (2002). The Importance of
the Normality Assumption in Large Public Health Data Sets. *Annual
Review of Public Health*, *23*, 151–169.
[doi:10.1146/annurev.publhealth.23.100901.140546](https://doi.org/10.1146/annurev.publhealth.23.100901.140546)

## See also

[`scale_mod()`](scale_mod.md) can simply perform the standardization if
preferred.

[`gscale()`](gscale.md) does the heavy lifting for mean-centering and
scaling behind the scenes.

Other summ: [`summ.glm()`](summ.glm.md),
[`summ.merMod()`](summ.merMod.md), [`summ.rq()`](summ.rq.md),
[`summ.svyglm()`](summ.svyglm.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
# Create lm object
fit <- lm(Income ~ Frost + Illiteracy + Murder,
          data = as.data.frame(state.x77))

# Print the output with standardized coefficients and 3 digits
summ(fit, scale = TRUE, digits = 3)
#> MODEL INFO:
#> Observations: 50
#> Dependent Variable: Income
#> Type: OLS linear regression 
#> 
#> MODEL FIT:
#> F(3,46) = 4.049, p = 0.012
#> R² = 0.209
#> Adj. R² = 0.157 
#> 
#> Standard errors:OLS
#> -------------------------------------------------------
#>                         Est.      S.E.   t val.       p
#> ----------------- ---------- --------- -------- -------
#> (Intercept)         4435.800    79.773   55.605   0.000
#> Frost                -65.188   109.686   -0.594   0.555
#> Illiteracy          -372.251   129.914   -2.865   0.006
#> Murder                85.179   114.217    0.746   0.460
#> -------------------------------------------------------
#> 
#> Continuous predictors are mean-centered and scaled by 1 s.d. The outcome variable remains in its original units.
```
