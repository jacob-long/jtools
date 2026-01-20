# Mixed effects regression summaries with options

[`summ()`](summ.md) prints output for a regression model in a fashion
similar to [`summary()`](https://rdrr.io/r/base/summary.html), but
formatted differently with more options.

## Usage

``` r
# S3 method for class 'merMod'
summ(
  model,
  scale = FALSE,
  confint = getOption("summ-confint", FALSE),
  ci.width = getOption("summ-ci.width", 0.95),
  conf.method = getOption("summ-conf.method", c("Wald", "profile", "boot")),
  digits = getOption("jtools-digits", default = 2),
  r.squared = TRUE,
  pvals = getOption("summ-pvals", NULL),
  n.sd = 1,
  center = FALSE,
  transform.response = FALSE,
  scale.only = FALSE,
  data = NULL,
  exp = FALSE,
  t.df = NULL,
  model.info = getOption("summ-model.info", TRUE),
  model.fit = getOption("summ-model.fit", TRUE),
  model.coefs = getOption("summ-model.coefs", TRUE),
  re.variance = getOption("summ-re.variance", c("sd", "var")),
  which.cols = NULL,
  re.table = getOption("summ-re.table", TRUE),
  groups.table = getOption("summ-groups.table", TRUE),
  ...
)
```

## Arguments

- model:

  A [`merMod`](https://rdrr.io/pkg/lme4/man/merMod-class.html) object.

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

- conf.method:

  Argument passed to
  [`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).
  Default is `"Wald"`, but `"profile"` or `"boot"` are better when
  accuracy is a priority. Be aware that both of the alternate methods
  are sometimes very time-consuming.

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 2. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

- r.squared:

  Calculate an r-squared model fit statistic? Default is `TRUE`, but if
  it has errors or takes a long time to calculate you may want to
  consider setting to FALSE.

- pvals:

  Show p values? If `FALSE`, these are not printed. Default is `TRUE`,
  except for merMod objects (see details).

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

- exp:

  If `TRUE`, reports exponentiated coefficients with confidence
  intervals for exponential models like logit and Poisson models. This
  quantity is known as an odds ratio for binary outcomes and incidence
  rate ratio for count models.

- t.df:

  For `lmerMod` models only. User may set the degrees of freedom used in
  conducting t-tests. See details for options.

- model.info:

  Toggles printing of basic information on sample size, name of DV, and
  number of predictors.

- model.fit:

  Toggles printing of model fit statistics.

- model.coefs:

  Toggles printing of model coefficents.

- re.variance:

  Should random effects variances be expressed in standard deviations or
  variances? Default, to be consistent with previous versions of
  `jtools`, is `"sd"`. Use `"var"` to get the variance instead.

- which.cols:

  Developmental feature. By providing columns by name, you can
  add/remove/reorder requested columns in the output. Not fully
  supported, for now.

- re.table:

  Show table summarizing variance of random effects? Default is TRUE.

- groups.table:

  Show table summarizing the grouping variables? Default is TRUE.

- ...:

  Among other things, arguments are passed to
  [`scale_mod()`](scale_mod.md) or [`center_mod()`](center_mod.md) when
  `center` or `scale` is `TRUE`.

## Value

If saved, users can access most of the items that are returned in the
output (and without rounding).

- coeftable:

  The outputted table of variables and coefficients

- rcoeftable:

  The secondary table with the grouping variables and random
  coefficients.

- gvars:

  The tertiary table with the grouping variables, numbers of groups, and
  ICCs.

- model:

  The model for which statistics are displayed. This would be most
  useful in cases in which `scale = TRUE`.

Much other information can be accessed as attributes.

## Details

By default, this function will print the following items to the console:

- The sample size

- The name of the outcome variable

- The (Pseudo-)R-squared value and AIC/BIC.

- A table with regression coefficients, standard errors, and t-values.

The `scale` and `center` options are performed via refitting the model
with [`scale_mod()`](scale_mod.md) and [`center_mod()`](center_mod.md),
respectively. Each of those in turn uses [`gscale()`](gscale.md) for the
mean-centering and scaling.

`merMod` models are a bit different than the others. The `lme4` package
developers have, for instance, made a decision not to report or compute
p values for [`lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) models.
There are good reasons for this, most notably that the t-values produced
are not "accurate" in the sense of the Type I error rate. For certain
large, balanced samples with many groups, this is no big deal. What's a
"big" or "small" sample? How much balance is necessary? What type of
random effects structure is okay? Good luck getting a statistician to
give you any clear guidelines on this. Some simulation studies have been
done on fewer than 100 observations, so for sure if your sample is
around 100 or fewer you should not interpret the t-values. A large
number of groups is also crucial for avoiding bias using t-values. If
groups are nested or crossed in a linear model, it is best to just get
the pbkrtest package.

By default, this function follows `lme4`'s lead and does not report the
p values for [`lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) models.
If the user has pbkrtest installed, however, p values are reported using
the Kenward-Roger d.f. approximation unless `pvals = FALSE` or `t.df` is
set to something other than `NULL`. In publications, you should cite the
Kenward & Roger (1997) piece as well as either this package or pbkrtest
package to explain how the p values were calculated.

See [`pvalues`](https://rdrr.io/pkg/lme4/man/pvalues.html) from the lme4
for more details. If you're looking for a simple test with no extra
packages installed, it is better to use the confidence intervals and
check to see if they exclude zero than use the t-test. For users of
[`glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html), see some of the
advice there as well. While `lme4` and by association
[`summ()`](summ.md) does as well, they are still imperfect.

You have some options to customize the output in this regard with the
`t.df` argument. If `NULL`, the default, the degrees of freedom used
depends on whether the user has lmerTest or pbkrtest installed. If
`lmerTest` is installed, the degrees of freedom for each coefficient are
calculated using the Satterthwaite method and the p values calculated
accordingly. If only `pbkrtest` is installed or `t.df` is `"k-r"`, the
Kenward-Roger approximation of the standard errors and degrees of
freedom for each coefficient is used. Note that Kenward-Roger standard
errors can take longer to calculate and may cause R to crash with models
fit to large (roughly greater than 5000 rows) datasets.

If neither is installed and the user sets `pvals = TRUE`, then the
residual degrees of freedom is used. If `t.df = "residual"`, then the
residual d.f. is used without a message. If the user prefers to use some
other method to determine the d.f., then any number provided as the
argument will be used.

**About pseudo-R^2**

There is no one way to calculate R^2 for mixed models or nonlinear
models. Many caution against interpreting or even using such
approximations outside of OLS regression. With that said, this package
reports one version for your benefit, though you should of course
understand that it is not an unambiguous measure of model fit.

This package calculates R^2 for mixed models using an adapted version of
`rsquared()` from the `piecewiseSEM` package. This is an implementation
of the Nakagawa & Schielzeth (2013) procedure with refinements by
Johnson (2014). If you choose to report the pseudo-R^2 in a publication,
you should cite Nakagawa & Schielzeth to explain how the calculation was
done.

## References

Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth's
\$R^2_GLMM\$ to random slopes models. *Methods in Ecology and
Evolution*, *5*, 944–946.
[doi:10.1111/2041-210X.12225](https://doi.org/10.1111/2041-210X.12225)

Kenward, M. G., & Roger, J. H. (1997). Small sample inference for fixed
effects from restricted maximum likelihood. *Biometrics*, *53*, 983.
[doi:10.2307/2533558](https://doi.org/10.2307/2533558)

Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017).
lmerTest package: Tests in linear mixed effects models. *Journal of
Statistical Software*, *82*.
[doi:10.18637/jss.v082.i13](https://doi.org/10.18637/jss.v082.i13)

Luke, S. G. (2017). Evaluating significance in linear mixed-effects
models in R. *Behavior Research Methods*, *49*, 1494–1502.
[doi:10.3758/s13428-016-0809-y](https://doi.org/10.3758/s13428-016-0809-y)

Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
obtaining \$R^2\$ from generalized linear mixed-effects models. *Methods
in Ecology and Evolution*, *4*, 133–142.
[doi:10.1111/j.2041-210x.2012.00261.x](https://doi.org/10.1111/j.2041-210x.2012.00261.x)

## See also

[`scale_mod()`](scale_mod.md) can simply perform the standardization if
preferred.

[`gscale()`](gscale.md) does the heavy lifting for mean-centering and
scaling behind the scenes.

[`pbkrtest::get_ddf_Lb()`](https://rdrr.io/pkg/pbkrtest/man/get_ddf_Lb.html)
gets the Kenward-Roger degrees of freedom if you have pbkrtest
installed.

A tweaked version of `piecewiseSEM::rsquared()` is used to generate the
pseudo-R-squared estimates for linear models.

Other summ: [`summ.glm()`](summ.glm.md), [`summ.lm()`](summ.lm.md),
[`summ.rq()`](summ.rq.md), [`summ.svyglm()`](summ.svyglm.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
if (requireNamespace("lme4")) {
  library(lme4, quietly = TRUE)
  data(sleepstudy)
  mv <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

  summ(mv) # Note lack of p values if you don't have lmerTest/pbkrtest

  # Without lmerTest/pbkrtest, you'll get message about Type 1 errors
  summ(mv, pvals = TRUE)

  # To suppress message, manually specify t.df argument
  summ(mv, t.df = "residual")

  # Confidence intervals may be better alternative to p values
  summ(mv, confint = TRUE)
  # Use conf.method to get profile intervals (may be slow to run)
  # summ(mv, confint = TRUE, conf.method = "profile")

}
#> MODEL INFO:
#> Observations: 180
#> Dependent Variable: Reaction
#> Type: Mixed effects linear regression 
#> 
#> MODEL FIT:
#> AIC = 1755.63, BIC = 1774.79
#> Pseudo-R² (fixed effects) = 0.28
#> Pseudo-R² (total) = 0.80 
#> 
#> FIXED EFFECTS:
#> --------------------------------------------------------------------
#>                       Est.     2.5%    97.5%   t val.    d.f.      p
#> ----------------- -------- -------- -------- -------- ------- ------
#> (Intercept)         251.41   238.03   264.78    36.84   17.00   0.00
#> Days                 10.47     7.44    13.50     6.77   17.00   0.00
#> --------------------------------------------------------------------
#> 
#> p values calculated using Satterthwaite d.f.
#> 
#> RANDOM EFFECTS:
#> ------------------------------------
#>   Group      Parameter    Std. Dev. 
#> ---------- ------------- -----------
#>  Subject    (Intercept)     24.74   
#>  Subject       Days         5.92    
#>  Residual                   25.59   
#> ------------------------------------
#> 
#> Grouping variables:
#> ---------------------------
#>   Group    # groups   ICC  
#> --------- ---------- ------
#>  Subject      18      0.48 
#> ---------------------------
```
