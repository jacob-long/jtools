# Test whether sampling weights are needed

Use the test proposed in Pfeffermann and Sverchkov (1999) to check
whether a regression model is specified correctly without weights.

## Usage

``` r
pf_sv_test(
  model,
  data = NULL,
  weights,
  sims = 1000,
  digits = getOption("jtools-digits", default = 3)
)
```

## Arguments

- model:

  The fitted model, without weights

- data:

  The data frame with the data fed to the fitted model and the weights

- weights:

  The name of the weights column in `model`'s data frame or a vector of
  weights equal in length to the number of observations included in
  `model`.

- sims:

  The number of bootstrap simulations to use in estimating the variance
  of the residual correlation. Default is 1000, but for publications or
  when computing power/time is sufficient, a higher number is better.

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 3. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

## Details

This is a test described by Pfeffermann and Sverchkov (1999) that is
designed to help analysts decide whether they need to use sample weights
in their regressions to avoid biased parameter estimation.

It first checks the correlation of the residuals of the model with the
weights. It then uses bootstrapping to estimate the variance of the
correlation, ending with a t-test of whether the correlation differs
from zero. This is done for the squared residuals and cubed residuals as
well. If anyone of them are statistically significant (at whatever level
you feel appropriate), it is best to do a weighted regression. Note that
in large samples, a very small correlation may have a low p-value
without a large bias in the unweighted regression.

## References

Pfeffermann, D., & Sverchkov, M. (1999). Parametric and semi-parametric
estimation of regression models fitted to survey data. *Sankhya: The
Indian Journal of Statistics*, *61*. 166-186.

## See also

Other survey tools: [`svycor()`](svycor.md), [`svysd()`](svysd.md),
[`weights_tests()`](weights_tests.md), [`wgttest()`](wgttest.md)

## Examples

``` r
# Note: This is a contrived example to show how the function works,
# not a case with actual sammpling weights from a survey vendor
if (requireNamespace("boot")) {
  states <- as.data.frame(state.x77)
  set.seed(100)
  states$wts <- runif(50, 0, 3)
  fit <- lm(Murder ~ Illiteracy + Frost, data = states)
  pf_sv_test(model = fit, data = states, weights = wts, sims = 100)
}
#> 
#> Pfeffermann-Sverchkov test of sample weight ignorability 
#> 
#> Residual correlation = -0.157, p = 0.328
#> Squared residual correlation = 0.250, p = 0.108
#> Cubed residual correlation = -0.000, p = 0.998
#> 
#> A significant correlation may indicate biased estimates
#> in the unweighted model.
```
