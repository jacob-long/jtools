# Test whether sampling weights are needed

Use the tests proposed in Pfeffermann and Sverchkov (1999) and DuMouchel
and Duncan (1983) to check whether a regression model is specified
correctly without weights.

## Usage

``` r
weights_tests(
  model,
  weights,
  data,
  model_output = TRUE,
  test = NULL,
  sims = 1000,
  digits = getOption("jtools-digits", default = 2)
)
```

## Arguments

- model:

  The fitted model, without weights

- weights:

  The name of the weights column in `model`'s data frame or a vector of
  weights equal in length to the number of observations included in
  `model`.

- data:

  The data frame with the data fed to the fitted model and the weights

- model_output:

  Should a summary of the model with weights as predictor be printed?
  Default is TRUE, but you may not want it if you are trying to
  declutter a document.

- test:

  Which type of test should be used in the ANOVA? The default, `NULL`,
  chooses based on the model type ("F" for linear models). This argument
  is passed to `anova`.

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

This function is a wrapper for the two tests implemented in this package
that test whether your regression model is correctly specified. The
first is [`wgttest`](wgttest.md), an R adaptation of the Stata macro of
the same name. This test can otherwise be referred to as the
DuMouchel-Duncan test. The other test is the Pfeffermann-Sverchkov test,
which can be accessed directly with [`pf_sv_test`](pf_sv_test.md).

For more details on each, visit the documentation on the respective
functions. This function just runs each of them for you.

## References

DuMouchel, W. H. & Duncan, D.J. (1983). Using sample survey weights in
multiple regression analyses of stratified samples. *Journal of the
American Statistical Association*, *78*. 535-543.

Nordberg, L. (1989). Generalized linear modeling of sample survey data.
*Journal of Official Statistics; Stockholm*, *5*, 223-239.

Pfeffermann, D., & Sverchkov, M. (1999). Parametric and semi-parametric
estimation of regression models fitted to survey data. *Sankhya: The
Indian Journal of Statistics*, *61*. 166-186.

## See also

Other survey tools: [`pf_sv_test()`](pf_sv_test.md),
[`svycor()`](svycor.md), [`svysd()`](svysd.md),
[`wgttest()`](wgttest.md)

## Examples

``` r
# Note: This is a contrived example to show how the function works,
# not a case with actual sammpling weights from a survey vendor
if (requireNamespace("boot")) {
  states <- as.data.frame(state.x77)
  set.seed(100)
  states$wts <- runif(50, 0, 3)
  fit <- lm(Murder ~ Illiteracy + Frost, data = states)
  weights_tests(model = fit, data = states, weights = wts, sims = 100)
}
#> DuMouchel-Duncan test of model change with weights
#> 
#> F(3,44) = 0.674
#> p = 0.572
#> 
#> Lower p values indicate greater influence of the weights.
#> 
#> Standard errors:OLS
#> ---------------------------------------------------
#>                         Est.   S.E.   t val.      p
#> -------------------- ------- ------ -------- ------
#> (Intercept)             2.68   4.80     0.56   0.58
#> Illiteracy              5.01   1.93     2.60   0.01
#> wts                     1.01   2.78     0.36   0.72
#> Frost                  -0.00   0.03    -0.15   0.88
#> Illiteracy:wts         -0.96   1.17    -0.83   0.41
#> wts:Frost              -0.00   0.01    -0.24   0.81
#> ---------------------------------------------------
#> 
#> ---
#> Pfeffermann-Sverchkov test of sample weight ignorability 
#> 
#> Residual correlation = -0.16, p = 0.33
#> Squared residual correlation = 0.25, p = 0.11
#> Cubed residual correlation = -0.00, p = 1.00
#> 
#> A significant correlation may indicate biased estimates
#> in the unweighted model.
```
