# Test whether sampling weights are needed

Use the DuMouchel-Duncan (1983) test to assess the need for sampling
weights in your linear regression analysis.

## Usage

``` r
wgttest(
  model,
  weights,
  data = NULL,
  model_output = FALSE,
  test = NULL,
  digits = getOption("jtools-digits", default = 3)
)
```

## Arguments

- model:

  The unweighted linear model (must be `lm`, `glm`, see details for
  other types) you want to check.

- weights:

  The name of the weights column in `model`'s data frame or a vector of
  weights equal in length to the number of observations included in
  `model`.

- data:

  The data frame with the data fed to the fitted model and the weights

- model_output:

  Should a summary of the model with weights as predictor be printed?
  Default is FALSE since the output can be very long for complex models.

- test:

  Which type of test should be used in the ANOVA? The default, `NULL`,
  chooses based on the model type ("F" for linear models). This argument
  is passed to `anova`.

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 3. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

## Details

This is designed to be similar to the `wgttest` macro for Stata
(<https://web.archive.org/web/20191112024439/http://fmwww.bc.edu/repec/bocode/w/wgttest.html>).
This method, advocated for by DuMouchel and Duncan (1983), is fairly
straightforward. To decide whether weights are needed, the weights are
added to the linear model as a predictor and interaction with each other
predictor. Then, an omnibus test of significance is performed to compare
the weights-added model to the original; if insignificant, weights are
not significantly related to the result and you can use the more
efficient estimation from unweighted OLS.

It can be helpful to look at the created model using
`model_output = TRUE` to see which variables might be the ones affected
by inclusion of weights.

This test can support most GLMs in addition to LMs, a use validated by
Nordberg (1989). This, to my knowledge, is different from the Stata
macro. It does not work for mixed models (e.g., `lmer` or `lme`) though
it could plausibly be implemented. However, there is no scholarly
consensus how to properly incorporate weights into mixed models. There
are other types of models that may work, but have not been tested. The
function is designed to be compatible with as many model types as
possible, but the user should be careful to make sure s/he understands
whether this type of test is appropriate for the model being considered.
DuMouchel and Duncan (1983) were only thinking about linear regression
when the test was conceived. Nordberg (1989) validated its use with
generalized linear models, but to this author's knowledge it has not
been tested with other model types.

## References

DuMouchel, W. H. & Duncan, D.J. (1983). Using sample survey weights in
multiple regression analyses of stratified samples. *Journal of the
American Statistical Association*, *78*. 535-543.

Nordberg, L. (1989). Generalized linear modeling of sample survey data.
*Journal of Official Statistics; Stockholm*, *5*, 223–239.

Winship, C. & Radbill, L. (1994). Sampling weights and regression
analysis. *Sociological Methods and Research*, *23*, 230-257.

## See also

Other survey tools: [`pf_sv_test()`](pf_sv_test.md),
[`svycor()`](svycor.md), [`svysd()`](svysd.md),
[`weights_tests()`](weights_tests.md)

## Examples

``` r
# First, let's create some fake sampling weights
wts <- runif(50, 0, 5)
# Create model
fit <- lm(Income ~ Frost + Illiteracy + Murder,
          data = as.data.frame(state.x77))
# See if the weights change the model
wgttest(fit, weights = wts)
#> DuMouchel-Duncan test of model change with weights
#> 
#> F(4,42) = 0.283
#> p = 0.887
#> 
#> Lower p values indicate greater influence of the weights.
#> 

# With a GLM
wts <- runif(100, 0, 2)
x <- rnorm(100)
y <- rbinom(100, 1, .5)
fit <- glm(y ~ x, family = binomial)
wgttest(fit, wts)
#> DuMouchel-Duncan test of model change with weights
#> 
#> Deviance (2) = 0.942
#> p = 0.624
#> 
#> Lower p values indicate greater influence of the weights.
#> 
## Can specify test manually
wgttest(fit, weights = wts, test = "Rao")
#> DuMouchel-Duncan test of model change with weights
#> 
#> Rao (2,96) = 0.926
#> p = 0.629
#> 
#> Lower p values indicate greater influence of the weights.
#> 

# Quasi family is treated differently than likelihood-based
## Dobson (1990) Page 93: Randomized Controlled Trial (plus some extra values):
counts <- c(18,17,15,20,10,20,25,13,12,18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,18)
treatment <- gl(3,6)
glm.D93 <- glm(counts ~ outcome + treatment, family = quasipoisson)
wts <- runif(18, 0, 3)
wgttest(glm.D93, weights = wts)
#> DuMouchel-Duncan test of model change with weights
#> 
#> F(5,8) = 2.139
#> p = 0.162
#> 
#> Lower p values indicate greater influence of the weights.
#> 
```
