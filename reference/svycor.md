# Calculate Pearson correlations with complex survey data

`svycor` extends the `survey` package by calculating correlations with
syntax similar to the original package, which for reasons unknown lacks
such a function.

## Usage

``` r
svycor(
  formula,
  design,
  na.rm = FALSE,
  digits = getOption("jtools-digits", default = 2),
  sig.stats = FALSE,
  bootn = 1000,
  mean1 = TRUE,
  ...
)
```

## Arguments

- formula:

  A formula (e.g., ~var1+var2) specifying the terms to correlate.

- design:

  The `survey.design` or `svyrep.design` object.

- na.rm:

  Logical. Should cases with missing values be dropped?

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 2. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

- sig.stats:

  Logical. Perform non-parametric bootstrapping (using
  [`wtd.cor`](https://rdrr.io/pkg/weights/man/wtd.cor.html)) to generate
  standard errors and associated t- and p-values. See details for some
  considerations when doing null hypothesis testing with complex survey
  correlations.

- bootn:

  If `sig.stats` is TRUE, this defines the number of bootstraps to be
  run to generate the standard errors and p-values. For large values and
  large datasets, this can contribute considerably to processing time.

- mean1:

  If `sig.stats` is TRUE, it is important to know whether the sampling
  weights should have a mean of 1. That is, should the standard errors
  be calculated as if the number of rows in your dataset is the total
  number of observations (TRUE) or as if the sum of the weights in your
  dataset is the total number of observations (FALSE)?

- ...:

  Additional arguments passed to
  [`svyvar()`](https://rdrr.io/pkg/survey/man/svyglm.html).

## Value

If significance tests are not requested, there is one returned value:

- cors:

  The correlation matrix (without rounding)

If significance tests are requested, the following are also returned:

- p.values:

  A matrix of p values

- t.values:

  A matrix of t values

- std.err:

  A matrix of standard errors

## Details

This function extends the `survey` package by calculating the
correlations for user-specified variables in survey design and returning
a correlation matrix.

Using the [`wtd.cor`](https://rdrr.io/pkg/weights/man/wtd.cor.html)
function, this function also returns standard errors and p-values for
the correlation terms using a sample-weighted bootstrapping procedure.
While correlations do not require distributional assumptions, hypothesis
testing (i.e., \\r \> 0\\) does. The appropriate way to calculate
standard errors and use them to define a probability is not
straightforward in this scenario since the weighting causes
heteroskedasticity, thereby violating an assumption inherent in the
commonly used methods for converting Pearson's correlations into
t-values. The method provided here is defensible, but if reporting in
scientific publications the method should be spelled out.

## Note

This function was designed in part on the procedure recommended by
Thomas Lumley, the author of the survey package, on [Stack
Overflow](https://stackoverflow.com/questions/34418822/pearson-correlation-coefficient-in-rs-survey-package#41031088).
However, he has not reviewed or endorsed this implementation. All
defects are attributed to the author.

## See also

[`wtd.cor`](https://rdrr.io/pkg/weights/man/wtd.cor.html),
[`svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)

Other survey package extensions: [`svysd()`](svysd.md)

Other survey tools: [`pf_sv_test()`](pf_sv_test.md),
[`svysd()`](svysd.md), [`weights_tests()`](weights_tests.md),
[`wgttest()`](wgttest.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
if (requireNamespace("survey")) {
 library(survey)
 data(api)
 # Create survey design object
 dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                     data = apistrat, fpc = ~fpc)

 # Print correlation matrix
 svycor(~api00 + api99 + dnum, design = dstrat)

 # Save the results, extract correlation matrix
 out <- svycor(~api00 + api99 + dnum, design = dstrat)
 out$cors

}
#>           api00     api99      dnum
#> api00 1.0000000 0.9759047 0.2543484
#> api99 0.9759047 1.0000000 0.2441910
#> dnum  0.2543484 0.2441910 1.0000000
```
