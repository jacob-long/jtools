# Calculate standard deviations with complex survey data

`svysd` extends the `survey` package by calculating standard deviations
with syntax similar to the original package, which provides only a
[`svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
function.

## Usage

``` r
svysd(
  formula,
  design,
  na.rm = FALSE,
  digits = getOption("jtools-digits", default = 3),
  ...
)
```

## Arguments

- formula:

  A formula (e.g., ~var1+var2) specifying the term(s) of interest.

- design:

  The `survey.design` or `svyrep.design` object.

- na.rm:

  Logical. Should cases with missing values be dropped?

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 3. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

- ...:

  Additional arguments passed to
  [`svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html).

## Details

An alternative is to simply do `sqrt(svyvar(~term, design = design))`.
However, if printing and sharing the output, this may be misleading
since the output will say "variance."

## Note

This function was designed independent of the survey package and is
neither endorsed nor known to its authors.

## See also

[`svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)

Other survey package extensions: [`svycor()`](svycor.md)

Other survey tools: [`pf_sv_test()`](pf_sv_test.md),
[`svycor()`](svycor.md), [`weights_tests()`](weights_tests.md),
[`wgttest()`](wgttest.md)

## Examples

``` r
if (requireNamespace("survey")) {
 library(survey)
 data(api)
 # Create survey design object
 dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat,
                     fpc=~fpc)

 # Print the standard deviation of some variables
 svysd(~api00+ell+meals, design = dstrat)
}
#>       std. dev.
#> api00   123.250
#> ell      21.267
#> meals    29.503
```
