# Set defaults for `summ()` functions

This function is convenience wrapper for manually setting options using
[`options()`](https://rdrr.io/r/base/options.html). This gives a handy
way to, for instance, set the arguments to be used in every call to
[`summ()`](summ.md) in your script/session.

To make the settings persist across sessions, you can run this in your
`.Rprofile` file.

Note that arguments that do not apply (e.g., `robust` for `merMod`
models) are silently ignored when those types of models are used.

## Usage

``` r
set_summ_defaults(
  digits = NULL,
  model.info = NULL,
  model.fit = NULL,
  model.coefs = NULL,
  pvals = NULL,
  robust = NULL,
  confint = NULL,
  ci.width = NULL,
  vifs = NULL,
  conf.method = NULL,
  table.format = NULL
)
```

## Arguments

- digits:

  An integer specifying the number of digits past the decimal to report
  in the output. Default is 2. You can change the default number of
  digits for all jtools functions with
  `options("jtools-digits" = digits)` where digits is the desired
  number.

- model.info:

  Toggles printing of basic information on sample size, name of DV, and
  number of predictors.

- model.fit:

  Toggles printing of model fit statistics.

- model.coefs:

  Toggles printing of model coefficents.

- pvals:

  Show p values? If `FALSE`, these are not printed. Default is `TRUE`.

- robust:

  If not `FALSE`, reports heteroskedasticity-robust standard errors
  instead of conventional SEs. These are also known as Huber-White
  standard errors. There are several options provided by
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html):
  `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`.

  Default is `FALSE`.

  This requires the `sandwich` package to compute the standard errors.

- confint:

  Show confidence intervals instead of standard errors? Default is
  `FALSE`.

- ci.width:

  A number between 0 and 1 that signifies the width of the desired
  confidence interval. Default is `.95`, which corresponds to a 95%
  confidence interval. Ignored if `confint = FALSE`.

- vifs:

  If `TRUE`, adds a column to output with variance inflation factors
  (VIF). Default is `FALSE`.

- conf.method:

  Argument passed to
  [`lme4::confint.merMod()`](https://rdrr.io/pkg/lme4/man/confint.merMod.html).
  Default is `"Wald"`, but `"profile"` or `"boot"` are better when
  accuracy is a priority. Be aware that both of the alternate methods
  are sometimes very time-consuming.

- table.format:

  A format understood by [`md_table()`](md_table.md)
