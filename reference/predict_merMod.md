# Alternative interface for `merMod` predictions

This function generates predictions for `merMod` models, but with the
ability to get standard errors as well.

## Usage

``` r
predict_merMod(
  object,
  newdata = NULL,
  se.fit = FALSE,
  use.re.var = FALSE,
  allow.new.levels = FALSE,
  type = c("link", "response", "terms"),
  na.action = na.pass,
  re.form = NULL,
  boot = FALSE,
  sims = 100,
  prog.arg = "none",
  ...
)
```

## Arguments

- object:

  a fitted model object

- newdata:

  data frame for which to evaluate predictions.

- se.fit:

  Include standard errors with the predictions? Note that these standard
  errors by default include only fixed effects variance. See details for
  more info. Default is FALSE.

- use.re.var:

  If `se.fit` is TRUE, include random effects variance in standard
  errors? Default is FALSE.

- allow.new.levels:

  logical if new levels (or NA values) in `newdata` are allowed. If
  FALSE (default), such new values in `newdata` will trigger an error;
  if TRUE, then the prediction will use the unconditional
  (population-level) values for data with previously unobserved levels
  (or NAs).

- type:

  character string - either `"link"`, the default, or `"response"`
  indicating the type of prediction object returned.

- na.action:

  [`function`](https://rdrr.io/r/base/function.html) determining what
  should be done with missing values for fixed effects in `newdata`. The
  default is to predict `NA`: see
  [`na.pass`](https://rdrr.io/r/stats/na.fail.html).

- re.form:

  (formula, `NULL`, or `NA`) specify which random effects to condition
  on when predicting. If `NULL`, include all random effects; if `NA` or
  `~0`, include no random effects.

- boot:

  Use bootstrapping (via
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html)) to
  estimate variance for `se.fit`? Default is FALSE

- sims:

  If `boot` is TRUE, how many simulations should be run? Default is 100.

- prog.arg:

  If `boot` and `se.fit` are TRUE, a character string - type of progress
  bar to display. Default is "none"; the function will look for a
  relevant \*ProgressBar function, so "txt" will work in general; "tk"
  is available if the tcltk package is loaded; or "win" on Windows
  systems. Progress bars are disabled (with a message) for parallel
  operation.

- ...:

  When `boot` and `se.fit` are TRUE, any additional arguments are passed
  to [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html).

## Details

The developers of lme4 omit an `se.fit` argument for a reason, which is
that it's not perfectly clear how best to estimate the variance for
these models. This solution is a logical one, but perhaps not perfect.
Bayesian models are one way to do better.

The method used here is based on the one described here:
<http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions>
