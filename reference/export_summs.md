# Export regression summaries to tables

This function allows users to use the features of [`summ()`](summ.md)
(e.g., standardization, robust standard errors) in the context of
shareable HTML, LaTeX, and Microsoft Word tables. It relies heavily on
[`huxtable::huxreg()`](https://hughjonesd.github.io/huxtable/reference/huxreg.html)
to do the table formatting. This is particularly useful for putting the
results of multiple models into a single table.

## Usage

``` r
export_summs(
  ...,
  error_format = "({std.error})",
  error_pos = c("below", "right", "same"),
  ci_level = 0.95,
  statistics = NULL,
  model.names = NULL,
  coefs = NULL,
  to.file = NULL,
  file.name = NULL
)
```

## Arguments

- ...:

  At minimum, a regression object(s). See details for more arguments.

- error_format:

  Which of standard error, confidence intervals, test statistics, or p
  values should be used to express uncertainty of estimates for
  regression coefficients? See details for more info. Default:
  `"({std.error})"`

- error_pos:

  Where should the error statistic defined in `error_style` be placed
  relative to the coefficient estimate? Default: "below"

- ci_level:

  If reporting confidence intervals, what should the confidence level
  be? By default, it is .95 if confidence intervals are requested in
  `error_format`.

- statistics:

  Which model summary statistics should be included? See
  [`huxreg`](https://hughjonesd.github.io/huxtable/reference/huxreg.html)
  for more on usage. The default for this function depends on the model
  type. See details for more on the defaults by model type.

- model.names:

  If you want to give your model(s) names at the top of each column,
  provide them here as a character vector. Otherwise, they will just be
  labeled by number. Default: NULL

- coefs:

  If you want to include only a subset of the coefficients in the table,
  specify them here in a character vector. If you want the table to show
  different names for the coefficients, give a named vector where the
  names are the preferred coefficient names. See details for more.

- to.file:

  Export the table to a Microsoft Word, PDF, or HTML document? This
  functionality relies on `huxtable`'s `quick_` functions
  ([`huxtable::quick_docx()`](https://hughjonesd.github.io/huxtable/reference/quick-output.html),
  [`huxtable::quick_pdf()`](https://hughjonesd.github.io/huxtable/reference/quick-output.html),
  [`huxtable::quick_html()`](https://hughjonesd.github.io/huxtable/reference/quick-output.html),
  [`huxtable::quick_xlsx()`](https://hughjonesd.github.io/huxtable/reference/quick-output.html)).
  Acceptable arguments are "Word" or "docx" (equivalent), "pdf", "html",
  or "xlsx". All are case insensitive. Default is NULL, meaning the
  table is not saved.

- file.name:

  File name with (optionally) file path to save the file. Ignored if
  `to.file` is FALSE. Default: NULL

## Value

A `huxtable`.

## Details

There are many optional parameters not documented above. Any argument
that you would want to pass to [`summ()`](summ.md), for instance, will
be used. Of particular interest may be the robust and scale arguments.
Note that some `summ` arguments may not have any bearing on the table
output.

The default model summary statistics reporting follows this logic:

- summ.lm = `c(N = "nobs", R2 = "r.squared")`,

- summ.glm =
  `` c(N = "nobs", AIC = "AIC", BIC = "BIC", `Pseudo R2` = "pseudo.r.squared") ``,

- summ.svyglm = `c(N = "nobs", R2 = "r.squared")`,

- summ.merMod =
  `` c(N = "nobs", AIC = "AIC", BIC = "BIC", `R2 (fixed)` = "r.squared.fixed", `R2 (total)` = "r.squared") ``

- summ.rq =
  `c(N = "nobs", tau = "tau", R1 = "r.1", AIC = "AIC", BIC = "BIC")`

Be sure to look at the [`summ()`](summ.md) documentation for more on the
calculation of these and other statistics, especially for mixed models.

If you set `statistics = "all"`, then the statistics argument passed to
`huxreg` will be `NULL`, which reports whichever model statistics are
available via `glance`. If you want no model summary statistics, set the
argument to `character(0)`.

You have a few options for the `error_format` argument. You can include
anything returned by
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
(see also [`tidy.summ()`](glance.summ.md)). For the most part, you will
be interested in `std.error` (standard error), `statistic` (test
statistic, e.g. t-value or z-value), `p.value`, or `conf.high` and
`conf.low`, which correspond to the upper and lower bounds of the
confidence interval for the estimate. Note that the default `ci_level`
argument is .95, but you can alter that as desired.

To format the error statistics, simply put the statistics desired in
curly braces wherever you want them in a character string. For example,
if you want the standard error in parentheses, the argument would be
`"({std.error})"`, which is the default. Some other ideas:

- `"({statistic})"`, which gives you the test statistic in parentheses.

- `"({statistic}, p = {p.value})"`, which gives the test statistic
  followed by a "p =" p value all in parentheses. Note that you'll have
  to pay special attention to rounding if you do this to keep cells
  sufficiently narrow.

- `"[{conf.low}, {conf.high}]"`, which gives the confidence interval in
  the standard bracket notation. You could also explicitly write the
  confidence level, e.g., `"CI [{conf.low}, {conf.high}]"`.

For `coefs`, the argument is slightly different than what is default in
`huxreg`. If you provide a named vector of coefficients, then the table
will refer to the selected coefficients by the names of the vector
rather than the coefficient names. For instance, if I want to include
only the coefficients for the `hp` and `mpg` but have the table refer to
them as "Horsepower" and "Miles/gallon", I'd provide the argument like
this: `c("Horsepower" = "hp", "Miles/gallon" = "mpg")`

You can also pass any argument accepted by the
[`huxtable::huxreg()`](https://hughjonesd.github.io/huxtable/reference/huxreg.html)
function. A few that are likely to be oft-used are documented above, but
visit `huxreg`'s documentation for more info.

For info on converting the
[`huxtable::huxtable()`](https://hughjonesd.github.io/huxtable/reference/huxtable.html)
object to HTML or LaTeX, see `huxtable`'s documentation.

## See also

[`summ`](summ.md)

[`huxreg`](https://hughjonesd.github.io/huxtable/reference/huxreg.html)

## Examples

``` r
states <- as.data.frame(state.x77)
fit1 <- lm(Income ~ Frost, data = states)
fit2 <- lm(Income ~ Frost + Illiteracy, data = states)
fit3 <- lm(Income ~ Frost + Illiteracy + Murder, data = states)

if (requireNamespace("huxtable")) {
  # Export all 3 regressions with "Model #" labels,
  # standardized coefficients, and robust standard errors
  export_summs(fit1, fit2, fit3,
               model.names = c("Model 1","Model 2","Model 3"),
               coefs = c("Frost Days" = "Frost",
                         "% Illiterate" = "Illiteracy",
                         "Murder Rate" = "Murder"),
               scale = TRUE, robust = TRUE)
}
#>  ─────────────────────────────────────────────────────────────────────────────
#>                           Model 1            Model 2            Model 3       
#>                     ──────────────────────────────────────────────────────────
#>    Frost Days                  139.04            -75.52            -65.19     
#>                                (94.89)          (138.74)          (149.01)    
#>    % Illiterate                                 -319.31 *         -372.25 **  
#>                                                 (124.83)          (120.00)    
#>    Murder Rate                                                      85.18     
#>                                                                   (136.02)    
#>                     ──────────────────────────────────────────────────────────
#>    N                            50                50                50        
#>    R2                            0.05              0.20              0.21     
#>  ─────────────────────────────────────────────────────────────────────────────
#>    All continuous predictors are mean-centered and scaled by 1                
#>    standard deviation. The outcome variable is in its original                
#>    units. Standard errors are heteroskedasticity robust. *** p <              
#>    0.001; ** p < 0.01; * p < 0.05.                                            
#> 
#> Column names: names, Model 1, Model 2, Model 3
```
