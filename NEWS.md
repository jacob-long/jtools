# jtools 2.0.2

Minor release. 

Fixes:

* Pseudo-R^2 calculations now handle GLMs with start values specified. (#63)
* `effect_plot()` no longer silently ignores the `at =` argument.
* `make_new_data()` no longer tries to calculate control values for variables
included in `data` but not the model.
* A documentation issue was causing errors on CRAN.

Other changes:

* `plot_coefs()` and `plot_summs()` can now accept a list of models as its 
input. (#64)
* `make_predictions()` and functions that depend on it (e.g., `effect_plot()`)
now use `fitted()` to get predicted values from `brmsfit` objects, which 
provide a smoother predicted line as would be expected.

# jtools 2.0.1

Minor release.

Fixes:

* When Satterthwaite or Kenward-Roger degrees of freedom were used to calculate
*p* values for linear `merMod` (i.e., `lmerMod`) models with `summ`, the 
*p* values reported were one-tailed --- half their actual value. *t* statistics
and standard errors were correct.
* When the deprecated `odds.ratio` argument was given to `summ()`, users were 
correctly warned that it is a deprecated argument but the exponentiated 
coefficients were not returned as they should have been.
* Fixed an error in `make_new_data()`/`make_predictions()`/`effect_plot()` when
offsets are specified in a formula or a variable is included more than once
in a formula.
* `make_predictions()` and `partialize()` handle missing data more gracefully,
especially when the original data are a `tibble`.

Other changes:

* Added `%just.list%` and `%not.list%` S3 methods. 
* `%just%` now sorts the matches on the left-hand side in the order they occur
on the right-hand side.
* `summ()` (and `md_table()`) now rely on `pander` to produce plain-text tables
and use `pander`'s `"multiline"` format by default. Check out `"grid"` for 
another option. You can change the default using `table.format` in 
`set_summ_defaults()`.
* `stars` (i.e., significance stars) are no longer available from `summ()`. 
This is partially due to the change to printing tables via `pander` but also
in keeping with statistical best practices. 
* `predict_merMod()`, which is used for generating confidence intervals for 
`merMod` model predictions in `make_predictions()` and `effect_plot()`, is
now a user-accessible function. 
* `stop_wrap()`, `warn_wrap()`, and `msg_wrap()` now interface with 
the `rlang` package equivalents rather than the base `stop()` and so on.
End users may also take advantage of the `rlang` sub-classing abilities through
these functions.
* `summ()` now passes extra arguments to `center_mod()`/`scale_mod()`, allowing
you to use those functions' more advanced options.

# jtools 2.0.0

**Big** changes.

## New spinoff package: `interactions`

To reduce the complexity of this package and help people understand what they 
are getting, I have removed all functions that directly analyze 
interaction/moderation effects and put them into a new package, 
[`interactions`](https://interactions.jacob-long.com).
There are still some functions in `jtools` that support `interactions`, but
some users may find that everything they ever used `jtools` for has now moved 
to `interactions`. The following functions have moved to `interactions`:

* `interact_plot()`
* `cat_plot()`
* `sim_slopes()`
* `johnson_neyman()`
* `probe_interaction()`

Hopefully moving these items to a separate package called `interactions` will
help more people discover those functions and reduce confusion about what both
packages are for. 

## Important changes to `make_predictions()` and removal of `plot_predictions()`

In the `jtools` 1.0.0 release, I introduced `make_predictions()` as a lower-level
way to emulate the functionality of `effect_plot()`, `interact_plot()`, and 
`cat_plot()`. This would return a list object with predicted data, the original 
data, and a bunch of attributes containing information about how to plot it.
One could then take this object, with class `predictions`, and use it as the 
main argument to `plot_predictions()`, which was another new function that 
creates the plots you would see in `effect_plot()` et al.

I have simplified `make_predictions()` to be less specific to those plotting 
functions and eliminated `plot_predictions()`, which was ultimately too complex
to maintain and caused problems for separating the interaction tools into a 
separate package. `make_predictions()` by default simply creates a new data frame
of predicted values along a `pred` variable. It no longer accepts `modx` or 
`mod2` arguments. Instead, it accepts an argument called `at` where a user can
specify any number of variables and values to generate predictions *at*. This
syntax is designed to be similar to the `predictions`/`margins` packages. See
the documentation for more info on this revised syntax. 

`make_new_data()` is a new function that supports `make_predictions()` by creating
the data frame of hypothetical values to which the predictions will be added.

## Generate partial residuals for plotting 

I have added a new function, `partialize()`, that creates partial residuals for
the purposes of plotting (e.g., with `effect_plot()`). One negative when 
visualizing predictions alongside original data with `effect_plot()` or similar
tools is that the observed data may be too spread out to pick up on any 
patterns. However, sometimes your model is controlling for the causes of this
scattering, especially with multilevel models that have random intercepts. 
Partial residuals include the effects of all the controlled-for variables 
and let you see how well your model performs with all of those things accounted
for. 

You can plot partial residuals instead of the observed data in `effect_plot()`
via the argument `partial.residuals = TRUE` or get the data yourself using
`partialize()`. It is also integrated into `make_predictions()`.

## New programming helpers

In keeping with the "tools" focus of this package, I am making available some
of the programming tools that previously had only been used internally inside 
the `jtools` package. 

### `%nin%`, `%not%`, and `%just%`

Many are familiar with how handy the `%in%` operator is, but sometimes we want
everything *except* the values in some object. In other words, we might want
`!(x %in% y)` instead of `x %in% y`. This is where `%nin%` ("not in") acts as a
useful shortcut. Now, instead of `!(x %in% y)`, you can just use `x %nin% y`.
Note that the actual implementation of `%nin%` is slightly different to produce
the same results but more quickly for large data. You may run into some other
packages that also have a `%nin%` function and they are, to my knowledge,
functionally the same.

One of my most common uses of both %in% and %nin% is when I want to subset
an object. For instance, assume `x` is 1 through 5, y is 3 through 7, and I 
want only the instances of `x` that are not in `y`. Using `%nin%`, I would write
`x[x %nin% y]`, which leaves you with 1 and 2.
I really don't like having to write the object's name twice
in a row like that, so I created something to simplify further: `%not%`. 
You can now subset `x` to only the parts that are not in `y` like this:
`x %not% y`. Conversely, you can do the equivalent of `x[x %in% y]` using the
`%just%` operator: `x %just% y`. 

As special cases for `%not%` and `%just%`, if the left-hand side is a matrix
or data frame, it is assumed that the right hand side are column indices (if 
numeric) or column names (if character). For example, if I do 
`mtcars %just% c("mpg", "qsec")`, I get a data frame that is just the "mpg" and
"qsec" columns of `mtcars`. It is an S3 method so support can be added for 
additional object types by other developers. 

### `wrap_str()`, `msg_wrap()`, `warn_wrap()`, and `stop_wrap()`

An irritation when writing messages/warnings/errors to users is breaking up the
long strings without unwanted line breaks in the output. One problem is not 
knowing how wide the user's console is. `wrap_str()` takes any string and inserts
line breaks at whatever the "width" option is set to, which automatically
changes according to the actual width in RStudio and in some other setups. 
This means you can write the error message in a single string across multiple,
perhaps indented, lines without those line breaks and indentations being part 
of the console output. `msg_wrap()`, `warn_wrap()`, and `stop_wrap()` are
`wrap_str()` wrappers (pun not intended) around `message()`, `warning()`, and
`stop()`, respectively.

### Other changes

* `summ()` no longer prints coefficient tables as data frames because this 
caused RStudio notebook users issues with the output not being printed to the
console and having the notebook format them in less-than-ideal ways. The tables
now have a markdown format that might remind you of Stata's coefficient tables.
* The function that prints those tables mentioned above is called `md_table()` 
and can be used by others if they want. It is based on `knitr`'s `kable` 
function.
* `summ()` no longer prints significance stars by default. This can be enabled
with the `stars = TRUE` argument or by setting the `"summ-stars"` option to 
`TRUE` (also available via `set_summ_defaults`)
* The `model.check` argument in `summ()` has been removed.
* A function called `get_colors` is now available to users. It retrieves 
the color palettes used in `jtools` functions.
* Plots made by `jtools` now have a new theme, which you can use yourself, 
called `theme_nice()`. The previous default, `theme_apa()`, is still available
but I don't like it as a default since I don't think the APA has defined the 
nicest-looking design guidelines for general use.
* `effect_plot()` now can plot categorical predictors, picking up a functionality
previously provided by `cat_plot()`.
* `effect_plot()` now uses *tidy evaluation* for the `pred` argument (#37).
This means you can pass a variable that contains the name of `pred`, which is
most useful if you are creating a function, for loop, etc. If using a variable,
put a  `!!` from the `rlang` package before it (e.g., `pred = !! variable`). 
For most users, these changes will not affect their usage.

### Bugfixes

* `make_predictions()` (and by extension `effect_plot()` and plotting functions in 
the `interactions` package) now understands dependent variable transformations
better. For instance, there shouldn't be issues if your response variable is 
`log(y)` instead of `y`. When returning the original data frame, these functions
will append a transformed (e.g., `log(y)`) column as needed. 
* `lme4` has a bug when generating predictions in models with offsets --- it 
ignores them when the offset is specified via the `offset =` argument. I have
created a workaround for this.

# jtools 1.1.1

This is a minor release.

## Bug fixes

* `plot_predictions()` had an incorrect default value for `interval`, causing
an error if you used the default arguments with `make_predictions()`. The default
is now `FALSE`. (#39)
* `interact_plot()`, `cat_plot()`, and `effect_plot()` would have errors when the
models included covariates (not involved in the interaction, if any) that
were non-numeric. That has been corrected. (#41)
* Logical variables (with values of `TRUE` or `FALSE`) were not handled by
the plotting functions appropriately, causing them to be treated as numeric.
They are now preserved as logical. (#40).
* `sim_slopes()` gave inaccurate results when factor moderators did not have
treatment coding (`"contr.treatment"`) but are now recoded to treatment 
coding.

## Other changes

* `summ()` output in RMarkdown documents is now powered by `kableExtra`, which
(in my opinion) offers more attractive HTML output and seems to have better
luck with float placement in PDF documents. Your mileage may vary.
* 2 vignettes are now made with `rmdformats` rather than the base `rmarkdown`
template.
* S3 methods for S3 generics that aren't imported by the package (`tidy` and
`glance` from `broom`, `knit_print` from `knitr`, `as_huxtable` from `huxtable`)
will now have conditional namespace registration for users of R 3.6. This 
shouldn't have much effect on end users.

# jtools 1.1.0

This release was initially intended to be a bugfix release, but enough other
things came up to make it a minor release.

## Bug fixes
* Suppressed a number of warning messages caused by a `broom` update 
when using `export_summs()` and `plot_coefs()`.
* Fixed an error with `plot_coefs()` arising from the latest update to `ggplot2`.
* Fixed a new bug introduced in 1.0.0 wherein the points of weighted data
were not being sized according to their weight.
* Fixed an issue with pseudo-R^2 calculation in non-interactive use. [#34]
* Pseudo-R^2 is now included in the `export_summs()` output for `glm` models.
[#36]
* `interact_plot()` no longer errors if there are missing observations in 
the original data and quantiles are requested.

## New features
* For `summ.merMod`, the default p-value calculation is now via the 
Satterthwaite method if you have `lmerTest` installed. The old default,
Kenward-Roger, is used by request or when `pbkrtest` is installed but not
`lmerTest`. It now calculates a different degrees of freedom for each 
predictor and also calculates a variance-covariance matrix for the model,
meaning the standard errors are adjusted as well. It is not the default
largely because the computation takes too long for too many models.
* `johnson_neyman()` now allows you to specify your own critical *t* value
if you are using some alternate method to calculate it. 
* `johnson_neyman()` now allows you to specify the range of moderator values
you want to plot as well as setting a title. 
* Users may now label values in `sim_slopes()` in a way similar to 
`interact_plot()`. [#35]
* Users may provide their own labels for preset moderator values with
`interact_plot()` (e.g., when `modx.values = "plus-minus"`). [#31]
* `plot_coefs()`/`plot_summs()` now supports facetting the coefficients
based on user-specified groupings. See `?plot_summs` for details.
* All `summ()` variants now have pretty output in RMarkdown documents if you 
have the `huxtable` package installed. This can be disabled with the chunk
option `render = 'normal_print'`.

## Interface changes
* All interaction functions now use `modx.values`, `mod2.values`, and 
`pred.values` in place of `modxvals`, `mod2vals`, and `predvals`. Don't
go running to change your code, though; those old argument names will 
still work, but these new ones are clearer and preferred in new code.

## New functions
* There is now a `plot()` method for `sim_slopes` objects. Just save
your `sim_slopes()` call to an object and call the `plot()` function on that
object to see what happens. Basically, it's `plot_coefs()` for `sim_slopes()`.
* For those who have `huxtable` installed, you can now call `as_huxtable`
on a `sim_slopes()` object to get a publication-style table. The interface
is comparable to `export_summs()`.



# jtools 1.0.0

## Major release

This release has several big changes embedded within, side projects that needed
a lot of work to implement and required some user-facing changes. Overall
these are improvements, but in some edge cases they could break old code.
The following sections are divided by the affected functions. Some of the 
functions are discussed in more than one section.

### `interact_plot()`, `cat_plot()`, and `effect_plot()`

These functions no longer re-fit the inputted model to center covariates,
impose labels on factors, and so on. This generally has several key positives,
including

* Major speed gains (15% faster for small `lm` models, 60% for `svyglm`,
and 80% for `merMod` in my testing). The speed gains increase as the models
become more complicated and the source data become larger.
* More model types are supported. In the past, some models failed because
the update method was not defined correctly or there was more information needed
to refit the model than what can be provided by these functions.
* More complicated formula input is supported, with a caveat. If you have,
for instance, log-transformed a predictor (with `log`) in the formula, 
the function would previously would have a lot of trouble and usually 
have errors. Now this is supported, provided you input the data used to fit
the model via the `data` argument. You'll receive a warning if the function
thinks this is needed to work right.

As noted, there is a new `data` argument for these functions. You do not 
normally need to use this if your model is fit with a `y ~ x + z` type of
formula. But if you start doing things like `y ~ factor(x) + z`, then 
you need to provide the source data frame. Another benefit is that this 
allows for fitting polynomials with `effect_plot()` or even interactions with
polynomials with `interact_plot()`. For instance, if my model was fit using
this kind of formula --- `y ~ poly(x, 2) + z` --- I could then plot the 
predicted curve with `effect_plot(fit, pred = x, data = data)` substituting
`fit` with whatever my model is called and `data` with whatever data frame
I used is called.

There are some possible drawbacks for these changes. One is that no longer are 
factor predictors supported in `interact_plot()` and `effect_plot()`,
even two-level ones. This worked before by coercing
them to 0/1 continuous variables and re-fitting the model. Since the model is
no longer re-fit, this can't be done. To work around it, either transform the
predictor to numeric before fitting the model or use `cat_plot()`. Relatedly,
two-level factor covariates are no longer centered and are simply
set to their reference value.

**Robust confidence intervals**: Plotting robust standard errors for compatible
models (tested on `lm`, `glm`). Just use the `robust` argument like you would 
for `sim_slopes()` or `summ()`.

**Preliminary support for confidence intervals for `merMod` models**: You may 
now get confidence intervals when using `merMod` objects as input to the
plotting functions. Of importance, though, is the uncertainty is *only for
the fixed effects*. For now, a warning is printed. See the next section for
another option for `merMod` confidence intervals.

**Rug plots in the margins**: So-called "rug" plots can be included in the 
margins of the plots for any of these functions. These show tick marks for 
each of the observed data points, giving a non-obtrusive impression of the 
distribution of the `pred` variable and (optionally) the dependent variable.
See the documentation for `interact_plot()` and `effect_plot()` and the 
`rug`/`rug.sides` arguments.

**Facet by the `modx` variable**: Some prefer to visualize the predicted lines
on separate panes, so that is now an option available via the `facet.modx` 
argument. You can also use `plot.points` with this, though the division into
groups is not straightforward is the moderator isn't a factor. See the 
documentation for more on how that is done.

### `make_predictions()` and `plot_predictions()`: New tools for advanced plotting 

To let users have some more flexibility, `jtools` now lets users directly 
access the (previously internal) functions that make `effect_plot()`, `cat_plot()`,
and `interact_plot()` work. This should make it easier to tailor the
outputs for specific needs. Some features may be implemented for these functions
only to keep the `_plot` functions from getting any more complicated than they
already are.

The simplest use of the two functions is to use `make_predictions()` just like 
you would `effect_plot()`/`interact_plot()`/`cat_plot()`. The difference is, of
course, that `make_predictions()` only makes the *data* that would be used for
plotting. The resulting `predictions` object has both the predicted and original
data as well as some attributes describing the arguments used. If you pass
this object to `plot_predictions()` with no further arguments, it should do 
exactly what the corresponding `_plot` function would do. However, you might 
want to do something entirely different using the predicted data which is part
of the reason these functions are separate.

One such feature specific to `make_predictions()` is **bootstrap confidence
intervals for `merMod` models**.

### All interaction tools

You may no longer use these tools to scale the models. Use `scale_mod()`, save
the resulting object, and use that as your input to the functions if you want
scaling.

All these tools have a new default `centered` argument. They are now set to
`centered = "all"`, but `"all"` no longer means what it used to. Now it refers
to *all variables not included in the interaction, including the dependent
variable*. This means that in effect, the default option does the same thing
that previous versions did. But instead of having that occur when 
`centered = NULL`, that's what `centered = "all"` means. There is no 
`NULL` option any longer. Note that with `sim_slopes()`, the focal predictor
(`pred`) will now be centered --- this only affects the conditional intercept.

### `sim_slopes()`

This function now supports categorical (factor) moderators, though there is
no option for Johnson-Neyman intervals in these cases. You can use the 
significance of the interaction term(s) for inference about whether the slopes
differ at each level of the factor when the moderator is a factor.

You may now also pass arguments to `summ()`, which is used internally to calculate
standard errors, p values, etc. This is particularly useful if you are using
a `merMod` model for which the `pbkrtest`-based p value calculation is too 
time-consuming.

### `gscale()`

The interface has been changed slightly, with the actual numbers always provided
as the `data` argument. There is no `x` argument and instead a `vars` argument
to which you can provide variable names. The upshot is that it now fits much 
better into a piping workflow. 

The entire function has gotten an extensive reworking, which in some cases 
should result in significant speed gains. And if that's not enough, just know
that the code was an absolute monstrosity before and now it's not.

There are two new functions that are wrappers around `gscale()`: `standardize()`
and `center()`, which call `gscale()` but with `n.sd = 1` in the first case and
with `center.only = TRUE` in the latter case.

### `summ()`

Tired of specifying your preferred configuration every time you use `summ()`?
Now, many arguments will by default check your options so you can set your
own defaults. See `?set_summ_defaults` for more info.

Rather than having separate `scale.response` and `center.response` arguments,
each `summ()` function now uses `transform.response` to collectively cover those
bases. Whether the response is centered or scaled depends on the `scale` and
`center` arguments.

The `robust.type` argument is deprecated. Now, provide the type of robust 
estimator directly to `robust`. For now, if `robust = TRUE`, it defaults to
`"HC3"` with a warning. Better is to provide the argument directly, e.g.,
`robust = "HC3"`. `robust = FALSE` is still fine for using OLS/MLE standard
errors.

Whereas `summ.glm`, `summ.svyglm`, and `summ.merMod` previously offered an
`odds.ratio` argument, that has been renamed to `exp` (short for exponentiate)
to better express the quantity.

`vifs` now works when there are factor variables in the model.

One of the first bugs `summ()` ever had occurred when the function was given
a rank-deficient model. It is not straightforward to detect, especially since
I need to make a space for an almost empty row in the outputted table. At long
last, this release can handle such models gracefully.

Like the rest of R, when `summ()` rounded your output, items rounded exactly to
zero would be treated as, well, zero. But this can be misleading if the 
original value was actually negative. For instance, if `digits = 2` and a 
coefficient was `-0.003`, the value printed to the console was `0.00`, 
suggesting a zero or slightly positive value when in fact it was the 
opposite. This is a limitation of the `round` (and `trunc`) function. I've 
now changed it so the zero-rounded value retains its sign.

`summ.merMod` now calculates pseudo-R^2 much, much faster. For only modestly
complex models, the speed-up is roughly 50x faster. Because of how much faster
it now is and how much less frequently it throws errors or prints cryptic 
messages, it is now calculated by default. The confidence interval calculation
is now "Wald" for these models (see `confint.merMod` for details) rather than
"profile", which for many models can take a very long time and sometimes does
not work at all. This can be toggled with the `conf.method` argument.

`summ.glm`/`summ.svyglm` now will calculate pseudo-R^2 for quasibinomial and
quasipoisson families using the value obtained from refitting them as
binomial/poisson. For now, I'm not touching AIC/BIC for such models 
because the underlying theory is a bit different and the implementation
more challenging.

`summ.lm` now uses the *t*-distribution for finding critical values for 
confidence intervals. Previously, a normal approximation was used.

The `summ.default` method has been removed. It was becoming an absolute terror
to maintain and I doubted anyone found it useful. It's hard to provide the
value added for models of a type that I do not know (robust errors don't 
always apply, scaling doesn't always work, model fit statistics may not make
sense, etc.). Bug me if this has really upset things for you.

One new model type has been supported: `rq` models from the `quantreg` package.
Please feel free to provide feedback for the output and support of these models.

### `scale_lm()` and `center_lm()` are now `scale_mod()`/`center_mod()`

To better reflect the capabilities of these functions (not restricted to `lm`
objects), they have been renamed. The old names will continue to work to 
preserve old code.

However, `scale.response` and `center.response` now default to `FALSE` to
reflect the fact that only OLS models can support transformations of the
dependent variable in that way.

There is a new `vars =` argument for `scale_mod()` that allows you to only apply
scaling to whichever variables are included in that character vector.

I've also implemented a neat technical fix that allows the updated model to
itself be updated while not also including the actual raw data in the model
call.

### `plot_coefs()` and `plot_summs()`

A variety of fixes and optimizations have been added to these functions. 
Now, by default, there are two confidence intervals plotted, a thick line
representing (with default settings) the 90% interval and a thinner line
for the 95% intervals. You can set `inner_ci_level` to `NULL` to get rid of
the thicker line.

With `plot_summs()`, you can also set per-model `summ()` arguments by providing
the argument as a vector (e.g., `robust = c(TRUE, FALSE)`). Length 1 arguments
are applied to all models. `plot_summs()` will now also support models not 
accepted by `summ()` by just passing those models to `plot_coefs()` without
using `summ()` on them.

Another new option is `point.shape`, similar to the model plotting functions.
This is most useful for when you are planning to distribute your output in
grayscale or to colorblind audiences (although the new default color scheme is
meant to be colorblind friendly, it is always best to use another visual cue).

The coolest is the new `plot.distributions` argument, which if TRUE will plot
normal distributions to even better convey the uncertainty. Of course, you
should use this judiciously if your modeling or estimation approach doesn't
produce coefficient estimates that are asymptotically normally distributed.
Inspiration comes from https://twitter.com/BenJamesEdwards/status/979751070254747650.

Minor fixes: `broom`'s interface for Bayesian methods is inconsistent, so I've
hacked together a few tweaks to make `brmsfit` and `stanreg` models work with
`plot_coefs()`.

You'll also notice vertical gridlines on the plots, which I think/hope will
be useful. They are easily removable (see `drop_x_gridlines()`) with ggplot2's
built-in theming options.

### `export_summs()`

Changes here are not too major. Like `plot_summs()`, you can now provide
unsupported model types to `export_summs()` and they are just passed through
to `huxreg`. You can also provide different arguments to `summ()` on a per-model
basis in the way described under the `plot_summs()` heading above.

There are some tweaks to the model info (provided by `glance`). Most prominent
is for `merMod` models, for which there is now a separate N for each grouping
factor.

### `theme_apa()` plus new functions `add_gridlines()`, `drop_gridlines()`

New arguments have been added to `theme_apa()`: `remove.x.gridlines` and 
`remove.y.gridlines`, both of which are `TRUE` by default. APA hates giving
hard and fast rules, but the norm is that gridlines should be omitted unless
they are crucial for interpretation. `theme_apa()` is also now a "complete" 
theme, which means specifying further options via `theme` will not revert
`theme_apa()`'s changes to the base theme. 

Behind the scenes the helper functions `add_gridlines()` and `drop_gridlines()`
are used, which do what they sound like they do. To avoid using the arguments
to those functions, you can also use `add_x_gridlines()`/`add_y_gridlines()` or
`drop_x_gridlines()`/`drop_y_gridlines()` which are wrappers around the 
more general functions.

### Survey tools

`weights_tests()` --- `wgttest()` and `pf_sv_test()` --- now handle missing data
in a more sensible and consistent way.

### Colors

There is a new default qualitative palette, based on Color Universal Design 
(designed to  be readable by the colorblind) that looks great to all. There are
several other new palette choices as well. These are all documented at 
`?jtools_colors`

### Other stuff

Using the `crayon` package as a backend, console output is now formatted
for most `jtools` functions for better readability on supported systems.
Feedback on this is welcome since this might look better or worse in
certain editors/setups.

# jtools 0.9.4 (CRAN release)

This release is limited to dealing with the `huxtable` package's temporary
removal from CRAN, which in turn makes this package out of compliance with
CRAN policies regarding dependencies on non-CRAN packages.

Look out for `jtools` 1.0.0 coming very soon!

# jtools 0.9.3 (CRAN release)

Bugfixes:

* `johnson_neyman()` and `sim_slopes()` were both encountering errors with 
`merMod` input. Thanks to Seongho Bae for reporting these issues and testing
out development versions. 
* An upcoming version of R will change a common warning to an error, causing
a need to change the internals of `gscale`. 
* The default model names in `export_summs()` had an extra space (e.g., `( 1)`) 
due to changes in `huxtable`. The defaults are now just single numbers.

# jtools 0.9.2 

Bugfix:

* Johnson-Neyman plots misreported the alpha level if `control.fdr` was `TRUE`.
It was reporting `alpha * 2` in the legend, but now it is accurate again.

Feature update:

* `johnson_neyman()` now handles multilevel models from `lme4`. 

# jtools 0.9.1 (CRAN release)

Bugfix update:

Jonas Kunst helpfully pointed out some odd behavior of `interact_plot()` with 
factor moderators. No longer should there be occasions in which you have two
different legends appear. The linetype and colors also should now be consistent
whether there is a second moderator or not. For continuous moderators, the 
darkest line should also be a solid line and it is by default the highest 
value of the moderator.

Other fixes:

* An update to `huxtable` broke `export_summs()`, but that has been fixed.

Feature updates:

* You can now manually provide colors to `interact_plot()` and `cat_plot()` by 
providing a vector of colors (any format that `ggplot2` accepts) for the 
`color.class` argument.
* Noah Greifer wrote up a tweak to `summ()` that formats the output in a way
that lines up the decimal points. It looks great.

# jtools 0.9.0 (CRAN release)

This may be the single biggest update yet. If you downloaded from CRAN, be sure
to check the 0.8.1 update as well.

New features are organized by function.

`johnson_neyman()`:

* A new `control.fdr` option is added to control the false discovery rate, 
building on new research. This makes the test more conservative but less likely 
to be a Type 1 error.
* A `line.thickness` argument has been added after Heidi Jacobs pointed out 
that it cannot be changed after the fact.
* The construction of the multiple plots when using `sim_slopes()` for 3-way
interactions is much-improved.
* The critical test statistic used by default has been slightly altered. It
previously used a normal approximation; i.e., if `alpha = .05` the critical
test statistic was always 1.96. Now, the residual degrees of freedom are used
with the t distribution. You can do it the old way by setting `df = "normal"`
or any arbitrary number.

`interact_plot()`: 

* More improvements to `plot.points` (see 0.8.1 for more). You can now plot
observed data with 3-way interactions.
* Another pre-set `modxvals` and `mod2vals` specification has been added:
`"terciles"`. This splits the observed data into 3 equally sized groups and 
chooses as values the mean of each of those groups. This is especially good
for skewed data and for second moderators.
* A new `linearity.check` option for two-way interactions. This facets by each
level of the moderator and lets you compare the fitted line with a loess 
smoothed line to ensure that the interaction effect is roughly linear at each
level of the (continuous) moderator. 
* When the model used weights, like survey sampling weights, the observed data
points are resized according to the observation's weight when 
`plot.points = TRUE`.
* New `jitter` argument added for those using `plot.points`. If you don't want 
the points jittered, you can set `jitter = 0`. If you want more or less, you
can play with the value until it looks right. This applies to `effect_plot()` as
well.

`summ()`:

* Users are now informed why the function is taking so long if `r.squared`
or `pbkrtest` are slowing things down. `r.squared` is now set to FALSE by 
default.

New functions!

`plot_summs()`: A graphic counterpart to `export_summs()`, which was introduced in
the 0.8.0 release. This plots regression coefficients to help in visualizing
the uncertainty of each estimate and facilitates the plotting of nested models
alongside each other for comparison. This allows you to use `summ()` features 
like robust standard errors and scaling with this type of plot that you could
otherwise create with some other packages.

`plot_coefs()`: Just like `plot_summs()`, but no special `summ()` features. This 
allows you to use models unsupported by `summ()`, however, and you can provide
`summ()` objects to plot the same model with different `summ()` argument alongside
each other.

`cat_plot()`: This was a long time coming. It is a complementary function to 
`interact_plot()`, but is designed to deal with interactions between 
categorical variables. You can use bar plots, line plots, dot plots, and
box and whisker plots to do so. You can also use the function to plot the effect
of a single categorical predictor without an interaction.

# jtools 0.8.1

Thanks to Kim Henry who reported a bug with `johnson_neyman()` in the case that
there is an interval, but the entire interval is outside of the plotted area:
When that happened, the legend wrongly stated the plotted line was 
non-significant.

Besides that bugfix, some new features:

* When `johnson_neyman()` fails to find the interval (because it doesn't exist),
it no longer quits with an error. The output will just state the interval was
not found and the plot will still be created.
* Much better support for plotting observed data in `interact_plot()` has been 
added. Previously, if the moderator was a factor, you would get very nicely
colored plotted points when using `plot.points = TRUE`. But if the moderator
was continuous, the points were just black and it wasn't very informative beyond
examining the main effect of the focal predictor. With this update, the
plotted points for continuous moderators are shaded along a gradient that matches
the colors used for the predicted lines and confidence intervals.

# jtools 0.8.0 (CRAN release)

Not many user-facing changes since 0.7.4, but major refactoring internally
should speed things up and make future development smoother.

# jtools 0.7.4

Bugfixes:

* `interact_plot()` and `effect_plot()` would trip up when one of the focal
predictors had a name that was a subset of a covariate (e.g., pred = "var" 
but a covariate is called "var_2"). That's fixed.
* Confidence intervals for `merMod` objects were not respecting the 
user-requested confidence level and that has been fixed.
* Confidence intervals for `merMod` objects were throwing a
spurious warning on R 3.4.2.
* `interact_plot()` was mis-ordering secondary moderators. That has been fixed.
* `export_summs()` had a major performance problem when providing extra arguments
which may have also caused it to wrongly ignore some arguments. That has been 
fixed and it is much faster.

Enhancements:
* `interact_plot()` now gives more informative labels for secondary moderators 
when the user has defined the values but not the labels.
* confidence intervals are now properly supported with `export_summs()`
* changes made to `export_summs()` for compatibility with huxtable 1.0.0 changes

# jtools 0.7.3 (CRAN release)

Important bugfix:

* When standardize was set to TRUE using `summ()`, the model was not 
mean-centered as the output stated. This has been fixed. I truly regret the
error---double-check any analyses you may have run with this feature.

New function: `export_summs()`. 

This function outputs regression models supported
by `summ()` in table formats useful for RMarkdown output as well as specific 
options for exporting to Microsoft Word files. This is particularly helpful for
those wanting an efficient way to export regressions that are standardized 
and/or use robust standard errors. 


# jtools 0.7.2

The documentation for `j_summ()` has been reorganized such that each supported
model type has its own, separate documentation. `?j_summ` will now just give you
links to each supported model type.

More importantly, `j_summ()` will from now on be referred to as, simply, 
`summ()`. Your old code is fine; `j_summ()` will now be an alias for `summ()`
and will run the same underlying code. Documentation will refer to the `summ()`
function, though. That includes the updated vignette.

One new feature for `summ.lm`:

* With the `part.corr = TRUE` argument for a linear model, partial and 
semipartial correlations for each variable are reported.

More tweaks to `summ.merMod`:

* Default behavior with regard to p values depends on model type (`lmer()` vs.
`glmer()`/`nlmer()`) and, in the case of linear models, whether the `pbkrtest`
package is installed. If it is, p values are calculated based on the 
Kenward-Roger degrees of freedom calculation and printed. Otherwise, p values 
are not shown by default with `lmer()` models. p values are shown with `glmer()`
models, since that is also the default behavior of `lme4`.
* There is an `r.squared` option, which for now is FALSE by default. It adds
runtime since it must fit a null model for comparison and sometimes this also
causes convergence issues.

# jtools 0.7.1

Returning to CRAN!

A very strange bug on CRAN's servers was causing jtools updates to silently fail
when I submitted updates; I'd get a confirmation that it passed all tests, but
a LaTeX error related to an Indian journal I cited was torpedoing it before
it reached CRAN servers.

The only change from 0.7.0 is fixing that problem, but if you're a CRAN user
you will want to flip through the past several releases as well to see what 
you've missed.

# jtools 0.7.0

New features:

* `j_summ()` can now provide cluster-robust standard errors for lm models.
* `j_summ()` output now gives info about missing observations for supported models.
* At long last, `j_summ()`/`scale_lm()`/`center_lm()` can standardize/center 
models with logged terms and other functions applied.
* `interact_plot()` and `effect_plot()` will now also support predictors that 
have functions applied to them.
* `j_summ()` now supports confidence intervals at user-specified widths.
* `j_summ()` now allows users to not display p-values if requested.
* I've added a warning to `j_summ()` output with merMod objects, since it 
provides p-values calculated on the basis of the estimated t-values. These are
not to be interpreted in the same way that OLS and GLM p-values are, since with 
smaller samples mixed model t-values will give inflated Type I error rates.
* By default, `j_summ()` will not show p-values for `merMod` objects.

Bug fix:

* `scale_lm()` did not have its center argument implemented and did not 
explain the option well in its documentation.
* `johnson_neyman()` got confused when a factor variable was given as a predictor

# jtools 0.6.1

Bug fix release:

* `wgttest()` acted in a way that might be unexpected when providing a weights
variable name but no data argument. Now it should work as expected by getting
the data frame from the model call.
* `gscale()` had a few situations in which it choked on missing data, especially
when weights were used. This in turn affected `j_summ()`, `scale_lm()`, and
`center_lm()`, which each rely on `gscale()` for standardization and 
mean-centering. That's fixed now.
* `gscale()` wasn't playing nicely with binary factors in survey designs, 
rendering the scaling incorrect. If you saw a warning, re-check your outputs
after this update.

# jtools 0.6.0

A lot of changes!

New functions:

* `effect_plot()`: If you like the visualization of moderation effects from 
`interact_plot()`, then you should enjoy `effect_plot()`. It is a clone of 
`interact_plot()`, but shows a single regression line rather than several. It
supports GLMs and lme4 models and can plot original, observed data points.
* `pf_sv_test()`: Another tool for survey researchers to test whether it's okay
to run unweighted regressions. Named after Pfeffermann and Sverchkov, who
devised the test.
* `weights_tests()`: Like `probe_interaction()` does for the interaction 
functions, `weights_tests()` will run the new `pf_sv_test()` as well as 
`wgttest()` simultaneously with a common set of arguments. 

Enhancements:

* Set a default number of digits to print for all jtools functions with the
option `"jtools-digits"`. 
* `wgttest()` now accepts and tests GLMs and may work for other regression 
models.


Bug fixes:

* `j_summ()` would print significance stars based on the rounded p value, 
sometimes resulting in misleading output. Now significance stars are based on
the non-rounded p values.
* `probe_interaction()` did not pass an "alpha" argument to `sim_slopes()`,
possibly confusing users of `johnson_neyman()`. The argument `sim_slopes()` is
looking for is called `"jnalpha"`. Now probe_interaction will pass
`"alpha"` arguments as `"jn_alpha"`.
* `interact_plot()` would stop on an error when the model included a two-level
factor not involved in the interaction and not centered. Now those factors in
that situation are treated like other factors.
* `interact_plot()` sometimes gave misleading output when users manually defined
moderator labels. It is now more consistent with the ordering the labels and 
values and will not wrongly label them when the values are provided in an
odd order.
* `wgttest()` now functions properly when a vector of weights is provided to the
weights argument rather than a column name.
* `gscale()` now works properly on tibbles, which requires a different style of 
column indexing than data frames.
* Related to the prior point, `j_summ()`/`standardize_lm()`/`center_lm()` now
work properly on models that were originally fit with tibbles in the data
argument.
* `sim_slopes()` would fail for certain weighted `lm` objects depending on the
way the weights were specified in the function call. It should now work for all
weighted `lm` objects.

# jtools 0.5.0

More goodies for users of `interact_plot()`:

* Added support for models with a weights parameter in `interact_plot()`. 
It would work previously, but didn't use a weighted mean or SD in calculating 
values of the moderator(s) and for mean-centering other predictors. Now it does.
* Added support for two-level factor predictors in `interact_plot()`.
Previously, factor variables had to be a moderator. 
* When predictor in `interact_plot()` has only two unique values (e.g., dummy 
variables that have numeric class), by default only those two values have
tick marks on the x-axis. Users may use the `pred.labels` argument to specify
labels for those ticks.
* Offsets are now supported (especially useful for Poisson GLMs), but only
if specified via the offset argument rather than included in the model formula.
You can (and should) specify the offset used for the plot using the `set.offset`
argument. By default it is 1 so that the y-axis represents a proportion. 

Other feature changes:

* `sim_slopes()` now supports weights (from the weights argument rather than a
`svyglm` model). Previously it used unweighted mean and standard deviation for
non-survey models with weights.
* Improved printing features of `wgttest()`.

Bug fixes:

* R 3.4 introduced a change that caused warning messages when return objects
are created in a certain way. This was first addressed in jtools 0.4.5, but
a few instances slipped through the cracks. Thanks to Kim Henry for pointing
out one such instance.
* When `sim_slopes()` called `johnson_neyman()` while the `robust` argument was 
set to TRUE, the `robust.type` argument was not being passed (causing the 
default of "HC3" to be used). Now it is passing that argument correctly.

# jtools 0.4.5

* Added better support for plotting nonlinear interactions with 
`interact_plot()`, providing an option to plot on original (nonlinear) scale.
* `interact_plot()` can now plot fixed effects interactions from `merMod`
objects
* Fixed warning messages when using `j_summ()` with R 3.4.x
* Added preliminary `merMod` support for `j_summ()`. Still needs convergence 
warnings, some other items.

# jtools 0.4.4

* Under the hood changes to `j_summ()`
* Cleaned up examples
* Added `wgttest()` function, which runs a test to assess need for sampling 
weights in linear regression

# jtools 0.4.3 

* No matter what you do, there's nothing like seeing your package on CRAN to 
open your eyes to all the typos, etc. you've put into your package. 

# jtools 0.4.2 — Initial CRAN release

* This is the first CRAN release. Compared to 0.4.1, the prior Github release,
dependencies have been removed and several functions optimized for speed.

