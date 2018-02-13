## jtools 1.0.0.9000

**Major release**

This release has several big changes embedded within, side projects that needed
a lot of work to implement and required some user-facing changes. Overall
these are improvements, but in some edge cases they could break old code.

`interact_plot`, `cat_plot`, and `effect_plot`:

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
allows for fitting polynomials with `effect_plot` or even interactions with
polynomials with `interact_plot`. For instance, if my model was fit using
this kind of formula --- `y ~ poly(x, 2) + z` --- I could then plot the 
predicted curve with `effect_plot(fit, pred = x, data = data)` substituting
`fit` with whatever my model is called and `data` with whatever data frame
I used is called.

There some possible drawbacks for these changes. One is that no longer are 
factor predictors supported in `interact_plot` and `effect_plot`,
even two-level ones. This worked before by coercing
them to 0/1 continuous variables and re-fitting the model. Since the model is
no longer re-fit, this can't be done. To work around it, either transform the
predictor to numeric before fitting the model or use `cat_plot`. Relatedly,
two-level factor covariates are no longer centered and are simply
set to their reference value.

*All interaction tools*:

All these tools have a new default `centered` argument. They are now set to
`centered = "all"`, but `"all"` no longer means what it used to. Now it refers
to *all variables not included in the interaction, including the dependent
variable*. This means that in effect, the default option does the same thing
that previous versions did. But instead of having that occur when 
`cenetered = NULL`, that's what `centered = "all"` means. There is no 
`NULL` option any longer. Note that with `sim_slopes`, the focal predictor
(`pred`) will now be centered --- this only affects the conditional intercept.

`sim_slopes`:

This function now supports categorical (factor) moderators, though there is
no option for Johnson-Neyman intervals in these cases. You can use the 
significance of the interaction term(s) for inference about whether the slopes
differ at each level of the factor when the moderator is a factor.

`gscale`: 

The interface has been changed slightly, with the actual numbers always provided
as the `data` argument. There is no `x` argument and instead a `vars` argument
to which you can provide variable names. The upshot is that it now fits much 
better into a piping workflow. 

The entire function has gotten an extensive reworking, which in some cases 
should result in significant speed gains. And if that's not enough, just know
that the code was an absolute monstrosity before and now it's not.

There are two new functions that are wrappers around `gscale`: `standardize`
and `center`, which call `gscale` but with `n.sd = 1` in the first case and
with `center.only = TRUE` in the latter case.


`summ`:

Like the rest of R, when `summ` rounded your output, items rounded exactly to
zero would be treated as, well, zero. But this can be misleading if the original
value was actually negative. For instance, if `digits = 2` and a coefficient
was `-0.003`, the value printed to the console was `0.00`, suggesting a zero
or slightly positive value when in fact it was the opposite. This is a 
deficiency of the `round` (and `trunc`) function. I've now changed it so the
zero-rounded value retains its sign.

## jtools 0.9.4 (CRAN release)

This release is limited to dealing with the `huxtable` package's temporary
removal from CRAN, which in turn makes this package out of compliance with
CRAN policies regarding dependencies on non-CRAN packages.

Look out for `jtools` 1.0.0 coming very soon!

## jtools 0.9.3 (CRAN release)

Bugfixes:

* `johnson_neyman` and `sim_slopes` were both encountering errors with 
`merMod` input. Thanks to Seongho Bae for reporting these issues and testing
out development versions. 
* An upcoming version of R will change a common warning to an error, causing
a need to change the internals of `gscale`. 
* The default model names in `export_summs` had an extra space (e.g., `( 1)`) 
due to changes in `huxtable`. The defaults are now just single numbers.

## jtools 0.9.2 

Bugfix:

* Johnson-Neyman plots misreported the alpha level if `control.fdr` was `TRUE`.
It was reporting `alpha * 2` in the legend, but now it is accurate again.

Feature update:

* `johnson_neyman` now handles multilevel models from `lme4`. 

## jtools 0.9.1 (CRAN release)

Bugfix update:

Jonas Kunst helpfully pointed out some odd behavior of `interact_plot` with 
factor moderators. No longer should there be occasions in which you have two
different legends appear. The linetype and colors also should now be consistent
whether there is a second moderator or not. For continuous moderators, the 
darkest line should also be a solid line and it is by default the highest 
value of the moderator.

Other fixes:

* An update to `huxtable` broke `export_summs`, but that has been fixed.

Feature updates:

* You can now manually provide colors to `interact_plot` and `cat_plot` by 
providing a vector of colors (any format that `ggplot2` accepts) for the 
`color.class` argument.
* Noah Greifer wrote up a tweak to `summ` that formats the output in a way that
lines up the decimal points. It looks great.

## jtools 0.9.0 (CRAN release)

This may be the single biggest update yet. If you downloaded from CRAN, be sure
to check the 0.8.1 update as well.

New features are organized by function.

johnson_neyman:

* A new `control.fdr` option is added to control the false discovery rate, 
building on new research. This makes the test more conservative but less likely 
to be a Type 1 error.
* A `line.thickness` argument has been added after Heidi Jacobs pointed out 
that it cannot be changed after the fact.
* The construction of the multiple plots when using `sim_slopes` for 3-way
interactions is much-improved.
* The critical test statistic used by default has been slightly altered. It
previously used a normal approximation; i.e., if `alpha = .05` the critical
test statistic was always 1.96. Now, the residual degrees of freedom are used
with the t distribution. You can do it the old way by setting `df = "normal"`
or any arbitrary number.

interact_plot: 

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
can play with the value until it looks right. This applies to `effect_plot` as
well.

summ:

* Users are now informed why the function is taking so long if `r.squared`
or `pbkrtest` are slowing things down. `r.squared` is now set to FALSE by 
default.

New functions!

`plot_summs`: A graphic counterpart to `export_summs`, which was introduced in
the 0.8.0 release. This plots regression coefficients to help in visualizing
the uncertainty of each estimate and facilitates the plotting of nested models
alongside each other for comparison. This allows you to use `summ` features 
like robust standard errors and scaling with this type of plot that you could
otherwise create with some other packages.

`plot_coefs`: Just like `plot_summs`, but no special `summ` features. This 
allows you to use models unsupported by `summ`, however, and you can provide
`summ` objects to plot the same model with different `summ` argument alongside
each other.

`cat_plot`: This was a long time coming. It is a complementary function to 
`interact_plot`, but is designed to deal with interactions between 
categorical variables. You can use bar plots, line plots, dot plots, and
box and whisker plots to do so. You can also use the function to plot the effect
of a single categorical predictor without an interaction.

## jtools 0.8.1

Thanks to Kim Henry who reported a bug with `johnson_neyman` in the case that
there is an interval, but the entire interval is outside of the plotted area:
When that happened, the legend wrongly stated the plotted line was 
non-significant.

Besides that bugfix, some new features:

* When `johnson_neyman` fails to find the interval (because it doesn't exist),
it no longer quits with an error. The output will just state the interval was
not found and the plot will still be created.
* Much better support for plotting observed data in `interact_plot` has been 
added. Previously, if the moderator was a factor, you would get very nicely
colored plotted points when using `plot.points = TRUE`. But if the moderator
was continuous, the points were just black and it wasn't very informative beyond
examining the main effect of the focal predictor. With this update, the
plotted points for continous moderators are shaded along a gradient that matches
the colors used for the predicted lines and confidence intervals.

## jtools 0.8.0 (CRAN release)

Not many user-facing changes since 0.7.4, but major refactoring internally
should speed things up and make future development smoother.

## jtools 0.7.4

Bugfixes:

* interact_plot and effect_plot would trip up when one of the focal predictors
had a name that was a subset of a covariate (e.g., pred = "var" but a covariate
is called "var_2"). That's fixed.
* Confidence intervals for merMod objects were not respecting the user-requested
confidence level and that has been fixed.
* Confidence intervals for merMod objects were throwing a
spurious warning on R 3.4.2.
* interact_plot was mis-ordering secondary moderators. That has been fixed.
* export_summs had a major performance problem when providing extra arguments
which may have also caused it to wrongly ignore some arguments. That has been 
fixed and it is much faster.

Enhancements:
* interact_plot now gives more informative labels for secondary moderators when
the user has defined the values but not the labels.
* confidence intervals are now properly supported with export_summs
* changes made to export_summs for compatibility with huxtable 1.0.0 changes

## jtools 0.7.3 (CRAN release)

Important bugfix:

* When standardize was set to TRUE using summ, the model was not mean-centered
as the output stated. This has been fixed. I truly regret the error---double-check
any analyses you may have run with this feature.

New function: `export_summs`. 

This function outputs regression models supported
by summ in table formats useful for RMarkdown output as well as specific options
for exporting to Microsoft Word files. This is particularly helpful for those
wanting an efficient way to export regressions that are standardized and/or use
robust standard errors. 


## jtools 0.7.2

The documentation for j_summ has been reorganized such that each supported
model type has its own, separate documentation. `?j_summ` will now just give you
links to each supported model type.

More importantly, j_summ will from now on be referred to as, simply, summ. 
Your old code is fine; j_summ will now be an alias for summ and will run the 
same underlying code. Documentation will refer to the summ function, though.
That includes the updated vignette.

One new feature for summ.lm:

* With the `part.corr = TRUE` argument for a linear model, partial and 
semipartial correlations for each variable are reported.

More tweaks to summ.merMod:

* Default behavior with regard to p values depends on model type (lmer vs.
glmer/nlmer) and, in the case of linear models, whether the `pbkrtest` package
is installed. If it is, p values are calculated based on the Kenward-Roger
degrees of freedom calculation and printed. Otherwise, p values are not 
shown by default with lmer models. P values are shown with glmer models, since
that is also the default behavior of `lme4`.
* There is an `r.squared` option, which for now is FALSE by default. It adds
runtime since it must fit a null model for comparison and sometimes this also
causes convergence issues.

## jtools 0.7.1

Returning to CRAN!

A very strange bug on CRAN's servers was causing jtools updates to silently fail
when I submitted updates; I'd get a confirmation that it passed all tests, but
a LaTeX error related to an Indian journal I cited was torpedoing it before
it reached CRAN servers.

The only change from 0.7.0 is fixing that problem, but if you're a CRAN user
you will want to flip through the past several releases as well to see what 
you've missed.

## jtools 0.7.0

New features:

* j_summ can now provide cluster-robust standard errors for lm models.
* j_summ output now gives info about missing observations for supported models.
* At long last, j_summ/scale_lm/center_lm can standardize/center models with
logged terms and other functions applied.
* interact_plot and effect_plot will now also support predictors that have
functions applied to them.
* j_summ now supports confidence intervals at user-specified widths.
* j_summ now allows users to not display p-values if requested.
* I've added a warning to j_summ output with merMod objects, since it provides
p-values calculated on the basis of the estimated t-values. These are not to
be interpreted in the same way that OLS and GLM p-values are, since with 
smaller samples mixed model t-values will give inflated Type I error rates.
* By default, j_summ will not show p-values for merMod objects.

Bug fix:

* scale_lm did not have its center argument implemented and did not 
explain the option well in its documentation.
* johnson_neyman got confused when a factor variable was given as a predictor

## jtools 0.6.1

Bug fix release:

* wgttest acted in a way that might be unexpected when providing a weights
variable name but no data argument. Now it should work as expected by getting
the data frame from the model call.
* gscale had a few situations in which it choked on missing data, especially
when weights were used. This in turn affected j_summ, scale_lm, and center_lm,
which each rely on gscale for standardization and mean-centering. That's fixed 
now.
* gscale wasn't playing nicely with binary factors in survey designs, rendering
the scaling incorrect. If you saw a warning, re-check your outputs after this
update.

## jtools 0.6.0

A lot of changes!

New functions:

* effect_plot: If you like the visualization of moderation effects from 
interact_plot, then you should enjoy effect_plot. It is a clone of 
interact_plot, but shows a single regression line rather than several. It
supports GLMs and lme4 models and can plot original, observed data points.
* pf_sv_test: Another tool for survey researchers to test whether it's okay
to run unweighted regressions. Named after Pfefferman and Svervchkov, who
devised the test.
* weights_tests: Like probe_interaction does for the interaction functions,
weights_tests will run the new pf_sv_test as well as wgttest simultaneously
with a common set of arguments. 

Enhancements:

* Set a default number of digits to print for all jtools functions with the
option "jtools-digits". 
* wgttest now accepts and tests GLMs and may work for other regression models.


Bug fixes:

* j_summ would print significance stars based on the rounded p value, sometimes
resulting in misleading output. Now significance stars are based on the 
non-rounded p values.
* probe_interaction did not pass an "alpha" argument to sim_slopes, possibly
confusing users of johnson_neyman. The argument sim_slopes is looking for is 
called "jnalpha". Now probe_interaction will pass "alpha" arguments as "jn_alpha".
* interact_plot would stop on an error when the model included a two-level factor
not involved in the interaction and not centered. Now those factors in that 
situation are treated like other factors.
* interact_plot sometimes gave misleading output when users manually defined
moderator labels. It is now more consistent with the ordering the labels and 
values and will not wrongly label them when the values are provided in an
odd order.
* wgttest now functions properly when a vector of weights is provided to the
weights argument rather than a column name.
* gscale now works properly on tibbles, which requires a different style of 
column indexing than data frames.
* Related to the prior point, j_summ/standardize_lm/center_lm now work properly
on models that were originally fit with tibbles in the data argument.
* sim_slopes would fail for certain weighted lm objects depending on the way
the weights were specified in the function call. It should now work for all
weighted lm objects.

## jtools 0.5.0

More goodies for users of interact_plot:

* Added support for models with a weights parameter in interact_plot. It would 
work previously, but didn't use a weighted mean or SD in calculating values of 
the moderator(s) and for mean-centering other predictors. Now it does.
* Added support for two-level factor predictors in interact_plot. Previously, 
factor variables had to be a moderator. 
* When predictor in interact_plot has only two unique values (e.g., dummy 
variables that have numeric class), by default only those two values have
tick marks on the x-axis. Users may use the pred.labels argument to specify
labels for those ticks.
* Offsets are now supported (especially useful for Poisson GLMs), but only
if specified via the offset argument rather than included in the model formula.
You can (and should) specify the offset used for the plot using the set.offset
argument. By default it is 1 so that the y-axis represents a proportion. 

Other feature changes:

* sim_slopes now supports weights (from the weights argument rather than a
svyglm model). Previously it used unweighted mean and standard deviation for
non-survey models with weights.
* Improved printing features of wgttest

Bug fixes:

* R 3.4 introduced a change that caused warning messages when return objects
are created in a certain way. This was first addressed in jtools 0.4.5, but
a few instances slipped through the cracks. Thanks to Kim Henry for pointing
out one such instance.
* When sim_slopes called johnson_neyman while the robust argument was set to 
TRUE, the robust.type argument was not being passed (causing the default of 
"HC3" to be used). Now it is passing that argument correctly.

## jtools 0.4.5

* Added better support for plotting nonlinear interactions with interact_plot,
providing an option to plot on original (nonlinear) scale.
* interact_plot can now plot fixed effects interactions from merMod objects
* Fixed warning messages when using j_summ with R 3.4.x
* Added preliminary merMod support for j_summ. Still needs convergence warnings,
  some other items.

## jtools 0.4.4

* Under the hood changes to j_summ
* Cleaned up examples
* Added wgttest function, which runs a test to assess need for sampling weights
in linear regression

## jtools 0.4.3 

* No matter what you do, there's nothing like seeing your package on CRAN to 
open your eyes to all the typos, etc. you've put into your package. 

## jtools 0.4.2 — Initial CRAN release

* This is the first CRAN release. Compared to 0.4.1, the prior Github release,
dependencies have been removed and several functions optimized for speed.

