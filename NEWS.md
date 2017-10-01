## jtools 0.7.3

Important bugfix:

* When standardize was set to TRUE using summ, the model was not mean-centered
as the output stated. This has been fixed. I truly regret the error---double-check
any analyses you may have run with this feature.

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

