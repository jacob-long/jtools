## jtools 0.4.6

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

