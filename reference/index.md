# Package index

## Summarizing and visualizing regression models

- [`summ()`](summ.md) [`j_summ()`](summ.md) : Regression summaries with
  options

- [`summ(`*`<lm>`*`)`](summ.lm.md) : Linear regression summaries with
  options

- [`summ(`*`<glm>`*`)`](summ.glm.md) : Generalized linear regression
  summaries with options

- [`summ(`*`<svyglm>`*`)`](summ.svyglm.md) : Complex survey regression
  summaries with options

- [`summ(`*`<merMod>`*`)`](summ.merMod.md) : Mixed effects regression
  summaries with options

- [`summ(`*`<rq>`*`)`](summ.rq.md) : Quantile regression summaries with
  options

- [`set_summ_defaults()`](set_summ_defaults.md) :

  Set defaults for [`summ()`](../reference/summ.md) functions

- [`export_summs()`](export_summs.md) : Export regression summaries to
  tables

- [`plot_summs()`](plot_summs.md) [`plot_coefs()`](plot_summs.md) : Plot
  Regression Summaries

- [`effect_plot()`](effect_plot.md) : Plot simple effects in regression
  models

## Plotting and generating model predictions

- [`effect_plot()`](effect_plot.md) : Plot simple effects in regression
  models
- [`make_predictions()`](make_predictions.md) : Generate predicted data
  for plotting results of regression models
- [`make_new_data()`](make_new_data.md) : Make new data for generating
  predicted data from regression models.
- [`partialize()`](partialize.md) : Adjust observed data for partial
  residuals plots

## Data centering and scaling tools

- [`gscale()`](gscale.md) : Scale and/or center data, including survey
  designs
- [`scale_mod()`](scale_mod.md) : Scale variables in fitted regression
  models
- [`center_mod()`](center_mod.md) : Center variables in fitted
  regression models
- [`standardize()`](standardize.md) : Standardize vectors, data frames,
  and survey designs
- [`center()`](center.md) : Mean-center vectors, data frames, and survey
  designs

## Survey data tools

- [`svycor()`](svycor.md) : Calculate Pearson correlations with complex
  survey data
- [`svysd()`](svysd.md) : Calculate standard deviations with complex
  survey data
- [`weights_tests()`](weights_tests.md) : Test whether sampling weights
  are needed
- [`wgttest()`](wgttest.md) : Test whether sampling weights are needed
- [`pf_sv_test()`](pf_sv_test.md) : Test whether sampling weights are
  needed

## Theming

- [`theme_apa()`](theme_apa.md) : Format ggplot2 figures in APA style

- [`theme_nice()`](theme_nice.md) :

  A nice, flexible `ggplot2` theme

- [`add_gridlines()`](gridlines.md) [`add_x_gridlines()`](gridlines.md)
  [`add_y_gridlines()`](gridlines.md) [`drop_gridlines()`](gridlines.md)
  [`drop_x_gridlines()`](gridlines.md)
  [`drop_y_gridlines()`](gridlines.md) : Add and remove gridlines

- [`jtools_colors`](jtools_colors.md) :

  Color palettes in `jtools` functions

## Miscellaneous

- [`` `%nin%` ``](nin.md) :

  Not `%in%`

- [`` `%not%` ``](subsetters.md) [`` `%not%<-`() ``](subsetters.md)
  [`` `%just%` ``](subsetters.md) [`` `%just%<-`() ``](subsetters.md) :
  Subsetting operators

- [`num_print()`](num_print.md) : Numbering printing with signed zeroes
  and trailing zeroes

- [`wrap_str()`](wrap_str.md) [`cat_wrap()`](wrap_str.md)
  [`warn_wrap()`](wrap_str.md) [`stop_wrap()`](wrap_str.md)
  [`msg_wrap()`](wrap_str.md) :

  `cat`, `message`, `warning`, and `stop` wrapped to fit the console's
  width.

- [`wtd.sd()`](wtd.sd.md) : Weighted standard deviation calculation

- [`get_offset_name()`](model_utils.md)
  [`get_weights()`](model_utils.md) [`get_data()`](model_utils.md)
  [`get_response_name()`](model_utils.md) : Utility functions for
  generating model predictions

- [`md_table()`](md_table.md) : Print attractive data frames in the
  console

- [`tidy(`*`<summ>`*`)`](glance.summ.md)
  [`tidy(`*`<summ.merMod>`*`)`](glance.summ.md)
  [`glance(`*`<summ.lm>`*`)`](glance.summ.md)
  [`glance(`*`<summ.glm>`*`)`](glance.summ.md)
  [`glance(`*`<summ.svyglm>`*`)`](glance.summ.md)
  [`glance(`*`<summ.merMod>`*`)`](glance.summ.md)
  [`glance(`*`<summ.rq>`*`)`](glance.summ.md) : Broom extensions for
  summ objects

- [`knit_print(`*`<summ.lm>`*`)`](knit_print.summ.md)
  [`knit_print(`*`<summ.glm>`*`)`](knit_print.summ.md)
  [`knit_print(`*`<summ.svyglm>`*`)`](knit_print.summ.md)
  [`knit_print(`*`<summ.merMod>`*`)`](knit_print.summ.md)
  [`knit_print(`*`<summ.rq>`*`)`](knit_print.summ.md) : knitr methods
  for summ

- [`get_colors()`](get_colors.md) : Get colors for plotting functions

- [`get_formula()`](get_formula.md) : Retrieve formulas from model
  objects

- [`get_robust_se()`](get_robust_se.md) : Calculate robust standard
  errors and produce coefficient tables

- [`interact_plot()`](interactions_deprecated.md)
  [`cat_plot()`](interactions_deprecated.md)
  [`sim_slopes()`](interactions_deprecated.md)
  [`johnson_neyman()`](interactions_deprecated.md)
  [`probe_interaction()`](interactions_deprecated.md) : Deprecated
  interaction functions

- [`predict_merMod()`](predict_merMod.md) :

  Alternative interface for `merMod` predictions

- [`movies`](movies.md) : Data about movies
