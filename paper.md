---
title: 'jtools: Analysis and Presentation of Social Scientific Data'
tags:
  - R
  - social science
  - regression
authors:
  - name: Jacob A. Long
    orcid: 0000-0002-1582-6214
    affiliation: 1
affiliations:
 - name: School of Journalism and Mass Communications, University of South Carolina, USA
   index: 1
date: 6 January 2024
bibliography: paper.bib

---

# Summary

`jtools` is an R package designed to ease the exploration and
presentation of regression models with a focus on the needs of social scientists.
Most notably, it generates results summaries that are meant to provide some
of the conveniences of commercial software such as Stata (e.g., calculating
robust standard errors and integrating them into a results table).
Additionally, `jtools` includes plotting functions to help users better 
understand and share the results and predictions from fitted regression models.

# Statement of need

Among the users of R are working or trainee scientists who are accustomed to
software that provides detailed and customizable summaries of regression results
"out of the box" (i.e., integrated into the modules that fit the model). While
R's `summary()` function provides useful information for many regression 
models — such as the method provided for `lm()` including coefficients, 
standard errors, test statistics, and *p*-values — there is typically little
flexiblity for customizing the output. Generating results summaries with 
additional information, such as robust standard errors, confidence intervals,
transformed/centered variables, and variance inflation factors demands 
considerably more programming knowledge and effort for the user, requiring more
time and making mistakes more likely. These same concerns exist for the case of
plotting predicted values from regression models.

`jtools` provides an alternative to `summary()`, `summ()`, that allows users
more flexibility in generating results tables for supported regression models.
Importantly, these capabilities are accessible almost solely through
arguments to `summ()`, thereby requiring little more programming knowledge
than what is already required to fit a regression model and print a minimal 
summary to the console. For plotting, the package includes `effect_plot()`
for creating line plots for a focal predictor variable. Users may add 
confidence intervals (including the ability to use robust standard errors for
the plotted intervals), the observed data as plotted points, partial residuals
as plotted points, and rug plots. Categorical predictors can be plotted as 
bar charts of single points with error bars as well. Finally, `jtools` features
the function `plot_coefs()` that allows users to plot regression coefficients
with confidence intervals for one or more models. This is particularly useful
for comparing nested/related model specifications [@kastellec2007]. 
Plots are generated with 
`ggplot2` — allowing knowledgeable users to further customize the appearance —
but do not require the user to know how to use `ggplot2`.

To support survey researchers, `jtools` functions generally support the use of
sampling weights and survey design objects created by the `survey` package 
[@survey]. To fill in a gap in `survey`'s offerings, it adds `svycor()`, which
calculates survey-weighted correlation matrices for survey design objects. 
In addition, `weights_tests()` implements the tests for the ignorability of 
sampling weights first devised by @dumouchel1983 and @pfeffermann1999. 
In layman's terms, these tests allow researchers to check whether the use of
sampling weights to make data better resemble a population meaningfully change
their statistical results compared to ignoring those weights.

## Alternative software

Other R packages exist to achieve some of these ends. In some cases, `jtools`
is using third-party packages for computation and simply repackaging the results
(e.g., `sandwich` \[@sandwich\] for robust standard errors and `pbkrtest` 
\[@pbkrtest\] for computation of 
*p*-values for multilevel models). `modelsummary` [@modelsummary] provides 
comparable functionality to `summ()` and has some advantages, such as a 
greater range
of supported models and more support for exporting to external documents. 
`marginaleffects` [@marginaleffects], `sjPlot` [@sjPlot], and `see` [@see] 
offer support for plotting predicted 
values from fitted regression models and again have some of their own 
advantages, such as more supported model types. There is also overlap in 
functionality with the `car` package [@car], like the computation of variance 
inflation factors and partial residuals, although `jtools` aims to improve
the user interface and support more model types in some cases.

## Real-world use

At the time of writing, Google Scholar has tracked 556 references to `jtools`.
These are predominantly, but not solely, in the social sciences, such as 
psychology, communication, and political science. For example, @sutin2023 
uses `summ()` to calculate degrees of freedom for a multilevel model in a 
study of personality and aging. @urban-wojcik2022 also use `summ()`, in this
case to summarize regression models with robust standard errors in their study
of physical activity and the hippocampus. @kraft2022 generate plotted regression
summaries with `plot_coefs()`. Finally, @spalti2023 use `weights_tests()` to 
assess the sensitivity of their estimates to the influence of survey sampling
weights.

# References