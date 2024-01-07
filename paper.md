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
date: 26 October 2023
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
for comparing nested/related model specifications. Plots are generated with 
`ggplot2` — allowing knowledgeable users to further customize the appearance —
but do not require the user to know how to use `ggplot2`.

Other R packages exist to achieve some of these ends. In some cases, `jtools`
is using third-party packages for computation and simply repackaging the results
(e.g., `sandwich` [@sandwich] for robust standard errors and `pbkrtest` for computation of 
*p*-values for multilevel models). `modelsummary` provides comparable
functionality to `summ()` and has some advantages, such as a greater range
of supported models and more support for exporting to external documents. 
`marginaleffects`, `sjPlot`, and `see` offer support for plotting predicted 
values from fitted regression models and again have some of their own 
advantages, such as more supported model types. There is also overlap in 
functionality with the `car` package (e.g., the computation of variance 
inflation factors and partial residuals), although `jtools` aims to improve
the user interface and support more model types in some cases.

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References