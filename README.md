
# jtools [![Build Status](https://travis-ci.org/jacob-long/jtools.svg?branch=master)](https://travis-ci.org/jacob-long/jtools) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jacob-long/JTools?branch=master&svg=true)](https://ci.appveyor.com/project/jacob-long/JTools) [![codecov](https://codecov.io/gh/jacob-long/jtools/branch/master/graph/badge.svg)](https://codecov.io/gh/jacob-long/jtools) [![Packagist](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://opensource.org/licenses/MIT)

This package consists of a series of functions created by the author (Jacob) to 
automate otherwise tedious research tasks. At this juncture, the unifying theme 
is the more efficient presentation of regression analyses, including those with 
interactions. Support for the `survey` package's `svyglm` objects is a common theme
throughout.

## Installation

If you don't have `devtools` installed, first install it.

```r
install.packages("devtools")
```

Then install the package from Github--it is not yet available from CRAN as it is in its early stages of development.

```r
devtools::install_github("jacob-long/jtools")
```

## Usage

Here's a brief synopsis of the current functions in the package:

* `j_summ()`: A replacement for `summary.lm` that provides the user several options for formatting regression summaries. Supports `glm` and `svyglm` objects as input as well, but it is not tested with nonlinear models. It supports calculation and reporting of robust standard errors via the `sandwich` and `lmtest` packages.
* `sim_slopes()`: An interface for simple slopes analysis for 2-way interactions. User can specify values of the moderator to test or use the default +/- 1 SD values.
* `interact_plot()`: Plots two-way interactions using `ggplot2` using a similar interface to the aforementioned `sim_slopes()` function. Users can customize the appearance with familiar `ggplot2` commands.
* `theme_apa()` will format your `ggplot2` graphics to make them (mostly) appropriate for APA style publications.
* `svycor()`: Calculate correlations with complex survey designs from the `survey` package.

Details on the arguments can be accessed via the R documentation (`?functionname`). 
There are now vignettes documenting just about everything you can do as well.

## Contributing

I'm happy to receive bug reports, suggestions, questions, and (most of all) contributions to fix problems and add features. I prefer you use the Github issues system over trying to reach out to me in other ways. Pull requests for contributions are encouraged.

## License

The source code of this package is licensed under the [MIT License](http://opensource.org/licenses/mit-license.php).
