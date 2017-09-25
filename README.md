
<!-- README.md is generated from README.Rmd. Please edit that file -->
jtools [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/jtools)](https://cran.r-project.org/package=jtools) [![Build Status](https://travis-ci.org/jacob-long/jtools.svg?branch=master)](https://travis-ci.org/jacob-long/jtools) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jacob-long/JTools?branch=master&svg=true)](https://ci.appveyor.com/project/jacob-long/JTools) [![codecov](https://codecov.io/gh/jacob-long/jtools/branch/master/graph/badge.svg)](https://codecov.io/gh/jacob-long/jtools)
====================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================

<!-- [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![MIT License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://opensource.org/licenses/MIT) -->
This package consists of a series of functions created by the author (Jacob) to automate otherwise tedious research tasks. At this juncture, the unifying theme is the more efficient presentation of regression analyses, including those with interactions. Support for the `survey` package's `svyglm` objects is a common theme throughout.

**Note**: This is beta software. Bugs are possible, both in terms of code-breaking errors and more pernicious errors of mistaken computation.

Installation
------------

For the most stable version, simply install from CRAN.

``` r
install.packages("jtools")
```

If you want the latest features and bug fixes (and perhaps the latest bugs, too) then you can download from Github. To do that you will need to have `devtools` installed if you don't already:

``` r
install.packages("devtools")
```

Then install the package from Github.

``` r
devtools::install_github("jacob-long/jtools")
```

Usage
-----

Here's a brief synopsis of the current functions in the package:

#### `summ`

This is a replacement for `summary` that provides the user several options for formatting regression summaries. It supports `glm`, `svyglm`, and `merMod` objects as input as well. It supports calculation and reporting of robust standard errors via the `sandwich` and `lmtest` packages.

``` r
fit <- lm(mpg ~ hp*wt, data=mtcars)
summ(fit)
```

    #> MODEL INFO:
    #> Observations: 32
    #> Dependent Variable: mpg
    #> 
    #> MODEL FIT: 
    #> F(3,28) = 71.66, p = 0
    #> R-squared = 0.885
    #> Adj. R-squared = 0.872
    #> 
    #> Standard errors: OLS 
    #>             Est.   S.E.  t val. p        
    #> (Intercept) 49.808 3.605 13.816 0     ***
    #> hp          -0.12  0.025 -4.863 0     ***
    #> wt          -8.217 1.27  -6.471 0     ***
    #> hp:wt       0.028  0.007 3.753  0.001 ***

It has some other conveniences as well, like re-fitting your model with standardized variables. I'm a fan of Andrew Gelman's 2 SD standardization method. You can also get a couple diagnostic checks for linear models along with multicollinearity information.

``` r
fit2 <- lm(Murder ~ Assault + UrbanPop, data = USArrests)
summ(fit2, standardize = TRUE, n.sd = 2, vifs = TRUE, robust = TRUE,
       model.check = TRUE)
```

    #> MODEL INFO:
    #> Observations: 50
    #> Dependent Variable: Murder
    #> 
    #> MODEL FIT: 
    #> F(2,47) = 46.319, p = 0
    #> R-squared = 0.663
    #> Adj. R-squared = 0.649
    #> 
    #> MODEL CHECKING:
    #> Homoskedasticity (Breusch-Pagan) = Assumption not violated (p = 0.144)
    #> Number of high-leverage observations = 5
    #> 
    #> Standard errors: Robust, type = HC3
    #>             Est.   S.E.  t val. p         VIF  
    #> (Intercept) 3.207  1.625 1.974  0.054 .        
    #> Assault     7.319  0.815 8.982  0     *** 1.072
    #> UrbanPop    -1.289 0.802 -1.608 0.115     1.072
    #> 
    #> All continuous variables are mean-centered and scaled by 2 s.d.

#### `sim_slopes`

This is an interface for simple slopes analysis for 2- and 3-way interactions. Users can specify values of the moderator to test or use the default +/- 1 SD values. Johnson-Neyman intervals are also calculated and, if requested, Johnson-Neyman plots are returned.

``` r
fiti <- lm(Murder ~ Assault*UrbanPop, data = USArrests)
sim_slopes(fiti, pred = UrbanPop, modx = Assault, jnplot = TRUE)
```

    #> JOHNSON-NEYMAN INTERVAL
    #> 
    #> The slope of UrbanPop is p < .05 when Assault is INSIDE this interval:
    #> [193.5072, 363.9215]
    #> Note: The range of observed values of Assault is [45, 337]

![](tools/README-j-n-plot-1.png)

    #> SIMPLE SLOPES ANALYSIS
    #> 
    #> Slope of UrbanPop when Assault = 254.098 (+1 SD): 
    #>   Est.   S.E.      p 
    #> -0.075  0.035  0.035 
    #> 
    #> Slope of UrbanPop when Assault = 170.76 (Mean): 
    #>   Est.   S.E.      p 
    #> -0.044  0.026  0.100 
    #> 
    #> Slope of UrbanPop when Assault = 87.422 (-1 SD): 
    #>   Est.   S.E.      p 
    #> -0.012  0.035  0.730

#### `interact_plot`

This function plots two-way interactions using `ggplot2` with a similar interface to the aforementioned `sim_slopes` function. Users can customize the appearance with familiar `ggplot2` commands. It supports several customizations, like confidence intervals.

``` r
interact_plot(fit, pred="wt", modx = "hp", interval = T, int.width = .95)
```

![](tools/README-interact_plot_continuous-1.png)

The function also supports categorical moderators and plotting observed data points alongside best-fitting lines.

``` r
fitiris <- lm(Petal.Length ~ Petal.Width*Species, data = iris)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species", plot.points = TRUE)
```

![](tools/README-interact_plot_factor-1.png)

#### `probe_interaction`

This function allows you to use the two above functions simultaneously, owing to their common syntax. Here is an example, which can also serve to show off how the 3-way interaction output looks.

``` r
fita3 <- lm(rating ~ privileges*critical*learning, data = attitude)
probe_interaction(fita3, pred = critical, modx = learning, mod2 = privileges)
```

    #> #######################################################
    #> While privileges (2nd moderator) = 40.898 (Mean of privileges -1 SD)
    #> #######################################################
    #> 
    #> JOHNSON-NEYMAN INTERVAL
    #> 
    #> The slope of critical is p < .05 when learning is INSIDE this interval:
    #> [52.8623, 73.5881]
    #> Note: The range of observed values of learning is [34, 75]
    #> 
    #> SIMPLE SLOPES ANALYSIS
    #> 
    #> Slope of critical when learning = 68.104 (+1 SD): 
    #>  Est.  S.E.     p 
    #> 1.109 0.553 0.057 
    #> 
    #> Slope of critical when learning = 56.367 (Mean): 
    #>  Est.  S.E.     p 
    #> 0.675 0.329 0.052 
    #> 
    #> Slope of critical when learning = 44.63 (-1 SD): 
    #>  Est.  S.E.     p 
    #> 0.242 0.237 0.318 
    #> 
    #> #######################################################
    #> While privileges (2nd moderator) = 53.133 (Mean of privileges)
    #> #######################################################
    #> 
    #> The Johnson-Neyman interval could not be found. Is your interaction term significant?
    #> 
    #> SIMPLE SLOPES ANALYSIS
    #> 
    #> Slope of critical when learning = 68.104 (+1 SD): 
    #>  Est.  S.E.     p 
    #> 0.023 0.330 0.946 
    #> 
    #> Slope of critical when learning = 56.367 (Mean): 
    #>  Est.  S.E.     p 
    #> 0.058 0.239 0.811 
    #> 
    #> Slope of critical when learning = 44.63 (-1 SD): 
    #>  Est.  S.E.     p 
    #> 0.093 0.340 0.788 
    #> 
    #> #######################################################
    #> While privileges (2nd moderator) = 65.369 (Mean of privileges +1 SD)
    #> #######################################################
    #> 
    #> The Johnson-Neyman interval could not be found. Is your interaction term significant?
    #> 
    #> SIMPLE SLOPES ANALYSIS
    #> 
    #> Slope of critical when learning = 68.104 (+1 SD): 
    #>   Est.   S.E.      p 
    #> -1.063  0.658  0.120 
    #> 
    #> Slope of critical when learning = 56.367 (Mean): 
    #>   Est.   S.E.      p 
    #> -0.560  0.502  0.276 
    #> 
    #> Slope of critical when learning = 44.63 (-1 SD): 
    #>   Est.   S.E.      p 
    #> -0.057  0.614  0.927

![](tools/README-probe_interaction_ex-1.png)

#### `svycor`

This function extends the `survey` package by calculating correlations with complex survey designs, a feature absent from `survey`. Users may request significance tests, which are calculated via bootstrap by calling the `weights` package.

``` r
library(survey)
data(api)
dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat, fpc=~fpc)

svycor(~api00+api99+dnum, design = dstrat, digits = 3, sig.stats = T, bootn = 2000)
```

    #>       api00  api99  dnum  
    #> api00 1      0.976* 0.254*
    #> api99 0.976* 1      0.244*
    #> dnum  0.254* 0.244* 1

#### `theme_apa`

This will format your `ggplot2` graphics to make them (mostly) appropriate for APA style publications. There's no drop-in, perfect way to get plots into APA format sight unseen, but this gets you close and returns a `ggplot` object that can be further tweaked to your specification. The plots produced by other functions in this package use `theme_apa`, but use its options to position the plots and alter other details to make them more in line with `ggplot2` defaults than APA norms.

You might start with something like the above interaction plots and then use `theme_apa` to tune it to APA specification. Note the `legend.pos` option:

``` r
p <- interact_plot(fitiris, pred = "Petal.Width", modx = "Species", plot.points = TRUE)
p + theme_apa(legend.pos = "topleft")
```

![](tools/README-theme_apa_ex-1.png)

Facet grids like those produced by `interact_plot` for 3-way interactions are also supported, though APA guidance on these kinds of constructions is less than clear. The embedded legend has a nice space-saving quality, though some trial and error may be needed before finding the ideal `legend.pos` argument.

``` r
p2 <- probe_interaction(fita3, pred = critical, modx = learning, mod2 = privileges)
p2 <- p2$interactplot
p2 + theme_apa(legend.pos = "topmiddle") 
```

![](tools/README-theme_apa_facet_ex-1.png)

You may need to make further changes to please your publisher, of course. Since these are regular `ggplot` theme changes, it shouldn't be a problem.

#### Tests for survey weight ignorability

In keeping with the package's attention to users of survey data, I've implemented a couple of tests that help to check whether your model is specified correctly without survey weights. It goes without saying that you shouldn't let statistical tests do your thinking for you, but they can provide useful info.

The first is `wgttest`, which implements the DuMouchel-Duncan (1983) procedure and is meant in part to duplicate the user-written Stata procedure of the same name. It can both test whether the model fit overall is changed with the addition of weights as well as show you which coefficients are most affected.

The next is `pf_sv_test`, short for Pfeffermann-Sverchkov (1999) test, which focuses on residual correlation with weights. You'll need the `boot` package for this one.

To run both at once, you can use `weights_tests`.

#### Others

`gscale`, `center_lm`, `scale_lm`, and `svysd` each do some of the behind the scenes computation in the above functions, but could do well for end users as well. See the documentation for more.

Details on the arguments can be accessed via the R documentation (`?functionname`). There are now vignettes documenting just about everything you can do as well.

Contributing
------------

I'm happy to receive bug reports, suggestions, questions, and (most of all) contributions to fix problems and add features. I prefer you use the Github issues system over trying to reach out to me in other ways. Pull requests for contributions are encouraged.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

License
-------

The source code of this package is licensed under the [MIT License](http://opensource.org/licenses/mit-license.php).
