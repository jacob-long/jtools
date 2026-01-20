# Calculate correlations and correlation tables with complex survey data

The `survey` package is one of R’s best tools for those working in the
social sciences. For many, it saves you from needing to use commercial
software for research that uses survey data. However, it lacks one
function that many academic researchers often need to report in
publications: correlations. The `svycor` function in `jtools` helps to
fill that gap.

A note, however, is necessary. The initial motivation to add this
feature comes from a [response to a
question](https://stackoverflow.com/questions/34418822/pearson-correlation-coefficient-in-rs-survey-package#41031088)
about calculating correlations with the `survey` package written by
Thomas Lumley, the `survey` package author. All that is good about this
function should be attributed to Dr. Lumley; all that is wrong with it
should be attributed to me (Jacob).

With that said, let’s look at an example. First, we need to get a
`survey.design` object. This one is built into the `survey` package.

``` r
library(survey)
data(api)
dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat, fpc=~fpc)
```

## Basic use

The necessary arguments are no different than when using `svyvar`.
Specify, using an equation, which variables (and from which design) to
include. It doesn’t matter which side of the equation the variables are
on.

``` r
svycor(~api00 + api99, design = dstrat)
```

          api00 api99
    api00  1.00  0.98
    api99  0.98  1.00

You can specify with the `digits =` argument how many digits past the
decimal point should be printed.

``` r
svycor(~api00 + api99, design = dstrat, digits = 4)
```

           api00  api99
    api00 1.0000 0.9759
    api99 0.9759 1.0000

Any other arguments that you would normally pass to `svyvar` will be
used as well, though in some cases it may not affect the output.

## Statistical significance tests

One thing that `survey` won’t do for you is give you *p* values for the
null hypothesis that $r = 0$. While at first blush finding the *p* value
might seem like a simple procedure, complex surveys will almost always
violate the important distributional assumptions that go along with
simple hypothesis tests of the correlation coefficient. There is not a
clear consensus on the appropriate way to conduct hypothesis tests in
this context, due in part to the fact that most analyses of complex
surveys occurs in the context of multiple regression rather than simple
bivariate cases.

If `sig.stats = TRUE`, then `svycor` will use the `wtd.cor` function
from the `weights` package to conduct hypothesis tests. The *p* values
are derived from a bootstrap procedure in which the weights define
sampling probability. The `bootn =` argument is given to `wtd.cor` to
define the number of simulations to run. This can significantly increase
the running time for large samples and/or large numbers of simulations.
The `mean1` argument tells `wtd.cor` whether it should treat your sample
size as the number of observations in the survey design (the number of
rows in the data frame) or the sum of the weights. Usually, the former
is desired, so the default value of `mean1` is `TRUE`.

``` r
svycor(~api00 + api99, design = dstrat, digits = 4, sig.stats = TRUE, bootn = 2000, mean1 = TRUE)
```

          api00   api99  
    api00 1       0.9759*
    api99 0.9759* 1      

When using `sig.stats = TRUE`, the correlation parameter estimates come
from the bootstrap procedure rather than the simpler method based on the
survey-weighted covariance matrix when `sig.stats = FALSE`.

By saving the output of the function, you can extract non-rounded
coefficients, *p* values, and standard errors.

``` r
c <- svycor(~api00 + api99, design = dstrat, digits = 4, sig.stats = TRUE, bootn = 2000, mean1 = TRUE)

c$cors
```

              api00     api99
    api00 1.0000000 0.9759047
    api99 0.9759047 1.0000000

``` r
c$p.values
```

          api00 api99
    api00     0     0
    api99     0     0

``` r
c$std.err
```

                api00       api99
    api00 0.000000000 0.003471507
    api99 0.003471507 0.000000000

## Technical details

The heavy lifting behind the scenes is done by `svyvar`, which from its
output you may not realize also calculates covariance.

``` r
svyvar(~api00 + api99, design = dstrat)
```

          variance     SE
    api00    15191 1255.7
    api99    16518 1318.4

But if you save the `svyvar` object, you can see that there’s more than
meets the eye.

``` r
var <- svyvar(~api00 + api99, design = dstrat)
var <- as.matrix(var)
var
```

             api00    api99
    api00 15190.59 15458.83
    api99 15458.83 16518.24
    attr(,"var")
            api00   api00   api99   api99
    api00 1576883 1580654 1580654 1561998
    api00 1580654 1630856 1630856 1657352
    api99 1580654 1630856 1630856 1657352
    api99 1561998 1657352 1657352 1738266
    attr(,"statistic")
    [1] "variance"

Once we know that, it’s just a matter of using R’s `cov2cor` function
and cleaning up the output.

``` r
cor <- cov2cor(var)
cor
```

              api00     api99
    api00 1.0000000 0.9759047
    api99 0.9759047 1.0000000
    attr(,"var")
            api00   api00   api99   api99
    api00 1576883 1580654 1580654 1561998
    api00 1580654 1630856 1630856 1657352
    api99 1580654 1630856 1630856 1657352
    api99 1561998 1657352 1657352 1738266
    attr(,"statistic")
    [1] "variance"

Now to get rid of that covariance matrix…

``` r
cor <- cor[1:nrow(cor), 1:nrow(cor)]
cor
```

              api00     api99
    api00 1.0000000 0.9759047
    api99 0.9759047 1.0000000

`svycor` has its own print method, so you won’t see so many digits past
the decimal point. You can extract the un-rounded matrix, however.

``` r
out <- svycor(~api99 + api00, design = dstrat)
out$cors
```

              api99     api00
    api99 1.0000000 0.9759047
    api00 0.9759047 1.0000000
