# Scale and/or center data, including survey designs

`gscale` standardizes variables by dividing them by 2 standard
deviations and mean-centering them by default. It contains options for
handling binary variables separately. `gscale()` is a fork of `rescale`
from the `arm` package—the key feature difference is that `gscale()`
will perform the same functions for variables in
[`svydesign`](https://rdrr.io/pkg/survey/man/svydesign.html) objects.
`gscale()` is also more user-friendly in that it is more flexible in how
it accepts input.

## Usage

``` r
gscale(
  data = NULL,
  vars = NULL,
  binary.inputs = "center",
  binary.factors = FALSE,
  n.sd = 2,
  center.only = FALSE,
  scale.only = FALSE,
  weights = NULL,
  apply.weighted.contrasts = getOption("jtools-weighted.contrasts", FALSE),
  x = NULL,
  messages = FALSE
)
```

## Arguments

- data:

  A data frame or survey design. Only needed if you would like to
  rescale multiple variables at once. If `x = NULL`, all columns will be
  rescaled. Otherwise, `x` should be a vector of variable names. If `x`
  is a numeric vector, this argument is ignored.

- vars:

  If `data` is a data.frame or similar, you can scale only select
  columns by providing a vector column names to this argument.

- binary.inputs:

  Options for binary variables. Default is `center`; `0/1` keeps
  original scale; `-0.5/0.5` rescales 0 as -0.5 and 1 as 0.5; `center`
  subtracts the mean; and `full` subtracts the mean and divides by 2 sd.

- binary.factors:

  Coerce two-level factors to numeric and apply scaling functions to
  them? Default is FALSE.

- n.sd:

  By how many standard deviations should the variables be divided by?
  Default for `gscale` is 2, like `arm`'s `rescale`. 1 is the more
  typical standardization scheme.

- center.only:

  A logical value indicating whether you would like to mean -center the
  values, but not scale them.

- scale.only:

  A logical value indicating whether you would like to scale the values,
  but not mean-center them.

- weights:

  A vector of weights equal in length to `x`. If iterating over a data
  frame, the weights will need to be equal in length to all the columns
  to avoid errors. You may need to remove missing values before using
  the weights.

- apply.weighted.contrasts:

  Factor variables cannot be scaled, but you can set the contrasts such
  that the intercept in a regression model will reflect the true mean
  (assuming all other variables are centered). If set to TRUE, the
  argument will apply weighted effects coding to all factors. This is
  similar to the R default effects coding, but weights according to how
  many observations are at each level. An adapted version of
  `contr.wec()` from the `wec` package is used to do this. See that
  package's documentation and/or Grotenhuis et al. (2016) for more info.

- x:

  Deprecated. Pass numeric vectors to `data`. Pass vectors of column
  names to `vars`.

- messages:

  Print messages when variables are not processed due to being
  non-numeric or all missing? Default is FALSE.

## Details

This function is adapted from the `rescale` function of the `arm`
package. It is named `gscale()` after the popularizer of this scaling
method, Andrew **G**elman. By default, it works just like `rescale`. But
it contains many additional options and can also accept multiple types
of input without breaking a sweat.

Only numeric variables are altered when in a data.frame or survey
design. Character variables, factors, etc. are skipped.

For those dealing with survey data, if you provide a `survey.design`
object you can rest assured that the mean-centering and scaling is
performed with help from the
[`svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html) and
[`svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
functions, respectively. It was among the primary motivations for
creating this function. `gscale()` will not center or scale the weights
variables defined in the survey design unless the user specifically
requests them in the `x =` argument.

## References

Gelman, A. (2008). Scaling regression inputs by dividing by two standard
deviations. *Statistics in Medicine*, *27*, 2865–2873.
<https://sites.stat.columbia.edu/gelman/research/published/standardizing7.pdf>

Grotenhuis, M. te, Pelzer, B., Eisinga, R., Nieuwenhuis, R.,
Schmidt-Catran, A., & Konig, R. (2017). When size matters: Advantages of
weighted effect coding in observational studies. *International Journal
of Public Health*, *62*, 163–167.
https://doi.org/10.1007/s00038-016-0901-1 ( open access)

## See also

[`j_summ`](summ.md) is a replacement for the `summary` function for
regression models. On request, it will center and/or standardize
variables before printing its output.

standardization, scaling, and centering tools [`center()`](center.md),
[`center_mod()`](center_mod.md), [`scale_mod()`](scale_mod.md),
[`standardize()`](standardize.md)

## Author

Jacob Long <jacob.long@sc.edu>

## Examples

``` r
x <- rnorm(10, 2, 1)
x2 <- rbinom(10, 1, .5)

# Basic use
gscale(x)
#>  [1] -0.59904299  0.03907175  0.04033855  0.02473279 -0.08509641  0.39422409
#>  [7]  0.97622763 -0.52170357  0.34698631 -0.61573814
# Normal standardization
gscale(x, n.sd = 1)
#>  [1] -1.19808598  0.07814351  0.08067709  0.04946558 -0.17019281  0.78844818
#>  [7]  1.95245525 -1.04340714  0.69397262 -1.23147629
# Scale only
gscale(x, scale.only = TRUE)
#>  [1] 0.07221425 0.71032899 0.71159578 0.69599002 0.58616083 1.06548133
#>  [7] 1.64748486 0.14955367 1.01824355 0.05551909
# Center only
gscale(x, center.only = TRUE)
#>  [1] -1.47808622  0.09640614  0.09953183  0.06102599 -0.20996794  0.97271348
#>  [7]  2.40875634 -1.28725796  0.85615839 -1.51928005
# Binary inputs
gscale(x2, binary.inputs = "0/1")
#>  [1] 0 1 0 0 1 1 0 0 1 0
gscale(x2, binary.inputs = "full") # treats it like a continous var
#>  [1] -0.3872983  0.5809475 -0.3872983 -0.3872983  0.5809475  0.5809475
#>  [7] -0.3872983 -0.3872983  0.5809475 -0.3872983
gscale(x2, binary.inputs = "-0.5/0.5") # keep scale, center at zero
#>  [1] -0.5  0.5 -0.5 -0.5  0.5  0.5 -0.5 -0.5  0.5 -0.5
gscale(x2, binary.inputs = "center") # mean center it
#>  [1] -0.4  0.6 -0.4 -0.4  0.6  0.6 -0.4 -0.4  0.6 -0.4

# Data frame as input
# loops through each numeric column
gscale(data = mtcars, binary.inputs = "-0.5/0.5")
#>                             mpg        cyl        disp          hp        drat
#> Mazda RX4            0.07544241 -0.0524939 -0.28530991 -0.26754642  0.28375684
#> Mazda RX4 Wag        0.07544241 -0.0524939 -0.28530991 -0.26754642  0.28375684
#> Datsun 710           0.22477172 -0.6124289 -0.49509105 -0.39152023  0.23699979
#> Hornet 4 Drive       0.10862670 -0.0524939  0.11004685 -0.26754642 -0.48305877
#> Hornet Sportabout   -0.11536726  0.5074411  0.52154061  0.20647109 -0.41759890
#> Valiant             -0.16514370 -0.0524939 -0.02308349 -0.30400931 -0.78230388
#> Duster 360          -0.48039447  0.5074411  0.52154061  0.71695148 -0.36149044
#> Merc 240D            0.35750889 -0.6124289 -0.33896547 -0.61759012  0.08737724
#> Merc 230             0.22477172 -0.6124289 -0.36276756 -0.37693508  0.30245966
#> Merc 280            -0.07388690 -0.0524939 -0.25464959 -0.17274292  0.30245966
#> Merc 280C           -0.19003192 -0.0524939 -0.25464959 -0.17274292  0.30245966
#> Merc 450SE          -0.30617694  0.5074411  0.18185654  0.24293397 -0.49241018
#> Merc 450SL          -0.23151228  0.5074411  0.18185654  0.24293397 -0.49241018
#> Merc 450SLC         -0.40572981  0.5074411  0.18185654  0.24293397 -0.49241018
#> Cadillac Fleetwood  -0.80394131  0.5074411  0.97337691  0.42524840 -0.62332991
#> Lincoln Continental -0.80394131  0.5074411  0.92496588  0.49817417 -0.55787004
#> Chrysler Imperial   -0.44721018  0.5074411  0.84428082  0.60756282 -0.34278762
#> Fiat 128             1.02119472 -0.6124289 -0.61329465 -0.58841981  0.45208222
#> Honda Civic          0.85527326 -0.6124289 -0.62539740 -0.69051589  1.24695206
#> Toyota Corolla       1.14563581 -0.6124289 -0.64395497 -0.59571239  0.58300196
#> Toyota Corona        0.11692278 -0.6124289 -0.44627659 -0.36234992  0.09672865
#> Dodge Challenger    -0.38084159  0.5074411  0.35210200  0.02415666 -0.78230388
#> AMC Javelin         -0.40572981  0.5074411  0.29562247  0.02415666 -0.41759890
#> Camaro Z28          -0.56335520  0.5074411  0.48119809  0.71695148  0.12478288
#> Pontiac Firebird    -0.07388690  0.5074411  0.68291072  0.20647109 -0.48305877
#> Fiat X1-9            0.59809500 -0.6124289 -0.61208437 -0.58841981  0.45208222
#> Porsche 914-2        0.49024605 -0.6124289 -0.44546974 -0.40610538  0.77938156
#> Lotus Europa         0.85527326 -0.6124289 -0.54713290 -0.24566869  0.16218851
#> Ford Pantera L      -0.35595337  0.5074411  0.48523234  0.85551044  0.58300196
#> Ferrari Dino        -0.03240653 -0.0524939 -0.34582370  0.20647109  0.02191737
#> Maserati Bora       -0.42232196  0.5074411  0.28351971  1.37328341 -0.05289391
#> Volvo 142E           0.10862670 -0.6124289 -0.44264576 -0.27483900  0.48013645
#>                               wt         qsec   vs   am       gear        carb
#> Mazda RX4           -0.305199784 -0.388582573 -0.5  0.5  0.2117771  0.36760154
#> Mazda RX4 Wag       -0.174892635 -0.231890410 -0.5  0.5  0.2117771  0.36760154
#> Datsun 710          -0.458502312  0.213003408  0.5  0.5  0.2117771 -0.56107604
#> Hornet 4 Drive      -0.001149769  0.445243578  0.5 -0.5 -0.4659096 -0.56107604
#> Hornet Sportabout    0.113827127 -0.231890410 -0.5 -0.5 -0.4659096 -0.25151684
#> Valiant              0.124047296  0.663493376  0.5 -0.5 -0.4659096 -0.56107604
#> Duster 360           0.180258223 -0.562063181 -0.5 -0.5 -0.4659096  0.36760154
#> Merc 240D           -0.013924980  0.601935740  0.5 -0.5  0.2117771 -0.25151684
#> Merc 230            -0.034365317  1.413377296  0.5 -0.5  0.2117771 -0.25151684
#> Merc 280             0.113827127  0.126263104  0.5 -0.5  0.2117771  0.36760154
#> Merc 280C            0.113827127  0.294147564  0.5 -0.5  0.2117771  0.36760154
#> Merc 450SE           0.435762437 -0.125563586 -0.5 -0.5 -0.4659096  0.05804235
#> Merc 450SL           0.262019572 -0.069602099 -0.5 -0.5 -0.4659096  0.05804235
#> Merc 450SLC          0.287569993  0.042320874 -0.5 -0.5 -0.4659096  0.05804235
#> Cadillac Fleetwood   1.038752382  0.036724726 -0.5 -0.5 -0.4659096  0.36760154
#> Lincoln Continental  1.127667849 -0.008044464 -0.5 -0.5 -0.4659096  0.36760154
#> Chrysler Imperial    1.087298183 -0.119967437 -0.5 -0.5 -0.4659096  0.36760154
#> Fiat 128            -0.519823324  0.453637801  0.5  0.5  0.2117771 -0.56107604
#> Honda Civic         -0.818763254  0.187820739  0.5  0.5  0.2117771 -0.25151684
#> Toyota Corolla      -0.706341400  0.573954997  0.5  0.5  0.2117771 -0.56107604
#> Toyota Corona       -0.384406090  0.604733815  0.5 -0.5 -0.4659096 -0.56107604
#> Dodge Challenger     0.154707802 -0.273861525 -0.5 -0.5 -0.4659096 -0.25151684
#> AMC Javelin          0.111272085 -0.153544329 -0.5 -0.5 -0.4659096 -0.25151684
#> Camaro Z28           0.318230499 -0.682380377 -0.5 -0.5 -0.4659096  0.36760154
#> Pontiac Firebird     0.320785541 -0.223496187 -0.5 -0.5 -0.4659096 -0.25151684
#> Fiat X1-9           -0.655240557  0.294147564  0.5  0.5  0.2117771 -0.56107604
#> Porsche 914-2       -0.550483829 -0.321428789 -0.5  0.5  0.8894638 -0.25151684
#> Lotus Europa        -0.870886114 -0.265467302  0.5  0.5  0.8894638 -0.25151684
#> Ford Pantera L      -0.024145148 -0.937005142 -0.5  0.5  0.8894638  0.36760154
#> Ferrari Dino        -0.228548520 -0.657197709 -0.5  0.5  0.8894638  0.98671992
#> Maserati Bora        0.180258223 -0.909024398 -0.5  0.5  0.8894638  1.60583831
#> Volvo 142E          -0.223438435  0.210205334  0.5  0.5  0.2117771 -0.25151684

# Specified vars in data frame
gscale(mtcars, vars = c("hp", "wt", "vs"), binary.inputs = "center")
#>                      mpg cyl  disp          hp drat           wt  qsec      vs
#> Mazda RX4           21.0   6 160.0 -0.26754642 3.90 -0.305199784 16.46 -0.4375
#> Mazda RX4 Wag       21.0   6 160.0 -0.26754642 3.90 -0.174892635 17.02 -0.4375
#> Datsun 710          22.8   4 108.0 -0.39152023 3.85 -0.458502312 18.61  0.5625
#> Hornet 4 Drive      21.4   6 258.0 -0.26754642 3.08 -0.001149769 19.44  0.5625
#> Hornet Sportabout   18.7   8 360.0  0.20647109 3.15  0.113827127 17.02 -0.4375
#> Valiant             18.1   6 225.0 -0.30400931 2.76  0.124047296 20.22  0.5625
#> Duster 360          14.3   8 360.0  0.71695148 3.21  0.180258223 15.84 -0.4375
#> Merc 240D           24.4   4 146.7 -0.61759012 3.69 -0.013924980 20.00  0.5625
#> Merc 230            22.8   4 140.8 -0.37693508 3.92 -0.034365317 22.90  0.5625
#> Merc 280            19.2   6 167.6 -0.17274292 3.92  0.113827127 18.30  0.5625
#> Merc 280C           17.8   6 167.6 -0.17274292 3.92  0.113827127 18.90  0.5625
#> Merc 450SE          16.4   8 275.8  0.24293397 3.07  0.435762437 17.40 -0.4375
#> Merc 450SL          17.3   8 275.8  0.24293397 3.07  0.262019572 17.60 -0.4375
#> Merc 450SLC         15.2   8 275.8  0.24293397 3.07  0.287569993 18.00 -0.4375
#> Cadillac Fleetwood  10.4   8 472.0  0.42524840 2.93  1.038752382 17.98 -0.4375
#> Lincoln Continental 10.4   8 460.0  0.49817417 3.00  1.127667849 17.82 -0.4375
#> Chrysler Imperial   14.7   8 440.0  0.60756282 3.23  1.087298183 17.42 -0.4375
#> Fiat 128            32.4   4  78.7 -0.58841981 4.08 -0.519823324 19.47  0.5625
#> Honda Civic         30.4   4  75.7 -0.69051589 4.93 -0.818763254 18.52  0.5625
#> Toyota Corolla      33.9   4  71.1 -0.59571239 4.22 -0.706341400 19.90  0.5625
#> Toyota Corona       21.5   4 120.1 -0.36234992 3.70 -0.384406090 20.01  0.5625
#> Dodge Challenger    15.5   8 318.0  0.02415666 2.76  0.154707802 16.87 -0.4375
#> AMC Javelin         15.2   8 304.0  0.02415666 3.15  0.111272085 17.30 -0.4375
#> Camaro Z28          13.3   8 350.0  0.71695148 3.73  0.318230499 15.41 -0.4375
#> Pontiac Firebird    19.2   8 400.0  0.20647109 3.08  0.320785541 17.05 -0.4375
#> Fiat X1-9           27.3   4  79.0 -0.58841981 4.08 -0.655240557 18.90  0.5625
#> Porsche 914-2       26.0   4 120.3 -0.40610538 4.43 -0.550483829 16.70 -0.4375
#> Lotus Europa        30.4   4  95.1 -0.24566869 3.77 -0.870886114 16.90  0.5625
#> Ford Pantera L      15.8   8 351.0  0.85551044 4.22 -0.024145148 14.50 -0.4375
#> Ferrari Dino        19.7   6 145.0  0.20647109 3.62 -0.228548520 15.50 -0.4375
#> Maserati Bora       15.0   8 301.0  1.37328341 3.54  0.180258223 14.60 -0.4375
#> Volvo 142E          21.4   4 121.0 -0.27483900 4.11 -0.223438435 18.60  0.5625
#>                     am gear carb
#> Mazda RX4            1    4    4
#> Mazda RX4 Wag        1    4    4
#> Datsun 710           1    4    1
#> Hornet 4 Drive       0    3    1
#> Hornet Sportabout    0    3    2
#> Valiant              0    3    1
#> Duster 360           0    3    4
#> Merc 240D            0    4    2
#> Merc 230             0    4    2
#> Merc 280             0    4    4
#> Merc 280C            0    4    4
#> Merc 450SE           0    3    3
#> Merc 450SL           0    3    3
#> Merc 450SLC          0    3    3
#> Cadillac Fleetwood   0    3    4
#> Lincoln Continental  0    3    4
#> Chrysler Imperial    0    3    4
#> Fiat 128             1    4    1
#> Honda Civic          1    4    2
#> Toyota Corolla       1    4    1
#> Toyota Corona        0    3    1
#> Dodge Challenger     0    3    2
#> AMC Javelin          0    3    2
#> Camaro Z28           0    3    4
#> Pontiac Firebird     0    3    2
#> Fiat X1-9            1    4    1
#> Porsche 914-2        1    5    2
#> Lotus Europa         1    5    2
#> Ford Pantera L       1    5    4
#> Ferrari Dino         1    5    6
#> Maserati Bora        1    5    8
#> Volvo 142E           1    4    2

# Weighted inputs

wts <- runif(10, 0, 1)
gscale(x, weights = wts)
#>  [1] -0.65254895 -0.06975260 -0.06859563 -0.08284852 -0.18315658  0.25461145
#>  [7]  0.78616089 -0.58191411  0.21146873 -0.66779679
# If using a weights column of data frame, give its name
mtcars$weights <- runif(32, 0, 1)
gscale(mtcars, weights = weights) # will skip over mtcars$weights
#>                             mpg         cyl        disp          hp        drat
#> Mazda RX4            0.07248094 -0.02324112 -0.27436951 -0.23720829  0.26907362
#> Mazda RX4 Wag        0.07248094 -0.02324112 -0.27436951 -0.23720829  0.26907362
#> Datsun 710           0.21844884 -0.58406280 -0.47014808 -0.36914555  0.22684412
#> Hornet 4 Drive       0.10491825 -0.02324112  0.09459779 -0.23720829 -0.42349015
#> Hornet Sportabout   -0.11403359  0.53758057  0.47862498  0.26725770 -0.36436886
#> Valiant             -0.16268955 -0.02324112 -0.02964630 -0.27601336 -0.69375894
#> Duster 360          -0.47084399  0.53758057  0.47862498  0.81052877 -0.31369346
#> Merc 240D            0.34819808 -0.58406280 -0.32444365 -0.60973702  0.09170973
#> Merc 230             0.21844884 -0.58406280 -0.34665699 -0.35362352  0.28596542
#> Merc 280            -0.07348695 -0.02324112 -0.24575572 -0.13631509  0.28596542
#> Merc 280C           -0.18701753 -0.02324112 -0.24575572 -0.13631509  0.28596542
#> Merc 450SE          -0.30054812  0.53758057  0.16161430  0.30606278 -0.43193605
#> Merc 450SL          -0.22756417  0.53758057  0.16161430  0.30606278 -0.43193605
#> Merc 450SLC         -0.39786005  0.53758057  0.16161430  0.30606278 -0.43193605
#> Cadillac Fleetwood  -0.78710776  0.53758057  0.90030190  0.50008816 -0.55017865
#> Lincoln Continental -0.78710776  0.53758057  0.85512223  0.57769831 -0.49105735
#> Chrysler Imperial   -0.43840668  0.53758057  0.77982278  0.69411354 -0.29680166
#> Fiat 128             0.99694427 -0.58406280 -0.58046178 -0.57869296  0.42109981
#> Honda Civic          0.83475772 -0.58406280 -0.59175669 -0.68734717  1.13900128
#> Toyota Corolla       1.11858418 -0.58406280 -0.60907557 -0.58645397  0.53934241
#> Toyota Corona        0.11302758 -0.58406280 -0.42459192 -0.33810149  0.10015563
#> Dodge Challenger    -0.37353206  0.53758057  0.32049614  0.07323232 -0.69375894
#> AMC Javelin         -0.39786005  0.53758057  0.26778653  0.07323232 -0.36436886
#> Camaro Z28          -0.55193727  0.53758057  0.44097526  0.81052877  0.12549332
#> Pontiac Firebird    -0.07348695  0.53758057  0.62922388  0.26725770 -0.42349015
#> Fiat X1-9            0.58336857 -0.58406280 -0.57933228 -0.57869296  0.42109981
#> Porsche 914-2        0.47794732 -0.58406280 -0.42383892 -0.38466758  0.71670630
#> Lotus Europa         0.83475772 -0.58406280 -0.51871623 -0.21392524  0.15927692
#> Ford Pantera L      -0.34920408  0.53758057  0.44474023  0.95798806  0.53934241
#> Ferrari Dino        -0.03294031 -0.02324112 -0.33084410  0.26725770  0.03258843
#> Maserati Bora       -0.41407870  0.53758057  0.25649161  1.50902014 -0.03497877
#> Volvo 142E           0.10491825 -0.58406280 -0.42120344 -0.24496930  0.44643751
#>                              wt        qsec         vs         am       gear
#> Mazda RX4           -0.30232736 -0.42822384 -0.4548393  0.5873491  0.1632857
#> Mazda RX4 Wag       -0.18618808 -0.28230225 -0.4548393  0.5873491  0.1632857
#> Datsun 710          -0.43896180  0.13201081  0.5451607  0.5873491  0.1632857
#> Hornet 4 Drive      -0.03133571  0.34828744  0.5451607 -0.4126509 -0.4772477
#> Hornet Sportabout    0.07114013 -0.28230225 -0.4548393 -0.4126509 -0.4772477
#> Valiant              0.08024909  0.55153536  0.5451607 -0.4126509 -0.4772477
#> Duster 360           0.13034839 -0.58977987 -0.4548393 -0.4126509 -0.4772477
#> Merc 240D           -0.04272191  0.49420903  0.5451607 -0.4126509  0.1632857
#> Merc 230            -0.06093983  1.24987436  0.5451607 -0.4126509  0.1632857
#> Merc 280             0.07114013  0.05123279  0.5451607 -0.4126509  0.1632857
#> Merc 280C            0.07114013  0.20757735  0.5451607 -0.4126509  0.1632857
#> Merc 450SE           0.35807246 -0.18328404 -0.4548393 -0.4126509 -0.4772477
#> Merc 450SL           0.20322009 -0.13116918 -0.4548393 -0.4126509 -0.4772477
#> Merc 450SLC          0.22599250 -0.02693948 -0.4548393 -0.4126509 -0.4772477
#> Cadillac Fleetwood   0.89550128 -0.03215097 -0.4548393 -0.4126509 -0.4772477
#> Lincoln Continental  0.97474926 -0.07384285 -0.4548393 -0.4126509 -0.4772477
#> Chrysler Imperial    0.93876886 -0.17807255 -0.4548393 -0.4126509 -0.4772477
#> Fiat 128            -0.49361558  0.35610467  0.5451607  0.5873491  0.1632857
#> Honda Civic         -0.76005275  0.10855913  0.5451607  0.5873491  0.1632857
#> Toyota Corolla      -0.65985415  0.46815160  0.5451607  0.5873491  0.1632857
#> Toyota Corona       -0.37292182  0.49681477  0.5451607 -0.4126509 -0.4772477
#> Dodge Challenger     0.10757598 -0.32138839 -0.4548393 -0.4126509 -0.4772477
#> AMC Javelin          0.06886289 -0.20934146 -0.4548393 -0.4126509 -0.4772477
#> Camaro Z28           0.25331939 -0.70182680 -0.4548393 -0.4126509 -0.4772477
#> Pontiac Firebird     0.25559663 -0.27448502 -0.4548393 -0.4126509 -0.4772477
#> Fiat X1-9           -0.61430934  0.20757735  0.5451607  0.5873491  0.1632857
#> Porsche 914-2       -0.52094247 -0.36568601 -0.4548393  0.5873491  0.8038191
#> Lotus Europa        -0.80650846 -0.31357116  0.5451607  0.5873491  0.8038191
#> Ford Pantera L      -0.05183087 -0.93894937 -0.4548393  0.5873491  0.8038191
#> Ferrari Dino        -0.23401013 -0.67837512 -0.4548393  0.5873491  0.8038191
#> Maserati Bora        0.13034839 -0.91289195 -0.4548393  0.5873491  0.8038191
#> Volvo 142E          -0.22945565  0.12940507  0.5451607  0.5873491  0.1632857
#>                            carb       weights
#> Mazda RX4            0.39985035 -0.8792090226
#> Mazda RX4 Wag        0.39985035 -0.0638528343
#> Datsun 710          -0.54700748 -0.1505213245
#> Hornet 4 Drive      -0.54700748  0.5021142898
#> Hornet Sportabout   -0.23138820 -0.8118087344
#> Valiant             -0.54700748  0.3515354750
#> Duster 360           0.39985035 -1.1292771069
#> Merc 240D           -0.23138820 -0.4690588420
#> Merc 230            -0.23138820  0.6069548347
#> Merc 280             0.39985035 -0.6527117893
#> Merc 280C            0.39985035  0.0240376372
#> Merc 450SE           0.08423108 -0.5112952523
#> Merc 450SL           0.08423108  0.0839340150
#> Merc 450SLC          0.08423108 -0.8407320497
#> Cadillac Fleetwood   0.39985035  0.8869615483
#> Lincoln Continental  0.39985035 -0.0389353759
#> Chrysler Imperial    0.39985035 -0.0340852774
#> Fiat 128            -0.54700748 -0.6417940573
#> Honda Civic         -0.23138820 -0.2577463891
#> Toyota Corolla      -0.54700748 -0.4295258332
#> Toyota Corona       -0.54700748 -1.2141604662
#> Dodge Challenger    -0.23138820 -0.2136891719
#> AMC Javelin         -0.23138820 -0.3872152093
#> Camaro Z28           0.39985035 -1.2324272989
#> Pontiac Firebird    -0.23138820 -0.4170526898
#> Fiat X1-9           -0.54700748  0.0008901617
#> Porsche 914-2       -0.23138820  0.6797963103
#> Lotus Europa        -0.23138820 -0.3991444729
#> Ford Pantera L       0.39985035 -0.0722063612
#> Ferrari Dino         1.03108891  0.0939283850
#> Maserati Bora        1.66232747 -0.6811475224
#> Volvo 142E          -0.23138820 -0.6156289069
# If using a weights column of data frame, can still select variables
gscale(mtcars, vars = c("hp", "wt", "vs"), weights = weights)
#>                      mpg cyl  disp          hp drat          wt  qsec
#> Mazda RX4           21.0   6 160.0 -0.23720829 3.90 -0.30232736 16.46
#> Mazda RX4 Wag       21.0   6 160.0 -0.23720829 3.90 -0.18618808 17.02
#> Datsun 710          22.8   4 108.0 -0.36914555 3.85 -0.43896180 18.61
#> Hornet 4 Drive      21.4   6 258.0 -0.23720829 3.08 -0.03133571 19.44
#> Hornet Sportabout   18.7   8 360.0  0.26725770 3.15  0.07114013 17.02
#> Valiant             18.1   6 225.0 -0.27601336 2.76  0.08024909 20.22
#> Duster 360          14.3   8 360.0  0.81052877 3.21  0.13034839 15.84
#> Merc 240D           24.4   4 146.7 -0.60973702 3.69 -0.04272191 20.00
#> Merc 230            22.8   4 140.8 -0.35362352 3.92 -0.06093983 22.90
#> Merc 280            19.2   6 167.6 -0.13631509 3.92  0.07114013 18.30
#> Merc 280C           17.8   6 167.6 -0.13631509 3.92  0.07114013 18.90
#> Merc 450SE          16.4   8 275.8  0.30606278 3.07  0.35807246 17.40
#> Merc 450SL          17.3   8 275.8  0.30606278 3.07  0.20322009 17.60
#> Merc 450SLC         15.2   8 275.8  0.30606278 3.07  0.22599250 18.00
#> Cadillac Fleetwood  10.4   8 472.0  0.50008816 2.93  0.89550128 17.98
#> Lincoln Continental 10.4   8 460.0  0.57769831 3.00  0.97474926 17.82
#> Chrysler Imperial   14.7   8 440.0  0.69411354 3.23  0.93876886 17.42
#> Fiat 128            32.4   4  78.7 -0.57869296 4.08 -0.49361558 19.47
#> Honda Civic         30.4   4  75.7 -0.68734717 4.93 -0.76005275 18.52
#> Toyota Corolla      33.9   4  71.1 -0.58645397 4.22 -0.65985415 19.90
#> Toyota Corona       21.5   4 120.1 -0.33810149 3.70 -0.37292182 20.01
#> Dodge Challenger    15.5   8 318.0  0.07323232 2.76  0.10757598 16.87
#> AMC Javelin         15.2   8 304.0  0.07323232 3.15  0.06886289 17.30
#> Camaro Z28          13.3   8 350.0  0.81052877 3.73  0.25331939 15.41
#> Pontiac Firebird    19.2   8 400.0  0.26725770 3.08  0.25559663 17.05
#> Fiat X1-9           27.3   4  79.0 -0.57869296 4.08 -0.61430934 18.90
#> Porsche 914-2       26.0   4 120.3 -0.38466758 4.43 -0.52094247 16.70
#> Lotus Europa        30.4   4  95.1 -0.21392524 3.77 -0.80650846 16.90
#> Ford Pantera L      15.8   8 351.0  0.95798806 4.22 -0.05183087 14.50
#> Ferrari Dino        19.7   6 145.0  0.26725770 3.62 -0.23401013 15.50
#> Maserati Bora       15.0   8 301.0  1.50902014 3.54  0.13034839 14.60
#> Volvo 142E          21.4   4 121.0 -0.24496930 4.11 -0.22945565 18.60
#>                             vs am gear carb    weights
#> Mazda RX4           -0.4548393  1    4    4 0.17467589
#> Mazda RX4 Wag       -0.4548393  1    4    4 0.53157354
#> Datsun 710           0.5451607  1    4    1 0.49363702
#> Hornet 4 Drive       0.5451607  0    3    1 0.77930863
#> Hornet Sportabout   -0.4548393  0    3    2 0.20417834
#> Valiant              0.5451607  0    3    1 0.71339728
#> Duster 360          -0.4548393  0    3    4 0.06521611
#> Merc 240D            0.5451607  0    4    2 0.35420680
#> Merc 230             0.5451607  0    4    2 0.82519942
#> Merc 280             0.5451607  0    4    4 0.27381825
#> Merc 280C            0.5451607  0    4    4 0.57004495
#> Merc 450SE          -0.4548393  0    3    3 0.33571908
#> Merc 450SL          -0.4548393  0    3    3 0.59626279
#> Merc 450SLC         -0.4548393  0    3    3 0.19151803
#> Cadillac Fleetwood  -0.4548393  0    3    4 0.94776394
#> Lincoln Continental -0.4548393  0    3    4 0.54248041
#> Chrysler Imperial   -0.4548393  0    3    4 0.54460339
#> Fiat 128             0.5451607  1    4    1 0.27859715
#> Honda Civic          0.5451607  1    4    2 0.44670247
#> Toyota Corolla       0.5451607  1    4    1 0.37151118
#> Toyota Corona        0.5451607  0    3    1 0.02806097
#> Dodge Challenger    -0.4548393  0    3    2 0.46598719
#> AMC Javelin         -0.4548393  0    3    2 0.39003139
#> Camaro Z28          -0.4548393  0    3    4 0.02006522
#> Pontiac Firebird    -0.4548393  0    3    2 0.37697093
#> Fiat X1-9            0.5451607  1    4    1 0.55991284
#> Porsche 914-2       -0.4548393  1    5    2 0.85708359
#> Lotus Europa         0.5451607  1    5    2 0.38480971
#> Ford Pantera L      -0.4548393  1    5    4 0.52791704
#> Ferrari Dino        -0.4548393  1    5    6 0.60063752
#> Maserati Bora       -0.4548393  1    5    8 0.26137136
#> Volvo 142E           0.5451607  1    4    2 0.29005016

# Survey designs
if (requireNamespace("survey")) {
  library(survey)
  data(api)
  ## Create survey design object
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                       data = apistrat, fpc=~fpc)
  # Creating test binary variable
  dstrat$variables$binary <- rbinom(200, 1, 0.5)

  gscale(data = dstrat, binary.inputs = "-0.5/0.5")
  gscale(data = dstrat, vars = c("api00","meals","binary"),
         binary.inputs = "-0.5/0.5")
}
#> Stratified Independent Sampling design
#> svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat, 
#>     fpc = ~fpc)


```
