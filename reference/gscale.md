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
#>  [1] -0.42887747  0.31166952 -0.89289131  0.19495774  0.47550979  0.71120693
#>  [7] -0.61756360  0.08680595  0.08820427  0.07097818
# Normal standardization
gscale(x, n.sd = 1)
#>  [1] -0.8577549  0.6233390 -1.7857826  0.3899155  0.9510196  1.4224139
#>  [7] -1.2351272  0.1736119  0.1764085  0.1419564
# Scale only
gscale(x, scale.only = TRUE)
#>  [1]  0.26839830  1.00894529 -0.19561554  0.89223351  1.17278556  1.40848270
#>  [7]  0.07971218  0.78408172  0.78548004  0.76825396
# Center only
gscale(x, center.only = TRUE)
#>  [1] -0.9586790  0.6966816 -1.9958991  0.4357932  1.0629172  1.5897761
#>  [7] -1.3804532  0.1940392  0.1971649  0.1586591
# Binary inputs
gscale(x2, binary.inputs = "0/1")
#>  [1] 0 1 1 0 1 1 0 1 1 1
gscale(x2, binary.inputs = "full") # treats it like a continous var
#>  [1] -0.7245688  0.3105295  0.3105295 -0.7245688  0.3105295  0.3105295
#>  [7] -0.7245688  0.3105295  0.3105295  0.3105295
gscale(x2, binary.inputs = "-0.5/0.5") # keep scale, center at zero
#>  [1] -0.5  0.5  0.5 -0.5  0.5  0.5 -0.5  0.5  0.5  0.5
gscale(x2, binary.inputs = "center") # mean center it
#>  [1] -0.7  0.3  0.3 -0.7  0.3  0.3 -0.7  0.3  0.3  0.3

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
#>  [1] -0.43125470  0.26464991 -0.86729640  0.15497386  0.41861339  0.64010201
#>  [7] -0.60856626  0.05334179  0.05465581  0.03846817
# If using a weights column of data frame, give its name
mtcars$weights <- runif(32, 0, 1)
gscale(mtcars, weights = weights) # will skip over mtcars$weights
#>                             mpg         cyl         disp          hp
#> Mazda RX4            0.06424156  0.01615019 -0.259872325 -0.22222143
#> Mazda RX4 Wag        0.06424156  0.01615019 -0.259872325 -0.22222143
#> Datsun 710           0.21520704 -0.53517597 -0.470660639 -0.34614194
#> Hornet 4 Drive       0.09778944  0.01615019  0.137382575 -0.22222143
#> Hornet Sportabout   -0.12865877  0.56747635  0.550851960  0.25159228
#> Valiant             -0.17898060  0.01615019  0.003613068 -0.25866864
#> Duster 360          -0.49768550  0.56747635  0.550851960  0.76185320
#> Merc 240D            0.34939858 -0.53517597 -0.313785490 -0.57211463
#> Merc 230             0.21520704 -0.53517597 -0.337701856 -0.33156305
#> Merc 280            -0.08672392  0.01615019 -0.229064802 -0.12745869
#> Merc 280C           -0.20414151  0.01615019 -0.229064802 -0.12745869
#> Merc 450SE          -0.32155911  0.56747635  0.209537036  0.28803949
#> Merc 450SL          -0.24607637  0.56747635  0.209537036  0.28803949
#> Merc 450SLC         -0.42220276  0.56747635  0.209537036  0.28803949
#> Cadillac Fleetwood  -0.82477737  0.56747635  1.004857560  0.47027553
#> Lincoln Continental -0.82477737  0.56747635  0.956214103  0.54316995
#> Chrysler Imperial   -0.46413761  0.56747635  0.875141674  0.65251157
#> Fiat 128             1.02035626 -0.53517597 -0.589431747 -0.54295686
#> Honda Civic          0.85261684 -0.53517597 -0.601592611 -0.64500905
#> Toyota Corolla       1.14616082 -0.53517597 -0.620239269 -0.55024631
#> Toyota Corona        0.10617642 -0.53517597 -0.421611820 -0.31698417
#> Dodge Challenger    -0.39704185  0.56747635  0.380599860  0.06935624
#> AMC Javelin         -0.42220276  0.56747635  0.323849160  0.06935624
#> Camaro Z28          -0.58155521  0.56747635  0.510315746  0.76185320
#> Pontiac Firebird    -0.08672392  0.56747635  0.712996817  0.25159228
#> Fiat X1-9            0.59262074 -0.53517597 -0.588215660 -0.54295686
#> Porsche 914-2        0.48359011 -0.53517597 -0.420801095 -0.36072082
#> Lotus Europa         0.85261684 -0.53517597 -0.522952355 -0.20035310
#> Ford Pantera L      -0.37188093  0.56747635  0.514369367  0.90035259
#> Ferrari Dino        -0.04478906  0.01615019 -0.320676646  0.25159228
#> Maserati Bora       -0.43897670  0.56747635  0.311688296  1.41790295
#> Volvo 142E           0.09778944 -0.53517597 -0.417963560 -0.22951087
#>                            drat           wt        qsec         vs         am
#> Mazda RX4            0.30386980 -0.307380611 -0.41003825 -0.4819734  0.6191224
#> Mazda RX4 Wag        0.30386980 -0.172587401 -0.27285304 -0.4819734  0.6191224
#> Datsun 710           0.25426903 -0.465960859  0.11665499  0.5180266  0.6191224
#> Hornet 4 Drive      -0.50958280  0.007136879  0.31998307  0.5180266 -0.3808776
#> Hornet Sportabout   -0.44014172  0.126072065 -0.27285304 -0.4819734 -0.3808776
#> Valiant             -0.82702771  0.136644081  0.51106248  0.5180266 -0.3808776
#> Duster 360          -0.38062080  0.194790172 -0.56192188 -0.4819734 -0.3808776
#> Merc 240D            0.09554657 -0.006078141  0.45716829  0.5180266 -0.3808776
#> Merc 230             0.32371011 -0.027222174  1.16759173  0.5180266 -0.3808776
#> Merc 280             0.32371011  0.126072065  0.04071317  0.5180266 -0.3808776
#> Merc 280C            0.32371011  0.126072065  0.18769733  0.5180266 -0.3808776
#> Merc 450SE          -0.51950295  0.459090584 -0.17976307 -0.4819734 -0.3808776
#> Merc 450SL          -0.51950295  0.279366304 -0.13076835 -0.4819734 -0.3808776
#> Merc 450SLC         -0.51950295  0.305796345 -0.03277891 -0.4819734 -0.3808776
#> Cadillac Fleetwood  -0.65838510  1.082839558 -0.03767838 -0.4819734 -0.3808776
#> Lincoln Continental -0.58894402  1.174816101 -0.07687416 -0.4819734 -0.3808776
#> Chrysler Imperial   -0.36078049  1.133056636 -0.17486360 -0.4819734 -0.3808776
#> Fiat 128             0.48243256 -0.529392958  0.32733228  0.5180266  0.6191224
#> Honda Civic          1.32564562 -0.838624440  0.09460736  0.5180266  0.6191224
#> Toyota Corolla       0.62131471 -0.722332259  0.43267093  0.5180266  0.6191224
#> Toyota Corona        0.10546673 -0.389313739  0.45961802  0.5180266 -0.3808776
#> Dodge Challenger    -0.82702771  0.168360131 -0.30959908 -0.4819734 -0.3808776
#> AMC Javelin         -0.44014172  0.123429061 -0.20426043 -0.4819734 -0.3808776
#> Camaro Z28           0.13522719  0.337512395 -0.66726053 -0.4819734 -0.3808776
#> Pontiac Firebird    -0.50958280  0.340155399 -0.26550383 -0.4819734 -0.3808776
#> Fiat X1-9            0.48243256 -0.669472176  0.18769733  0.5180266  0.6191224
#> Porsche 914-2        0.82963794 -0.561109007 -0.35124459 -0.4819734  0.6191224
#> Lotus Europa         0.17490780 -0.892541725 -0.30224987  0.5180266  0.6191224
#> Ford Pantera L       0.62131471 -0.016650158 -0.89018651 -0.4819734  0.6191224
#> Ferrari Dino         0.02610550 -0.228090488 -0.64521291 -0.4819734  0.6191224
#> Maserati Bora       -0.05325573  0.194790172 -0.86568915 -0.4819734  0.6191224
#> Volvo 142E           0.51219302 -0.222804480  0.11420525  0.5180266  0.6191224
#>                           gear       carb     weights
#> Mazda RX4            0.1858306  0.4200398  0.09608644
#> Mazda RX4 Wag        0.1858306  0.4200398 -0.29173217
#> Datsun 710           0.1858306 -0.5246565  0.01377414
#> Hornet 4 Drive      -0.4754387 -0.5246565  0.05356828
#> Hornet Sportabout   -0.4754387 -0.2097577 -1.15332479
#> Valiant             -0.4754387 -0.5246565  0.27882770
#> Duster 360          -0.4754387  0.4200398  0.28754293
#> Merc 240D            0.1858306 -0.2097577  0.76031886
#> Merc 230             0.1858306 -0.2097577  0.71713156
#> Merc 280             0.1858306  0.4200398 -0.52628944
#> Merc 280C            0.1858306  0.4200398 -0.37228111
#> Merc 450SE          -0.4754387  0.1051410 -0.68444146
#> Merc 450SL          -0.4754387  0.1051410 -0.98509702
#> Merc 450SLC         -0.4754387  0.1051410 -0.22173057
#> Cadillac Fleetwood  -0.4754387  0.4200398 -0.30287280
#> Lincoln Continental -0.4754387  0.4200398  0.30814866
#> Chrysler Imperial   -0.4754387  0.4200398 -0.92199439
#> Fiat 128             0.1858306 -0.5246565  0.16717124
#> Honda Civic          0.1858306 -0.2097577 -1.21921995
#> Toyota Corolla       0.1858306 -0.5246565 -0.60109934
#> Toyota Corona       -0.4754387 -0.5246565  0.40630423
#> Dodge Challenger    -0.4754387 -0.2097577 -0.77304198
#> AMC Javelin         -0.4754387 -0.2097577 -0.13944428
#> Camaro Z28          -0.4754387  0.4200398 -0.64064262
#> Pontiac Firebird    -0.4754387 -0.2097577 -0.08336709
#> Fiat X1-9            0.1858306 -0.5246565 -0.94907346
#> Porsche 914-2        0.8470999 -0.2097577  0.66845681
#> Lotus Europa         0.8470999 -0.2097577 -0.19840193
#> Ford Pantera L       0.8470999  0.4200398 -0.19386109
#> Ferrari Dino         0.8470999  1.0498372 -0.76282040
#> Maserati Bora        0.8470999  1.6796347 -0.40326085
#> Volvo 142E           0.1858306 -0.2097577 -0.56408708
# If using a weights column of data frame, can still select variables
gscale(mtcars, vars = c("hp", "wt", "vs"), weights = weights)
#>                      mpg cyl  disp          hp drat           wt  qsec
#> Mazda RX4           21.0   6 160.0 -0.22222143 3.90 -0.307380611 16.46
#> Mazda RX4 Wag       21.0   6 160.0 -0.22222143 3.90 -0.172587401 17.02
#> Datsun 710          22.8   4 108.0 -0.34614194 3.85 -0.465960859 18.61
#> Hornet 4 Drive      21.4   6 258.0 -0.22222143 3.08  0.007136879 19.44
#> Hornet Sportabout   18.7   8 360.0  0.25159228 3.15  0.126072065 17.02
#> Valiant             18.1   6 225.0 -0.25866864 2.76  0.136644081 20.22
#> Duster 360          14.3   8 360.0  0.76185320 3.21  0.194790172 15.84
#> Merc 240D           24.4   4 146.7 -0.57211463 3.69 -0.006078141 20.00
#> Merc 230            22.8   4 140.8 -0.33156305 3.92 -0.027222174 22.90
#> Merc 280            19.2   6 167.6 -0.12745869 3.92  0.126072065 18.30
#> Merc 280C           17.8   6 167.6 -0.12745869 3.92  0.126072065 18.90
#> Merc 450SE          16.4   8 275.8  0.28803949 3.07  0.459090584 17.40
#> Merc 450SL          17.3   8 275.8  0.28803949 3.07  0.279366304 17.60
#> Merc 450SLC         15.2   8 275.8  0.28803949 3.07  0.305796345 18.00
#> Cadillac Fleetwood  10.4   8 472.0  0.47027553 2.93  1.082839558 17.98
#> Lincoln Continental 10.4   8 460.0  0.54316995 3.00  1.174816101 17.82
#> Chrysler Imperial   14.7   8 440.0  0.65251157 3.23  1.133056636 17.42
#> Fiat 128            32.4   4  78.7 -0.54295686 4.08 -0.529392958 19.47
#> Honda Civic         30.4   4  75.7 -0.64500905 4.93 -0.838624440 18.52
#> Toyota Corolla      33.9   4  71.1 -0.55024631 4.22 -0.722332259 19.90
#> Toyota Corona       21.5   4 120.1 -0.31698417 3.70 -0.389313739 20.01
#> Dodge Challenger    15.5   8 318.0  0.06935624 2.76  0.168360131 16.87
#> AMC Javelin         15.2   8 304.0  0.06935624 3.15  0.123429061 17.30
#> Camaro Z28          13.3   8 350.0  0.76185320 3.73  0.337512395 15.41
#> Pontiac Firebird    19.2   8 400.0  0.25159228 3.08  0.340155399 17.05
#> Fiat X1-9           27.3   4  79.0 -0.54295686 4.08 -0.669472176 18.90
#> Porsche 914-2       26.0   4 120.3 -0.36072082 4.43 -0.561109007 16.70
#> Lotus Europa        30.4   4  95.1 -0.20035310 3.77 -0.892541725 16.90
#> Ford Pantera L      15.8   8 351.0  0.90035259 4.22 -0.016650158 14.50
#> Ferrari Dino        19.7   6 145.0  0.25159228 3.62 -0.228090488 15.50
#> Maserati Bora       15.0   8 301.0  1.41790295 3.54  0.194790172 14.60
#> Volvo 142E          21.4   4 121.0 -0.22951087 4.11 -0.222804480 18.60
#>                             vs am gear carb    weights
#> Mazda RX4           -0.4819734  1    4    4 0.68016292
#> Mazda RX4 Wag       -0.4819734  1    4    4 0.49884561
#> Datsun 710           0.5180266  1    4    1 0.64167935
#> Hornet 4 Drive       0.5180266  0    3    1 0.66028435
#> Hornet Sportabout   -0.4819734  0    3    2 0.09602416
#> Valiant              0.5180266  0    3    1 0.76560016
#> Duster 360          -0.4819734  0    3    4 0.76967480
#> Merc 240D            0.5180266  0    4    2 0.99071231
#> Merc 230             0.5180266  0    4    2 0.97052090
#> Merc 280             0.5180266  0    4    4 0.38918276
#> Merc 280C            0.5180266  0    4    4 0.46118646
#> Merc 450SE          -0.4819734  0    3    3 0.31524175
#> Merc 450SL          -0.4819734  0    3    3 0.17467589
#> Merc 450SLC         -0.4819734  0    3    3 0.53157354
#> Cadillac Fleetwood  -0.4819734  0    3    4 0.49363702
#> Lincoln Continental -0.4819734  0    3    4 0.77930863
#> Chrysler Imperial   -0.4819734  0    3    4 0.20417834
#> Fiat 128             0.5180266  1    4    1 0.71339728
#> Honda Civic          0.5180266  1    4    2 0.06521611
#> Toyota Corolla       0.5180266  1    4    1 0.35420680
#> Toyota Corona        0.5180266  0    3    1 0.82519942
#> Dodge Challenger    -0.4819734  0    3    2 0.27381825
#> AMC Javelin         -0.4819734  0    3    2 0.57004495
#> Camaro Z28          -0.4819734  0    3    4 0.33571908
#> Pontiac Firebird    -0.4819734  0    3    2 0.59626279
#> Fiat X1-9            0.5180266  1    4    1 0.19151803
#> Porsche 914-2       -0.4819734  1    5    2 0.94776394
#> Lotus Europa         0.5180266  1    5    2 0.54248041
#> Ford Pantera L      -0.4819734  1    5    4 0.54460339
#> Ferrari Dino        -0.4819734  1    5    6 0.27859715
#> Maserati Bora       -0.4819734  1    5    8 0.44670247
#> Volvo 142E           0.5180266  1    4    2 0.37151118

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
