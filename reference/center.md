# Mean-center vectors, data frames, and survey designs

This function is a wrapper around [`gscale()`](gscale.md) that is
configured to mean-center variables without affecting the scaling of
those variables.

## Usage

``` r
center(
  data = NULL,
  vars = NULL,
  binary.inputs = "center",
  binary.factors = FALSE,
  weights = NULL
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

- weights:

  A vector of weights equal in length to `x`. If iterating over a data
  frame, the weights will need to be equal in length to all the columns
  to avoid errors. You may need to remove missing values before using
  the weights.

## Value

A transformed version of the `data` argument.

## Details

Some more information can be found in the documentation for
[`gscale()`](gscale.md)

## See also

standardization, scaling, and centering tools
[`center_mod()`](center_mod.md), [`gscale()`](gscale.md),
[`scale_mod()`](scale_mod.md), [`standardize()`](standardize.md)

## Examples

``` r
# Standardize just the "qsec" variable in mtcars
standardize(mtcars, vars = "qsec")
#>                      mpg cyl  disp  hp drat    wt        qsec vs am gear carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 -0.77716515  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 -0.46378082  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320  0.42600682  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215  0.89048716  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 -0.46378082  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460  1.32698675  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 -1.12412636  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190  1.20387148  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150  2.82675459  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440  0.25252621  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440  0.58829513  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 -0.25112717  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 -0.13920420  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780  0.08464175  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250  0.07344945  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 -0.01608893  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 -0.23993487  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200  0.90727560  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615  0.37564148  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835  1.14790999  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465  1.20946763  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 -0.54772305  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 -0.30708866  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 -1.36476075  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 -0.44699237  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935  0.58829513  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 -0.64285758  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 -0.53093460  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 -1.87401028  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 -1.31439542  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 -1.81804880  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780  0.42041067  1  1    4    2
```
