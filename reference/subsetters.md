# Subsetting operators

`%just%` and `%not%` are subsetting convenience functions for situations
when you would do `x[x %in% y]` or `x[x %nin% y]`. See details for
behavior when `x` is a data frame or matrix.

## Usage

``` r
x %not% y

x %not% y <- value

x %just% y

x %just% y <- value

# Default S3 method
x %not% y

# Default S3 method
x %not% y <- value

# S3 method for class 'data.frame'
x %not% y

# S3 method for class 'data.frame'
x %not% y <- value

# S3 method for class 'matrix'
x %not% y

# S3 method for class 'matrix'
x %not% y <- value

# S3 method for class 'list'
x %not% y

# S3 method for class 'list'
x %not% y <- value

# Default S3 method
x %just% y

# Default S3 method
x %just% y <- value

# S3 method for class 'data.frame'
x %just% y

# S3 method for class 'data.frame'
x %just% y <- value

# S3 method for class 'matrix'
x %just% y

# S3 method for class 'matrix'
x %just% y <- value

# S3 method for class 'list'
x %just% y

# S3 method for class 'list'
x %just% y <- value
```

## Arguments

- x:

  Object to subset

- y:

  List of items to include if they are/aren't in `x`

- value:

  The object(s) to assign to the subsetted `x`

## Value

All of `x` that are in `y` (`%just%`) or all of `x` that are not in `y`
(`%not%`).

## Details

The behavior of `%not%` and `%just%` are different when you're
subsetting data frames or matrices. The subset `y` in this case is
interpreted as **column** names or indices.

You can also make assignments to the subset in the same way you could if
subsetting with brackets.

## See also

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

Other subsetters: [`%nin%()`](nin.md)

## Examples

``` r
 x <- 1:5
 y <- 3:8
 
 x %just% y # 3 4 5
#> [1] 3 4 5
 x %not% y # 1 2
#> [1] 1 2

 # Assignment works too
 x %just% y <- NA # 1 2 NA NA NA
 x %not% y <- NA # NA NA 3 4 5
 
 mtcars %just% c("mpg", "qsec", "cyl") # keeps only columns with those names
#>                      mpg cyl  qsec
#> Mazda RX4           21.0   6 16.46
#> Mazda RX4 Wag       21.0   6 17.02
#> Datsun 710          22.8   4 18.61
#> Hornet 4 Drive      21.4   6 19.44
#> Hornet Sportabout   18.7   8 17.02
#> Valiant             18.1   6 20.22
#> Duster 360          14.3   8 15.84
#> Merc 240D           24.4   4 20.00
#> Merc 230            22.8   4 22.90
#> Merc 280            19.2   6 18.30
#> Merc 280C           17.8   6 18.90
#> Merc 450SE          16.4   8 17.40
#> Merc 450SL          17.3   8 17.60
#> Merc 450SLC         15.2   8 18.00
#> Cadillac Fleetwood  10.4   8 17.98
#> Lincoln Continental 10.4   8 17.82
#> Chrysler Imperial   14.7   8 17.42
#> Fiat 128            32.4   4 19.47
#> Honda Civic         30.4   4 18.52
#> Toyota Corolla      33.9   4 19.90
#> Toyota Corona       21.5   4 20.01
#> Dodge Challenger    15.5   8 16.87
#> AMC Javelin         15.2   8 17.30
#> Camaro Z28          13.3   8 15.41
#> Pontiac Firebird    19.2   8 17.05
#> Fiat X1-9           27.3   4 18.90
#> Porsche 914-2       26.0   4 16.70
#> Lotus Europa        30.4   4 16.90
#> Ford Pantera L      15.8   8 14.50
#> Ferrari Dino        19.7   6 15.50
#> Maserati Bora       15.0   8 14.60
#> Volvo 142E          21.4   4 18.60
 mtcars %not% 1:5 # drops columns 1 through 5
#>                        wt  qsec vs am gear carb
#> Mazda RX4           2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       2.875 17.02  0  1    4    4
#> Datsun 710          2.320 18.61  1  1    4    1
#> Hornet 4 Drive      3.215 19.44  1  0    3    1
#> Hornet Sportabout   3.440 17.02  0  0    3    2
#> Valiant             3.460 20.22  1  0    3    1
#> Duster 360          3.570 15.84  0  0    3    4
#> Merc 240D           3.190 20.00  1  0    4    2
#> Merc 230            3.150 22.90  1  0    4    2
#> Merc 280            3.440 18.30  1  0    4    4
#> Merc 280C           3.440 18.90  1  0    4    4
#> Merc 450SE          4.070 17.40  0  0    3    3
#> Merc 450SL          3.730 17.60  0  0    3    3
#> Merc 450SLC         3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  5.250 17.98  0  0    3    4
#> Lincoln Continental 5.424 17.82  0  0    3    4
#> Chrysler Imperial   5.345 17.42  0  0    3    4
#> Fiat 128            2.200 19.47  1  1    4    1
#> Honda Civic         1.615 18.52  1  1    4    2
#> Toyota Corolla      1.835 19.90  1  1    4    1
#> Toyota Corona       2.465 20.01  1  0    3    1
#> Dodge Challenger    3.520 16.87  0  0    3    2
#> AMC Javelin         3.435 17.30  0  0    3    2
#> Camaro Z28          3.840 15.41  0  0    3    4
#> Pontiac Firebird    3.845 17.05  0  0    3    2
#> Fiat X1-9           1.935 18.90  1  1    4    1
#> Porsche 914-2       2.140 16.70  0  1    5    2
#> Lotus Europa        1.513 16.90  1  1    5    2
#> Ford Pantera L      3.170 14.50  0  1    5    4
#> Ferrari Dino        2.770 15.50  0  1    5    6
#> Maserati Bora       3.570 14.60  0  1    5    8
#> Volvo 142E          2.780 18.60  1  1    4    2

 # Assignment works for data frames as well
 mtcars %just% c("mpg", "qsec") <- gscale(mtcars, c("mpg", "qsec"))
#> Warning: provided 11 variables to replace 2 variables
 mtcars %not% c("mpg", "qsec") <- gscale(mtcars %not% c("mpg", "qsec"))
 
 
```
