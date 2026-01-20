# knitr methods for summ

There's no reason for end users to utilize these functions, but CRAN
requires it to be documented.

## Usage

``` r
# S3 method for class 'summ.lm'
knit_print(x, options = NULL, ...)

# S3 method for class 'summ.glm'
knit_print(x, options = NULL, ...)

# S3 method for class 'summ.svyglm'
knit_print(x, options = NULL, ...)

# S3 method for class 'summ.merMod'
knit_print(x, options = NULL, ...)

# S3 method for class 'summ.rq'
knit_print(x, options = NULL, ...)
```

## Arguments

- x:

  The `summ` object

- options:

  Chunk options.

- ...:

  Ignored.
