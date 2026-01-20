# Broom extensions for summ objects

These are functions used for compatibility with broom's tidying
functions to facilitate use with huxreg, thereby making
[`export_summs`](export_summs.md) works.

## Usage

``` r
# S3 method for class 'summ'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'summ.merMod'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'summ.lm'
glance(x, ...)

# S3 method for class 'summ.glm'
glance(x, ...)

# S3 method for class 'summ.svyglm'
glance(x, ...)

# S3 method for class 'summ.merMod'
glance(x, ...)

# S3 method for class 'summ.rq'
glance(x, ...)
```

## Arguments

- x:

  The `summ` object.

- conf.int:

  Include confidence intervals? Default is FALSE.

- conf.level:

  How wide confidence intervals should be, if requested. Default is .95.

- ...:

  Other arguments (usually ignored)

## Value

A data.frame with columns matching those appropriate for the model type
per `glance` documentation.

## See also

[`glance`](https://generics.r-lib.org/reference/glance.html)
