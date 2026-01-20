# Calculate robust standard errors and produce coefficient tables

This function wraps around several sandwich and lmtest functions to
calculate robust standard errors and returns them in a useful format.

## Usage

``` r
get_robust_se(
  model,
  type = "HC3",
  cluster = NULL,
  data = model.frame(model),
  vcov = NULL
)
```

## Arguments

- model:

  A regression model, preferably of class `lm` or `glm`

- type:

  One of `"HC3"`, `"const"`, `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC4"`,
  `"HC4m"`, `"HC5"`. See
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
  for some more details on these choices. Note that some of these do not
  work for clustered standard errors (see
  [`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html)).

- cluster:

  If you want clustered standard errors, either a string naming the
  column in `data` that represents the clusters or a vector of clusters
  that is the same length as the number of rows in `data`.

- data:

  The data used to fit the model. Default is to just get the
  `model.frame` from `model`.

- vcov:

  You may provide the variance-covariance matrix yourself and this
  function will just calculate standard errors, etc. based on that.
  Default is NULL.

## Value

A list with the following:

- `coefs`: a coefficient table with the estimates, standard errors,
  t-statistics, and p-values from `lmtest`.

- `ses`: The standard errors from `coefs`.

- `ts`: The t-statistics from `coefs`.

- `ps`: The p-values from `coefs`.

- `type`: The argument to `robust`.

- `use_cluster`: `TRUE` or `FALSE` indicator of whether clusters were
  used.

- `cluster`: The clusters or name of cluster variable used, if any.

- `vcov`: The robust variance-covariance matrix.
