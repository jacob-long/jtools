# Regression summaries with options

To get specific documentation, choose the appropriate link to the type
of model that you want to summarize from the details section.

## Usage

``` r
summ(model, ...)

j_summ(model, ...)
```

## Arguments

- model:

  A `lm`, `glm`, [`svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html),
  [`merMod`](https://rdrr.io/pkg/lme4/man/merMod-class.html),
  [`rq`](https://rdrr.io/pkg/quantreg/man/rq.html) object.

- ...:

  Other arguments to be passed to the model-specific function.

## Details

- [`summ.lm`](summ.lm.md)

- [`summ.glm`](summ.glm.md)

- [`summ.svyglm`](summ.svyglm.md)

- [`summ.merMod`](summ.merMod.md)

- [`summ.rq`](summ.rq.md)
