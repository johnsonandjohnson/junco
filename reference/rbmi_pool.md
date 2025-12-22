# Pool analysis results obtained from the imputed datasets

Pool analysis results obtained from the imputed datasets

## Usage

``` r
rbmi_pool(
  results,
  conf.level = 0.95,
  alternative = c("two.sided", "less", "greater"),
  type = c("percentile", "normal")
)
```

## Arguments

- results:

  an analysis object created by analyse().

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1. Default is 0.95.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"two.sided"` (default), `"greater"` or `"less"`.

- type:

  a character string of either `"percentile"` (default) or `"normal"`.
  Determines what method should be used to calculate the bootstrap
  confidence intervals. See details. Only used if
  `method_condmean(type = "bootstrap")` was specified in the original
  call to draws().

## Value

A list of class `pool`.

## Details

This has been forked from the `rbmi` package, mainly to support in
addition the pooling of variance estimates. See pool() for more details.
