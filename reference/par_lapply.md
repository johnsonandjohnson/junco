# Parallelise Lapply

**\[deprecated\]** This internal helper is no longer exported. Use
[`rbmi::analyse()`](https://openpharma.github.io/rbmi/latest-tag/reference/analyse.html)
directly.

## Usage

``` r
par_lapply(cl, fun, x, ...)
```

## Arguments

- cl:

  (`cluster object`)  
  Cluster created by
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  or `NULL`

- fun:

  (`functions`)  
  Function to be run

- x:

  (`object`)  
  Object to be looped over

- ...:

  Extra arguments passed to `fun`

## Value

`list` of results of calling `fun` on elements of `x`.
