# Parallelise Lapply

Simple wrapper around `lapply` and
[`parallel::clusterApplyLB`](https://rdrr.io/r/parallel/clusterApply.html)
to abstract away the logic of deciding which one to use.

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
