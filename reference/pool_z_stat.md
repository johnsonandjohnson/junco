# Pool z statistics across imputations

Combines z statistics from multiple imputed datasets and calculates a
two-sided p-value for the pooled statistic.

## Usage

``` r
pool_z_stat(z_stat_vals)
```

## Arguments

- z_stat_vals:

  (`numeric`) Vector of z statistics, with one value for each imputed
  dataset.

## Value

A named `list` with the pooled z statistic (`z`) and its two-sided
p-value (`p`).

## Details

The within-imputation variance of a z statistic is approximately 1. The
between-imputation variance is pooled using the same `(1 + 1 / m)`
multiplier as
[`pool_rubin_scalar()`](https://johnsonandjohnson.github.io/junco/reference/pool_rubin_scalar.md).
The returned p-value is calculated from the standard normal
distribution.

## See also

[`pool_rubin_scalar()`](https://johnsonandjohnson.github.io/junco/reference/pool_rubin_scalar.md)

## Examples

``` r
pool_z_stat(c(1.1, 1.3, 1.2))
#> $z
#> [1] 1.192079
#> 
#> $p
#> [1] 0.2332302
#> 
```
