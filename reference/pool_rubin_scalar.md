# Pool a scalar estimate across imputations

Applies Rubin's rules to combine scalar estimates and their
within-imputation variance estimates.

## Usage

``` r
pool_rubin_scalar(q, u)
```

## Arguments

- q:

  (`numeric`) Vector of scalar estimates, with one value for each
  imputed dataset.

- u:

  (`numeric`) Vector of within-imputation variance estimates
  corresponding to `q`.

## Value

A named `list` with the pooled estimate (`est`), standard error (`se`),
variance (`var`), and number of imputations (`m`).

## Details

For `m` imputations, the pooled estimate is the mean of `q`. The pooled
variance combines the mean within-imputation variance with the
between-imputation variance, using the usual `(1 + 1 / m)` multiplier.
The returned standard error is the square root of the pooled variance.

## See also

[`pool_z_stat()`](https://johnsonandjohnson.github.io/junco/reference/pool_z_stat.md)

## Examples

``` r
pool_rubin_scalar(
  q = c(0.20, 0.25, 0.15),
  u = c(0.01, 0.016, 0.012)
)
#> $est
#> [1] 0.2
#> 
#> $se
#> [1] 0.1264911
#> 
#> $var
#> [1] 0.016
#> 
#> $m
#> [1] 3
#> 
```
