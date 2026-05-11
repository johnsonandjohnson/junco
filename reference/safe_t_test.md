# Safe Wrapper for `stats::t.test()`

**\[experimental\]**

This is a robust wrapper around
[stats::t.test](https://rdrr.io/r/stats/t.test.html) that prevents
errors from interrupting execution. Instead of failing, it returns a
structured result containing `NA` values and an informative error
message.

This is particularly useful in pipelines, simulations, or batch analyses
where occasional invalid inputs (e.g., constant vectors, insufficient
observations) would otherwise stop execution.

## Usage

``` r
safe_t_test(x, y = NULL, ...)
```

## Arguments

- x:

  a (non-empty) numeric vector of data values.

- y:

  an optional (non-empty) numeric vector of data values.

- ...:

  further arguments to be passed to or from methods. For the `formula`
  method, this includes arguments of the default method, but not
  `paired`.

## Value

A `list`: either the standard `htest` object from
[stats::t.test](https://rdrr.io/r/stats/t.test.html), or (on error) a
list with `NA` statistics, sample mean(s) in `estimate`, and an
`error_text` field containing the error message.

## Details

When [stats::t.test](https://rdrr.io/r/stats/t.test.html) succeeds, the
result is returned unchanged. If an error occurs, a list is returned
mimicking key components of a `htest` object. Any `NaN` estimates are
converted to `NA_real_`.

## Examples

``` r
# Standard usage
t.test(1:10, 11:20)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  1:10 and 11:20
#> t = -7.3855, df = 18, p-value = 7.503e-07
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -12.844662  -7.155338
#> sample estimates:
#> mean of x mean of y 
#>       5.5      15.5 
#> 
safe_t_test(1:10, 11:20)
#> 
#>  Welch Two Sample t-test
#> 
#> data:  1:10 and 11:20
#> t = -7.3855, df = 18, p-value = 7.503e-07
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -12.844662  -7.155338
#> sample estimates:
#> mean of x mean of y 
#>       5.5      15.5 
#> 

# Example triggering failure (zero variance)
if (FALSE) stats::t.test(rep(10, 5), rep(10, 5)) # \dontrun{}
safe_t_test(rep(10, 5), rep(10, 5))
#> $statistic
#> [1] NA
#> 
#> $parameter
#> [1] NA
#> 
#> $p.value
#> [1] NA
#> 
#> $estimate
#> mean_x mean_y 
#>     10     10 
#> 
#> $conf.int
#> [1] NA NA
#> 
#> $method
#> [1] "t-test (failed)"
#> 
#> $data.name
#> [1] "x and y"
#> 
#> $error_text
#> [1] "data are essentially constant"
#> 
```
