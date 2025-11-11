# Function factory for range with censoring information formatting

A function factory to generate formatting functions for range formatting
that includes information about the censoring of survival times.

## Usage

``` r
jjcsformat_range_fct(str)
```

## Arguments

- str:

  `string`  
  the format specifying the number of digits to be used, for the range
  values, e.g. `"xx.xx"`.

## Value

A function that formats a numeric vector with 4 elements:

- minimum

- maximum

- censored minimum? (1 if censored, 0 if event)

- censored maximum? (1 if censored, 0 if event) The range along with the
  censoring information is returned as a string with the specified
  numeric format as `(min, max)`, and the `+` is appended to `min` or
  `max` if these have been censored.

## See also

Other JJCS formats:
[`count_fraction`](https://johnsonandjohnson.github.io/junco/reference/count_fraction.md),
[`format_xx_fct()`](https://johnsonandjohnson.github.io/junco/reference/format_xx_fct.md),
[`jjcsformat_pval_fct()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_pval_fct.md)

## Examples

``` r
my_range_format <- jjcsformat_range_fct("xx.xx")
my_range_format(c(0.35235, 99.2342, 1, 0))
#> [1] "(0.35+, 99.23)"
my_range_format(c(0.35235, 99.2342, 0, 1))
#> [1] "(0.35, 99.23+)"
my_range_format(c(0.35235, 99.2342, 0, 0))
#> [1] "(0.35, 99.23)"
my_range_format(c(0.35235, 99.2342, 1, 1))
#> [1] "(0.35+, 99.23+)"
```
