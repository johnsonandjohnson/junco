# Formatting functions for count and fraction, and for count denominator and fraction values

Formats a count together with fraction (and/or denominator) with special
consideration when count is 0, or fraction is 1.  
See also:
[`tern::format_count_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/latest-tag/reference/format_count_fraction_fixed_dp.html)

## Usage

``` r
jjcsformat_cnt_den_fract_fct(
  d = 1,
  type = c("count_fraction", "count_denom_fraction", "fraction_count_denom"),
  verbose = FALSE
)

jjcsformat_count_fraction(x, round_type = valid_round_type, output, ...)

jjcsformat_count_denom_fraction(x, round_type = valid_round_type, output, ...)

jjcsformat_fraction_count_denom(x, round_type = valid_round_type, output, ...)
```

## Arguments

- d:

  (`numeric(1)`)  
  Number of digits to round fraction to (default = 1)

- type:

  (`character(1`)  
  One of `count_fraction`, `count_denom_fraction`,
  `fraction_count_denom`, to specify the type of format the function
  will represent.

- verbose:

  (`logical`)  
  Whether to print verbose output

- x:

  (`numeric vector`)  
  Vector with elements `num` and `fraction` or `num`, `denom` and
  `fraction`.

- round_type:

  (`character(1)`)  
  the type of rounding to perform. See
  [`formatters::format_value()`](https://insightsengineering.github.io/formatters/latest-tag/reference/format_value.html)
  for more details.

- output:

  (`string`)  
  output type. See
  [`formatters::format_value()`](https://insightsengineering.github.io/formatters/latest-tag/reference/format_value.html)
  for more details.

- ...:

  Additional arguments passed to other methods.

## Value

A formatting function to format input into string in the format
`count / denom (ratio percent)`. If `count` is 0, the format is `0`. If
fraction is \>0.99, the format is `count / denom (>99.9 percent)`

## See also

Other JJCS formatting functions:
[`jjcsformat_xx()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_xx.md)

## Examples

``` r
jjcsformat_count_fraction(c(7, 0.7))
#> [1] "7 (70.0%)"
jjcsformat_count_fraction(c(70000, 70000 / 70001))
#> [1] "70000 (>99.9%)"
jjcsformat_count_fraction(c(235, 235 / 235))
#> [1] "235 (100.0%)"
fmt <- jjcsformat_cnt_den_fract_fct(type = "count_fraction", d = 2)
fmt(c(23, 23 / 235))
#> [1] "23 (9.79%)"

jjcsformat_count_denom_fraction(c(7, 10, 0.7))
#> [1] "7/10 (70.0%)"
jjcsformat_count_denom_fraction(c(70000, 70001, 70000 / 70001))
#> [1] "70000/70001 (>99.9%)"
jjcsformat_count_denom_fraction(c(235, 235, 235 / 235))
#> [1] "235/235 (100.0%)"
fmt <- jjcsformat_cnt_den_fract_fct(type = "count_denom_fraction", d = 2)
fmt(c(23, 235, 23 / 235))
#> [1] "23/235 (9.79%)"

jjcsformat_fraction_count_denom(c(7, 10, 0.7))
#> [1] "70.0% (7/10)"
jjcsformat_fraction_count_denom(c(70000, 70001, 70000 / 70001))
#> [1] ">99.9% (70000/70001)"
jjcsformat_fraction_count_denom(c(235, 235, 235 / 235))
#> [1] "100.0% (235/235)"
fmt <- jjcsformat_cnt_den_fract_fct(type = "fraction_count_denom", d = 2)
fmt(c(23, 235, 23 / 235))
#> [1] "9.79% (23/235)"
```
