# Formatting function for percentage values

**\[experimental\]**

Formats fractional values as percentages with special consideration for
values that round to 0 or 100, as well as missing values. The input is
expected to be a fraction, which is multiplied by 100 internally before
formatting. Missing values are formatted as "-".

## Usage

``` r
jjcsformat_fraction(
  x,
  round_type = valid_round_type,
  output = c("ascii", "html"),
  d = 1,
  add_pct = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  (`numeric(1)`)  
  Fraction to format as a percentage (e.g. `0.15` represents `15%`. `NA`
  is formatted as `"-"`.

- round_type:

  (`character(1)`)  
  The type of rounding to perform. See
  [`formatters::format_value()`](https://pharmaverse.github.io/formatters/latest-tag/reference/format_value.html)
  for more details.

- output:

  (`character(1)`)  
  Output type. See
  [`formatters::format_value()`](https://pharmaverse.github.io/formatters/latest-tag/reference/format_value.html)
  for more details.

- d:

  (`numeric(1)`)  
  Number of digits to round the percentage to.

- add_pct:

  (`logical(1)`)  
  Whether to append the percentage symbol (`"%"`) to the formatted
  value.

- verbose:

  (`logical(1)`)  
  Whether to print verbose output.

- ...:

  Additional arguments passed to other methods.

## Value

A formatted string.

## See also

Other JJCS formatting functions:
[`count and fraction related formatting functions`](https://johnsonandjohnson.github.io/junco/reference/count_fraction.md),
[`jjcsformat_xx()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_xx.md)

## Author

WW

## Examples

``` r
jjcsformat_fraction(0.15)
#> [1] "15.0"
jjcsformat_fraction(0.15, d = 2)
#> [1] "15.00"
jjcsformat_fraction(0.15, d = 2, verbose = TRUE)
#> round_type used: iec
#> [1] "15.00"
```
