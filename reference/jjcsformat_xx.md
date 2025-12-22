# Utility for specifying custom formats

Utility for specifying custom formats that can be used as a format in
[`formatters::format_value`](https://insightsengineering.github.io/formatters/latest-tag/reference/format_value.html)

A function factory to generate formatting functions for p-value
formatting that support rounding close to the significance level
specified.

A function factory to generate formatting functions for range formatting
that includes information about the censoring of survival times.

## Usage

``` r
jjcsformat_xx(
  str,
  na_str = na_str_dflt,
  na_str_dflt = "NE",
  replace_na_dflt = TRUE
)

jjcsformat_pval_fct(alpha = 0.05)

jjcsformat_range_fct(str, censor_char = "+")
```

## Arguments

- str:

  (`string`)  
  the format specifying the number of digits to be used, for the range
  values, e.g. `"xx.xx"`.

- na_str:

  String for NA values.

- na_str_dflt:

  Character to represent NA value

- replace_na_dflt:

  logical(1). Should an `na_string` of "NA" within the formatters
  framework be overridden by `na_str_default`? Defaults to `TRUE`, as a
  way to have a different default na string behavior from the base
  `formatters` framework.

- alpha:

  (`numeric`)  
  the significance level to account for during rounding.

- censor_char:

  (`string`)  
  the character (of length 1) to be appended to `min` or `max`

## Value

Either a supported format string, or a formatting function that can be
used as format in
[`formatters::format_value`](https://insightsengineering.github.io/formatters/latest-tag/reference/format_value.html)

The p-value in the standard format. If `count` is 0, the format is `0`.
If it is smaller than 0.001, then `<0.001`, if it is larger than 0.999,
then `>0.999` is returned. Otherwise, 3 digits are used. In the special
case that rounding from below would make the string equal to the
specified `alpha`, then a higher number of digits is used to be able to
still see the difference. For example, 0.0048 is not rounded to 0.005
but stays at 0.0048 if `alpha = 0.005` is set.

A function that formats a numeric vector with 4 elements:

- minimum

- maximum

- censored minimum? (1 if censored, 0 if event)

- censored maximum? (1 if censored, 0 if event) The range along with the
  censoring information is returned as a string with the specified
  numeric format as `(min, max)`, and the `censor_char` is appended to
  `min` or `max` if these have been censored.

## See also

Other JJCS formatting functions:
[`count and fraction related formatting functions`](https://johnsonandjohnson.github.io/junco/reference/count_fraction.md)

## Examples

``` r
value <- c(1.65, 8.645)
fmt <- jjcsformat_xx("xx.x")
is.function(fmt)
#> [1] FALSE
fmt
#> [1] "xx.x"
format_value(value[1], fmt, round_type = "sas")
#> [1] "1.7"
format_value(value[1], fmt, round_type = "iec")
#> [1] "1.6"
if (is.function(fmt)) fmt(value[1])

fmt2 <- jjcsformat_xx("xx.x (xx.xxx)")
is.function(fmt2)
#> [1] TRUE
value <- c(1.65, 8.645)
format_value(value, fmt2, round_type = "sas")
#> [1] "1.7 (8.645)"
format_value(value, fmt2, round_type = "iec")
#> [1] "1.6 (8.645)"
# only possible when resulting format is a function
if (is.function(fmt2)) fmt2(value, round_type = "sas")
#> [1] "1.7 (8.645)"

value <- c(1.65, NA)
format_value(value, fmt2, round_type = "iec", na_str = c("ne1", "ne2"))
#> [1] "1.6 (ne1)"
if (is.function(fmt2)) fmt2(value, round_type = "iec", na_str = c("ne1", "ne2"))
#> [1] "1.6 (ne2)"
my_pval_format <- jjcsformat_pval_fct(0.005)
my_pval_format(0.2802359)
#> [1] "0.280"
my_pval_format(0.0048)
#> [1] "0.0048"
my_pval_format(0.00499)
#> [1] "0.00499"
my_pval_format(0.004999999)
#> [1] "0.004999999"
my_pval_format(0.0051)
#> [1] "0.005"
my_pval_format(0.0009)
#> [1] "<0.001"
my_pval_format(0.9991)
#> [1] ">0.999"

my_range_format <- jjcsformat_range_fct("xx.xx")
my_range_format(c(0.35235, 99.2342, 1, 0))
#> [1] "(0.35+, 99.23)"
my_range_format(c(0.35235, 99.2342, 0, 1))
#> [1] "(0.35, 99.23+)"
my_range_format(c(0.35235, 99.2342, 0, 0))
#> [1] "(0.35, 99.23)"
my_range_format(c(0.35235, 99.2342, 1, 1))
#> [1] "(0.35+, 99.23+)"
my_range_format <- jjcsformat_range_fct("xx.xx", censor_char = "*")
my_range_format(c(0.35235, 99.2342, 1, 1))
#> [1] "(0.35*, 99.23*)"
```
