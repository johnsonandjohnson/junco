# Function factory for p-value formatting

A function factory to generate formatting functions for p-value
formatting that support rounding close to the significance level
specified

## Usage

``` r
jjcsformat_pval_fct(alpha = 0.05)
```

## Arguments

- alpha:

  `number`  
  the significance level to account for during rounding.

## Value

The p-value in the standard format. If `count` is 0, the format is `0`.
If it is smaller than 0.001, then `<0.001`, if it is larger than 0.999,
then `>0.999` is returned. Otherwise, 3 digits are used. In the special
case that rounding from below would make the string equal to the
specified `alpha`, then a higher number of digits is used to be able to
still see the difference. For example, 0.0048 is not rounded to 0.005
but stays at 0.0048 if `alpha = 0.005` is set.

## See also

Other JJCS formats:
[`count_fraction`](https://johnsonandjohnson.github.io/junco/reference/count_fraction.md),
[`format_xx_fct()`](https://johnsonandjohnson.github.io/junco/reference/format_xx_fct.md),
[`jjcsformat_range_fct()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_range_fct.md)

## Examples

``` r
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
```
