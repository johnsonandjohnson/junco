# Formatting count and fraction values

Formats a count together with fraction (and/or denominator) with special
consideration when count is 0, or fraction is 1.  
See also: tern::format_count_fraction_fixed_dp()

## Usage

``` r
jjcsformat_count_fraction(x, d = 1, roundmethod = c("sas", "iec"), ...)
```

## Arguments

- x:

  `numeric`  
  with elements `num` and `fraction` or `num`, `denom` and `fraction`.

- d:

  numeric(1). Number of digits to round fraction to (default=1)

- roundmethod:

  (`string`)  
  choice of rounding methods. Options are:

  - `sas`: the underlying rounding method is
    [`tidytlg::roundSAS`](https://pharmaverse.github.io/tidytlg/main/reference/roundSAS.html),
    where  
    roundSAS comes from this Stack Overflow post
    https://stackoverflow.com/questions/12688717/round-up-from-5

  - `iec`: the underlying rounding method is `round`

- ...:

  Additional arguments passed to other methods.

## Value

A string in the format `count / denom (ratio percent)`. If `count` is 0,
the format is `0`. If fraction is \>0.99, the format is
`count / denom (>99.9 percent)`

## See also

Other JJCS formats:
[`format_xx_fct()`](https://johnsonandjohnson.github.io/junco/reference/format_xx_fct.md),
[`jjcsformat_pval_fct()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_pval_fct.md),
[`jjcsformat_range_fct()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_range_fct.md)

## Examples

``` r
jjcsformat_count_fraction(c(7, 0.7))
#> [1] "7 (70.0%)"
jjcsformat_count_fraction(c(70000, 0.9999999))
#> [1] "70000 (>99.9%)"
jjcsformat_count_fraction(c(70000, 1))
#> [1] "70000 (100.0%)"
```
