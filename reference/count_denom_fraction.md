# Formatting count, denominator and fraction values

Formatting count, denominator and fraction values

## Usage

``` r
jjcsformat_count_denom_fraction(x, d = 1, roundmethod = c("sas", "iec"), ...)
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

`x`, formatted into a string with the appropriate format and `d` digits
of precision.

## Examples

``` r
jjcsformat_count_denom_fraction(c(7, 10, 0.7))
#> [1] "7/10 (70.0%)"
jjcsformat_count_denom_fraction(c(70000, 70001, 70000 / 70001))
#> [1] "70000/70001 (>99.9%)"
jjcsformat_count_denom_fraction(c(235, 235, 235 / 235))
#> [1] "235/235 (100.0%)"
```
