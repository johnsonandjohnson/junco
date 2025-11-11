# Formatting fraction, count and denominator values

Formatting fraction, count and denominator values

## Usage

``` r
jjcsformat_fraction_count_denom(x, d = 1, roundmethod = c("sas", "iec"), ...)
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

`x` formatted as a string with `d` digits of precision, with special
cased values as described in Details above.

## Details

Formats a 3-dimensional value such that percent values near 0 or 100%
are formatted as .e.g, `"<0.1%"` and `">99.9%"`, where the cutoff is
controlled by `d`, and formatted as `"xx.x% (xx/xx)"` otherwise, with
the precision of the percent also controlled by `d`.

## Examples

``` r
jjcsformat_fraction_count_denom(c(7, 10, 0.7))
#> [1] "70.0% (7/10)"
jjcsformat_fraction_count_denom(c(70000, 70001, 70000 / 70001))
#> [1] ">99.9% (70000/70001)"
jjcsformat_fraction_count_denom(c(235, 235, 235 / 235))
#> [1] "100.0% (235/235)"
```
