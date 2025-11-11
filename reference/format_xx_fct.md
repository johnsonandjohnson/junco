# Function factory for xx style formatting

A function factory to generate formatting functions for value formatting
that support the xx style format and control the rounding method

## Usage

``` r
format_xx_fct(
  roundmethod = c("sas", "iec"),
  na_str_dflt = "NE",
  replace_na_dflt = TRUE
)
```

## Arguments

- roundmethod:

  (`string`)  
  choice of rounding methods. Options are:

  - `sas`: the underlying rounding method is
    [`tidytlg::roundSAS`](https://pharmaverse.github.io/tidytlg/main/reference/roundSAS.html),
    where  
    roundSAS comes from this Stack Overflow post
    https://stackoverflow.com/questions/12688717/round-up-from-5

  - `iec`: the underlying rounding method is `round`

- na_str_dflt:

  Character to represent NA value

- replace_na_dflt:

  logical(1). Should an `na_string` of "NA" within the formatters
  framework be overridden by `na_str_default`? Defaults to `TRUE`, as a
  way to have a different default na string behavior from the base
  `formatters` framework.

## Value

`format_xx_fct()` format function that can be used in rtables formatting
calls

## See also

Other JJCS formats:
[`count_fraction`](https://johnsonandjohnson.github.io/junco/reference/count_fraction.md),
[`jjcsformat_pval_fct()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_pval_fct.md),
[`jjcsformat_range_fct()`](https://johnsonandjohnson.github.io/junco/reference/jjcsformat_range_fct.md)

## Examples

``` r
jjcsformat_xx_SAS <- format_xx_fct(roundmethod = "sas")
jjcsformat_xx <- jjcsformat_xx_SAS
rcell(c(1.453), jjcsformat_xx("xx.xx"))
#> rcell: 1.45 
rcell(c(), jjcsformat_xx("xx.xx"))
#> rcell:  
rcell(c(1.453, 2.45638), jjcsformat_xx("xx.xx (xx.xxx)"))
#> rcell: 1.45 (2.456) 
```
