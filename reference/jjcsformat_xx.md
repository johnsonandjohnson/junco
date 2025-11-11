# Formatting of values

jjcs formatting function

## Usage

``` r
jjcsformat_xx(str, na_str = na_str_dflt)
```

## Arguments

- str:

  The formatting that is required specified as a text string, eg "xx.xx"

- na_str:

  character. Na string that will be passed from `formatters` into our
  formatting functions.

## Value

a formatting function with `"sas"`-style rounding.
