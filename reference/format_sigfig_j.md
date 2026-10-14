# Format numeric values by significant figures.

**\[experimental\]**

Format numeric values by significant figures with controllable
round_type, whole_integer and trailing zeros.  
The underlying function for significant figures is
`modified_signif_j()`, which is a modified version for
[`base::signif()`](https://rdrr.io/r/base/Round.html)

## Usage

``` r
format_sigfig_j(
  sigfig,
  format = "xx",
  whole_integer = TRUE,
  zero_threshold = 10^(-2 * sigfig),
  drop0trailing = FALSE
)

modified_signif_j(
  x,
  digits = 6,
  round_type = valid_round_type,
  whole_integer = FALSE,
  zero_threshold = 0
)
```

## Arguments

- sigfig:

  (`integer(1)`)  
  number of significant figures to display.

- format:

  (`character(1)`)  
  layout format string controlling how one or two values are arranged,
  e.g. `"xx"`, `"xx (xx)"`, `"(xx, xx)"`. Any decimal specification in
  `xx.` or `xx.x+` is stripped — precision is controlled by `sigfig`
  alone.

- whole_integer:

  (`logical(1)`)  
  if `TRUE`, all significant figures to the left of the decimal marker
  can be shown. If `FALSE`, the specified number of significant figures
  is shown.

- zero_threshold:

  (`numeric(1)`)  
  very small absolute values (`abs(x) < zero_threshold`) can be treated
  as zero. Defaults to `0` (values are left as is).

- drop0trailing:

  (`logical(1)`)  
  indicating if trailing zeros, i.e., "0" after the decimal mark, should
  be removed  
  argument is passed to function `formatC`

- x:

  (`numeric`)  
  numbers to round.

- digits:

  (`integer(1)`)  
  number of significant figures to display.

- round_type:

  (`character(1)`)  
  rounding method, passed onto
  [`formatters::round_fmt()`](https://pharmaverse.github.io/formatters/latest-tag/reference/round_fmt.html).
  See
  [`formatters::format_value()`](https://pharmaverse.github.io/formatters/latest-tag/reference/format_value.html)
  for details.

## Value

A formatting function with signature `function(x, round_type, ...)` that
returns a formatted string. The `round_type` argument accepts rounding
methods ("iec", "iec_mod", "sas") (see
[`formatters::round_fmt()`](https://pharmaverse.github.io/formatters/latest-tag/reference/round_fmt.html)).

## Functions

- `format_sigfig_j()`: A function factory that produces formatting
  functions to round values to a specified number of significant
  figures, suitable for use with
  [`formatters::format_value()`](https://pharmaverse.github.io/formatters/latest-tag/reference/format_value.html).  
    
  The function is an alternative to
  [`tern::format_sigfig()`](https://pharmaverse.github.io/tern/latest-tag/reference/format_sigfig.html),
  `format_sigfig_j()` has fixed the `formatC` argument `num_fmt = "fg"`,
  and has extra options for `whole_integer` and `drop0trailing` to
  control the format behavior.

- `modified_signif_j()`: A modified version of base function
  [base::signif](https://rdrr.io/r/base/Round.html) with 3 extensions:

  - Rounding is performed using
    [`formatters::round_fmt()`](https://pharmaverse.github.io/formatters/latest-tag/reference/round_fmt.html),
    and allows for round types ("iec", "iec_mod", "sas")  

  - Very small absolute values can be treated as zero
    (`zero_threshold`).  

  - All significant figures to the left of the decimal marker can be
    shown (whole_integer), rather than limiting the result to the
    specified number of significant figures.

  Returns a numeric vector of the same length as `x`, rounded to
  `digits` significant figures.

## See also

[`tern::format_sigfig()`](https://pharmaverse.github.io/tern/latest-tag/reference/format_sigfig.html)

## Examples

``` r
fmt <- format_sigfig_j(3)
fmt(0.001234)
#> [1] "0.00123"
fmt(1234.5)
#> [1] "1234"

fmt2 <- format_sigfig_j(3, format = "xx (xx)")
fmt2(c(1234.5, 0.00567))
#> [1] "1234 (0.00567)"


format_sigfig_j(3, drop0trailing = FALSE)(0.01)
#> [1] "0.0100"
format_sigfig_j(3, drop0trailing = TRUE)(0.01)
#> [1] "0.01"
# example code
junco:::modified_signif_j(c(0.1, 1e-9, 1256.0, 1256.5, 1256.3, 1254.5, 1256.5),
  digits = 3,
  zero_threshold = 0,
  whole_integer = TRUE,
  round_type = "sas"
)
#> [1] 1.000e-01 1.000e-09 1.256e+03 1.257e+03 1.256e+03 1.255e+03 1.257e+03

junco:::modified_signif_j(c(0.1, 1e-9, 1256.0, 1256.5, 1256.3, 1254.5, 1256.5),
  digits = 3,
  zero_threshold = 1e-6,
  whole_integer = TRUE,
  round_type = "sas"
)
#> [1]    0.1    0.0 1256.0 1257.0 1256.0 1255.0 1257.0
```
