# Conversion of inches to spaces.

Conversion of inches to spaces.

## Usage

``` r
inches_to_spaces(ins, fontspec, raw = FALSE, tol = sqrt(.Machine$double.eps))
```

## Arguments

- ins:

  (`numeric`)  
  Vector of widths in inches.

- fontspec:

  (`font_spec`)  
  The font specification to use.

- raw:

  (`logical(1)`)  
  Should the answer be returned unrounded (`TRUE`), or rounded to the
  nearest reasonable value (`FALSE`, the default).

- tol:

  (`numeric(1)`)  
  The numeric tolerance. Values between an integer `n`, and `n+tol` will
  be returned as `n`, rather than `n+1`, if `raw == FALSE`. Ignored when
  `raw` is `TRUE`.

## Value

The number of either fractional (`raw = TRUE`) or whole (`raw = FALSE`)
spaces that will fit within `ins` inches in the specified font.
