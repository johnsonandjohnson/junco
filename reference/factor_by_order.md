# Create a factor with levels ordered by a separate ordering vector

**\[stable\]**

Converts a character vector or factor into a factor where the level
order is determined by a second integer-like vector containing the
corresponding order values.

## Usage

``` r
factor_by_order(x, y, ordered = FALSE)
```

## Arguments

- x:

  (`character` or `factor`)  
  A vector to be converted to a factor.

- y:

  (`integerish`)  
  A vector defining the order of levels in `x`. Must have the same
  length as `x`.

- ordered:

  (`logical(1)`)  
  Indicates whether the result should be an ordered factor. Defaults to
  `FALSE`.

## Value

A factor created from `x`, with levels ordered according to `y`.
Attributes of `x` other than `class` and `levels` are preserved. If
`ordered = TRUE`, an ordered factor is returned. If `x` is empty or
consists entirely of missing values, a factor with no levels is
returned.

## Details

The values in `x` and `y` must define a bijection between the unique
non-missing values: each unique value in `x` must correspond to exactly
one unique value in `y`, and vice versa. Missing values are handled
separately: `NA` values in `x` and `y` must occur at the same positions.
Missing values are not included as factor levels.

If `x` is already a factor containing unobserved (unused) levels, these
unobserved levels will be dropped in the resulting factor.

## Author

WW

## Examples

``` r
factor_by_order(c("A", "A", "B"), c(1, 1, 2))
#> [1] A A B
#> Levels: A B

factor_by_order(c("A", "A", "B"), c(1, 1, 2), ordered = TRUE)
#> [1] A A B
#> Levels: A < B

factor_by_order(c("A", "A", "B"), c(2, 2, 1))
#> [1] A A B
#> Levels: B A

factor_by_order(c("A", "A", "B", NA), c(1, 1, 2, NA))
#> [1] A    A    B    <NA>
#> Levels: A B

if (FALSE) { # \dontrun{
factor_by_order(c("A", "A", "B", NA), c(1, 2, 2, 4))
} # }
```
