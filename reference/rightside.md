# Extract the right-hand side of a formula

Extract the right-hand side of a formula

## Usage

``` r
rightside(x)
```

## Arguments

- x:

  (`formula`)  
  A two-sided formula, e.g., `y ~ x1 + x2`.

## Value

(`character(1)`) The right-hand side of the formula.

## Examples

``` r
rightside(y ~ x1 + x2)
#> [1] "x1 + x2"
```
