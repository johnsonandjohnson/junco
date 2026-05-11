# Extract the left-hand side of a formula

Extract the left-hand side of a formula

## Usage

``` r
leftside(x)
```

## Arguments

- x:

  (`formula`)  
  A two-sided formula, e.g., `y ~ x1 + x2`.

## Value

(`character(1)`) The name of the left-hand side of the formula.

## Examples

``` r
leftside(y ~ x)
#> [1] "y"
```
