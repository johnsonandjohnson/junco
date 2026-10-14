# Categorize p-values

Assigns each p-value to a category defined by a named list of lower and
upper bounds.

## Usage

``` r
categorize_pval(p, pvalcat)
```

## Arguments

- p:

  (`numeric`)  
  A vector of p-values to categorize. Missing values are permitted.

- pvalcat:

  (`named list`)  
  A non-empty named list of p-value category bounds. See
  [`h_normalize_pvalcat()`](https://johnsonandjohnson.github.io/junco/reference/h_normalize_pvalcat.md)
  for the required structure.

## Value

A character vector of category labels, with one element per value in
`p`.

## Details

Categories are evaluated in the order supplied by `pvalcat`. Bounds are
lower-inclusive and upper-exclusive, except that the upper bound of the
final category is inclusive. P-values that do not fall in a category,
including missing values, are returned as `NA_character_`.

## Examples

``` r
pvalcat <- list(
  "<0.001" = c(0, 0.001),
  "0.001 to <0.05" = c(0.001, 0.05),
  ">=0.05" = c(0.05, 1)
)

categorize_pval(c(0, 0.001, 0.049, 0.05, 1, NA), pvalcat)
#> [1] "<0.001"         "0.001 to <0.05" "0.001 to <0.05" ">=0.05"        
#> [5] ">=0.05"         NA              
```
