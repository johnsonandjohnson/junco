# Helper function to normalize p-value categories

Converts a named list of p-value category bounds into a matrix and
retains the category labels in their supplied order.

## Usage

``` r
h_normalize_pvalcat(pvalcat)
```

## Arguments

- pvalcat:

  (`named list`)  
  A non-empty named list. Each element must be a numeric vector of
  length two specifying the lower and upper bounds of a p-value
  category.

## Value

A named `list` with the following elements:

- `bounds`: a two-column numeric matrix of lower and upper category
  bounds.

- `cats`: a character vector of category labels.
