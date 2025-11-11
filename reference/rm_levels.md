# Removal of Levels

custom function for removing level inside pre step in make_split_fun.

## Usage

``` r
rm_levels(excl)
```

## Arguments

- excl:

  Choose which level(s) to remove

## Value

a function implementing pre-processing split behavior (for use in
`make_split_fun(pre = )` which removes the levels in `excl` from the
data before facets are generated.
