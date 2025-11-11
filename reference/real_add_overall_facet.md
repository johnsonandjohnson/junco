# Add Overall Facet

A function to help add an overall facet to your tables

## Usage

``` r
real_add_overall_facet(name, label)
```

## Arguments

- name:

  character(1). Name/virtual 'value' for the new facet

- label:

  character(1). label for the new facet

## Value

function usable directly as a split function.

## Note

current add_overall_facet is bugged, can use that directly after it's
fixed https://github.com/insightsengineering/rtables/issues/768

## Examples

``` r
splfun <- make_split_fun(post = list(real_add_overall_facet('Total', 'Total')))
```
