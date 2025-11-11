# Get Visit Levels in Order Defined by Numeric Version

Get Visit Levels in Order Defined by Numeric Version

## Usage

``` r
get_visit_levels(visit_cat, visit_n)
```

## Arguments

- visit_cat:

  (`character`)  
  the categorical version.

- visit_n:

  (`numeric`)  
  the numeric version.

## Value

The unique visit levels in the order defined by the numeric version.

## Examples

``` r
get_visit_levels(
  visit_cat = c("Week 1", "Week 11", "Week 2"),
  visit_n = c(1, 5, 2)
)
#> [1] "Week 1"  "Week 2"  "Week 11"
```
