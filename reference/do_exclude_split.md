# Predicate to Check if Split Should be Excluded

Predicate to Check if Split Should be Excluded

## Usage

``` r
do_exclude_split(exclude_levels, .spl_context)
```

## Arguments

- exclude_levels:

  (`list`)  
  A named list where names correspond to split variables and values are
  vectors of levels to exclude.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

## Value

`TRUE` if the current split context matches any of the exclude levels,
`FALSE` otherwise.

## Examples

``` r
do_exclude_split(
  exclude_levels = list(AVISIT = "Baseline"),
  .spl_context = data.frame(
    split = c("AVISIT", "ARM"),
    value = c("Week 4", "Placebo")
  )
)
#> [1] FALSE
do_exclude_split(
  exclude_levels = list(AVISIT = "Baseline"),
  .spl_context = data.frame(
    split = c("AVISIT", "ARM"),
    value = c("Baseline", "Placebo")
  )
)
#> [1] TRUE
```
