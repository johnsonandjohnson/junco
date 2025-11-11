# Helper method (for [`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)) to prepare a data frame from an `pool` `rbmi` object containing the LS means and contrasts and multiple visits

Helper method (for
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html))
to prepare a data frame from an `pool` `rbmi` object containing the LS
means and contrasts and multiple visits

## Usage

``` r
# S3 method for class 'pool'
tidy(x, visits, ...)
```

## Arguments

- x:

  (`pool`) is a list of pooled object from `rbmi` analysis results. This
  list includes analysis results, confidence level, hypothesis testing
  type.

- visits:

  (`character`)  
  all visit levels. Otherwise too hard to guess this.

- ...:

  Additional arguments. Not used. Needed to match generic signature
  only.

## Value

A `data.frame`.
