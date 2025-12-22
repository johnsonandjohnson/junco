# Helper for Finding AVISIT after which CHG are all Missing

Helper for Finding AVISIT after which CHG are all Missing.

## Usage

``` r
find_missing_chg_after_avisit(df)
```

## Arguments

- df:

  (`data.frame`)  
  with `CHG` and `AVISIT` variables.

## Value

A string with either the factor level after which `AVISIT` is all
missing, or `NA`.

## Examples

``` r
df <- data.frame(
  AVISIT = factor(c(1, 2, 3, 4, 5)),
  CHG = c(5, NA, NA, NA, 3)
)
find_missing_chg_after_avisit(df)
#> [1] NA

df2 <- data.frame(
  AVISIT = factor(c(1, 2, 3, 4, 5)),
  CHG = c(5, NA, 3, NA, NA)
)
find_missing_chg_after_avisit(df2)
#> [1] "4"

df3 <- data.frame(
  AVISIT = factor(c(1, 2, 3, 4, 5)),
  CHG = c(NA, NA, NA, NA, NA)
)
find_missing_chg_after_avisit(df3)
#> [1] "1"
```
