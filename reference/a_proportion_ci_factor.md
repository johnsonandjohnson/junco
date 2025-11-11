# Formatted Analysis Function For Proportion Confidence Interval for Factor

Formatted Analysis Function For Proportion Confidence Interval for
Factor

## Usage

``` r
a_proportion_ci_factor(df, .var, ...)
```

## Arguments

- df:

  (`data.frame`)  
  including factor `.var`.

- .var:

  (`string`)  
  name of the factor variable.

- ...:

  see
  [`a_proportion_ci_logical()`](https://johnsonandjohnson.github.io/junco/reference/a_proportion_ci_logical.md)
  for additionally required arguments.

## Value

The
[`rtables::rcell()`](https://insightsengineering.github.io/rtables/latest-tag/reference/rcell.html)
result.

## Examples

``` r
a_proportion_ci_factor(
  df = DM,
  .var = "SEX",
  .alt_df = DM,
  conf_level = 0.95,
  formats = list(prop_ci = jjcsformat_xx("xx.x%, xx.x%")),
  method = "clopper-pearson"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>           row_name formatted_cell indent_mod        row_label
#> 1                F   47.2%, 57.8%          0                F
#> 2                M   42.2%, 52.8%          0                M
#> 3                U     0.0%, 1.0%          0                U
#> 4 UNDIFFERENTIATED     0.0%, 1.0%          0 UNDIFFERENTIATED
```
