# Formatted Analysis Function for Comparative Statistic in Response Tables (RESP01)

This function applies to a `factor` column called `.var` from `df`.

## Usage

``` r
resp01_a_comp_stat_factor(df, .var, include, ...)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- include:

  (`character`)  
  for which factor levels to include the comparison statistic results.

- ...:

  see
  [`resp01_a_comp_stat_logical()`](https://johnsonandjohnson.github.io/junco/reference/resp01_a_comp_stat_logical.md)
  for additional required arguments.

## Value

The formatted result as
[`rtables::rcell()`](https://insightsengineering.github.io/rtables/latest-tag/reference/rcell.html).

## Examples

``` r
dm <- droplevels(subset(formatters::DM, SEX %in% c("F", "M")))

resp01_a_comp_stat_factor(
  dm,
  .var = "COUNTRY",
  conf_level = 0.9,
  include = c("USA", "CHN"),
  arm = "SEX",
  strata = "RACE",
  stat = "comp_stat_ci",
  methods = list(comp_stat_ci = "or_cmh"),
  formats = list(
    comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
    pval = jjcsformat_pval_fct(0.05)
  )
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name     formatted_cell indent_mod row_label
#> 1      CHN 1.00 (0.71 - 1.42)          0       CHN
#> 2      USA 1.09 (0.64 - 1.86)          0       USA
#> 3      BRA                             0       BRA
#> 4      PAK                             0       PAK
#> 5      NGA                             0       NGA
#> 6      RUS                             0       RUS
#> 7      JPN                             0       JPN
#> 8      GBR                             0       GBR
#> 9      CAN                             0       CAN
```
