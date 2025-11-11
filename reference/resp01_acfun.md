# Formatted Analysis and Content Summary Function for Response Tables (RESP01)

This function applies to both `factor` and `logical` columns called
`.var` from `df`. Depending on the position in the split, it returns the
right formatted results for the RESP01 and related layouts.

## Usage

``` r
resp01_acfun(
  df,
  labelstr = NULL,
  label = NULL,
  .var,
  .spl_context,
  include_comp,
  .alt_df,
  conf_level,
  arm,
  strata,
  formats,
  methods
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`character`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- label:

  (`string`)  
  only for logicals, which label to use. (For factors, the labels are
  the factor levels.)

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- include_comp:

  (`character` or `flag`)  
  whether to include comparative statistic results, either `character`
  for factors or `flag` for logicals.

- .alt_df:

  (`data.frame`)  
  alternative data frame used for denominator calculation.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- arm:

  (`string`)  
  column name in the data frame that identifies the treatment arms.

- strata:

  (`character` or `NULL`)  
  variable names indicating stratification factors.

- formats:

  (`list`)  
  containing formats for `prop_ci`, `comp_stat_ci` and `pval`.

- methods:

  (`list`)  
  containing methods for comparative statistics. The element
  `comp_stat_ci` can be 'rr' (relative risk), 'or_cmh' (odds ratio with
  CMH estimation and p-value) or 'or_logistic' (odds ratio estimated by
  conditional or standard logistic regression). The element `pval` can
  be 'fisher' (Fisher's exact test) or 'chisq' (chi-square test), only
  used when using unstratified analyses with 'or_logistic'. The element
  `prop_ci` specifies the method for proportion confidence interval
  calculation.

## Value

The formatted result as
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
result.

## Examples

``` r
fake_spl_context <- data.frame(
  cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "count_prop")))
)
dm <- droplevels(subset(DM, SEX %in% c("F", "M")))
resp01_acfun(
  dm,
  .alt_df = dm,
  .var = "COUNTRY",
  .spl_context = fake_spl_context,
  conf_level = 0.9,
  include_comp = c("USA", "CHN"),
  arm = "SEX",
  strata = "RACE",
  methods = list(
    comp_stat_ci = "or_cmh",
    pval = "",
    prop_ci = "wald"
  ),
  formats = list(
    prop_ci = jjcsformat_xx("xx.% - xx.%"),
    comp_stat_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
    pval = jjcsformat_pval_fct(0.05)
  )
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod row_label
#> 1      CHN    179 (50.3%)          0       CHN
#> 2      USA     44 (12.4%)          0       USA
#> 3      BRA      29 (8.1%)          0       BRA
#> 4      PAK      28 (7.9%)          0       PAK
#> 5      NGA      24 (6.7%)          0       NGA
#> 6      RUS      20 (5.6%)          0       RUS
#> 7      JPN      18 (5.1%)          0       JPN
#> 8      GBR       7 (2.0%)          0       GBR
#> 9      CAN       7 (2.0%)          0       CAN
fake_spl_context2 <- data.frame(
  cur_col_split_val = I(list(c(ARM = "Overall", comp_stat_ci = "comp_stat_ci")))
)
resp01_acfun(
  dm,
  .alt_df = dm,
  .var = "COUNTRY",
  .spl_context = fake_spl_context2,
  conf_level = 0.9,
  include_comp = c("USA", "CHN"),
  arm = "SEX",
  strata = "RACE",
  methods = list(
    comp_stat_ci = "or_cmh",
    pval = "",
    prop_ci = "wald"
  ),
  formats = list(
    prop_ci = jjcsformat_xx("xx.% - xx.%"),
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
