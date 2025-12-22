# Formatted Analysis Function For Proportion Confidence Interval for Logical

Formatted Analysis Function For Proportion Confidence Interval for
Logical.

## Usage

``` r
a_proportion_ci_logical(x, .alt_df, conf_level, method, formats)
```

## Arguments

- x:

  (`logical`)  
  including binary response values.

- .alt_df:

  (`data.frame`)  
  alternative data frame used for denominator calculation.

- conf_level:

  (`numeric`)  
  confidence level for the confidence interval.

- method:

  (`string`)  
  please see
  [`tern::s_proportion()`](https://insightsengineering.github.io/tern/latest-tag/reference/estimate_proportion.html)
  for possible methods.

- formats:

  (`list`)  
  including element `prop_ci` with the required format. Note that the
  value is in percent already.

## Value

The
[`rtables::rcell()`](https://insightsengineering.github.io/rtables/latest-tag/reference/rcell.html)
result.

## Examples

``` r
a_proportion_ci_logical(
  x = DM$SEX == "F",
  .alt_df = DM,
  conf_level = 0.95,
  formats = list(prop_ci = jjcsformat_xx("xx.xx% - xx.xx%")),
  method = "wald"
)
#> rcell: 47.34% - 57.72% 
```
