# Difference in Means with Confidence Interval

**\[experimental\]**

Computes the difference in means between two samples along with a
confidence interval. The interval is computed using a t-distribution
framework via
[`safe_t_test()`](https://johnsonandjohnson.github.io/junco/reference/safe_t_test.md).

Supports both independent and paired samples. For paired data,
observations are matched using `paired_by`, and the inference is based
on within-pair differences using a paired t-distribution framework.

## Usage

``` r
s_diff_mean_ci(
  df1,
  df2,
  .var,
  paired = FALSE,
  paired_by = NULL,
  conf.level = 0.95,
  ...
)
```

## Arguments

- df1:

  (`data.frame`)  
  Dataset for the first sample.

- df2:

  (`data.frame`)  
  Dataset for the second sample.

- .var:

  (`character(1)`)  
  Column name in `df1` and `df2` containing numeric values.

- paired:

  (`logical(1)`)  
  Whether the samples are paired.

- paired_by:

  (`character` or `NULL`)  
  Column name(s) in `df1` and `df2` used to match observations between
  datasets. Required when `paired = TRUE` and must uniquely identify
  each pair in both datasets.

- conf.level:

  (`proportion`)  
  Confidence level for the interval.

- ...:

  Additional arguments passed to
  [`safe_t_test()`](https://johnsonandjohnson.github.io/junco/reference/safe_t_test.md).

## Value

A named `list` with a single element `diff_mean_ci`, containing the
difference in means and confidence interval estimates.

## Details

The first sample is taken from `df1[[.var]]` and the second from
`df2[[.var]]`.

If `paired = TRUE`, observations are matched using `paired_by`. In this
case, the difference in means and its confidence interval are computed
using a t-statistic for paired data (based on within-pair differences).
Otherwise, a t-statistic for two independent samples is used.

Any `NA` or `NaN` values in columns specified by `paired_by` are ignored
and excluded from matching (see
`merge(..., incomparables = c(NA, NaN))`).

When `paired = TRUE`, only complete pairs are passed to
[`safe_t_test()`](https://johnsonandjohnson.github.io/junco/reference/safe_t_test.md)
(i.e., rows with missing values in `.var` are removed prior to
computation). For unpaired cases, missing values are removed separately
from each sample before computation.

## Examples

``` r
df1 <- data.frame(
  USUBJID = c("X01", "X02", "X03", "X04", "X05"),
  CHG = c(4, 1, -1, 9, -2)
)
df2 <- data.frame(
  USUBJID = c("X01", "X02", "X03", "X04", "X05"),
  CHG = c(-2, 4, -2, 5, 2)
)

# Paired
s_diff_mean_ci(df1, df2, "CHG", paired = TRUE, paired_by = "USUBJID")
#> $diff_mean_ci
#> diff_mean    ci_lwr    ci_upr 
#>  0.800000 -4.569389  6.169389 
#> attr(,"label")
#> [1] "Difference in Means + 95% CI"
#> 

# Unpaired
s_diff_mean_ci(df1, df2, "CHG")
#> $diff_mean_ci
#> diff_mean    ci_lwr    ci_upr 
#>  0.800000 -4.980938  6.580938 
#> attr(,"label")
#> [1] "Difference in Means + 95% CI"
#> 
```
