# Descriptive Statistics for Univariate Data with Optional Reference Comparison

**\[experimental\]**

Computes descriptive statistics for a single variable `df[[.var]]` using
[`tern::s_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html),
which dispatches type-specific methods depending on the S3 class of the
input (e.g., character, factor, logical, numeric).

Optionally, it computes a difference in means with confidence interval
between `df[[.var]]` and `.ref_group[[.var]]` using
[`s_diff_mean_ci()`](https://johnsonandjohnson.github.io/junco/reference/s_diff_mean_ci.md).
This statistic is applicable only to numeric variables.

## Usage

``` r
s_summary_diff(
  df,
  .var,
  .stats = NULL,
  .ref_group = NULL,
  .in_ref_col = FALSE,
  control = tern::control_analyze_vars(),
  ...
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`character(1)`)  
  Name of the column in `df` containing the values for which statistics
  are computed. The variable type is handled by the corresponding
  methods of
  [`tern::s_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html).
  The `diff_mean_ci` statistic is only valid when `df[[.var]]` is
  numeric.

- .stats:

  (`character` or `NULL`)  
  Names of statistics to be computed. For numerical data, supported
  statistics are listed via
  `tern::get_stats(method_groups = "analyze_vars_numeric", custom_stats_in = "diff_mean_ci")`.
  If `NULL`, all available statistics for numerical data are computed.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`logical`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- control:

  (`list`)  
  List of control options passed to
  [`tern::s_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html).
  If *diff_mean_ci* statistic is requested, `control$conf_level`
  specifies the confidence level used for the interval.

- ...:

  Additional arguments passed to
  [`tern::s_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html)
  and to
  [`s_diff_mean_ci()`](https://johnsonandjohnson.github.io/junco/reference/s_diff_mean_ci.md)
  when *diff_mean_ci* is computed.

## Value

A named `list` with the requested statistics.

## Examples

``` r
df <- data.frame(
  USUBJID = c("X01", "X02", "X03", "X04", "X05"),
  TRT01A = rep("ARM_A", 5),
  PARAMCD = rep("SYSBP", 5),
  AVISIT = rep("Visit 1", 5),
  CHG = c(4, 1, -1, 9, -2)
)
df
#>   USUBJID TRT01A PARAMCD  AVISIT CHG
#> 1     X01  ARM_A   SYSBP Visit 1   4
#> 2     X02  ARM_A   SYSBP Visit 1   1
#> 3     X03  ARM_A   SYSBP Visit 1  -1
#> 4     X04  ARM_A   SYSBP Visit 1   9
#> 5     X05  ARM_A   SYSBP Visit 1  -2

rg <- data.frame(
  USUBJID = c("X06", "X07", "X08", "X09", "X10"),
  TRT01A = rep("Placebo", 5),
  PARAMCD = rep("SYSBP", 5),
  AVISIT = rep("Visit 1", 5),
  CHG = c(-2, 6, -2, 5, 2)
)
rg
#>   USUBJID  TRT01A PARAMCD  AVISIT CHG
#> 1     X06 Placebo   SYSBP Visit 1  -2
#> 2     X07 Placebo   SYSBP Visit 1   6
#> 3     X08 Placebo   SYSBP Visit 1  -2
#> 4     X09 Placebo   SYSBP Visit 1   5
#> 5     X10 Placebo   SYSBP Visit 1   2

.stats <- c("n", "mean_sd", "diff_mean_ci")

# With reference group.
s_summary_diff(df, "CHG", .stats, rg)
#> $n
#> n 
#> 5 
#> 
#> $mean_sd
#>     mean       sd 
#> 2.200000 4.438468 
#> 
#> $diff_mean_ci
#> diff_mean    ci_lwr    ci_upr 
#>  0.400000 -5.632096  6.432096 
#> attr(,"label")
#> [1] "Difference in Means + 95% CI"
#> 

# Using df as reference.
s_summary_diff(df, "CHG", .stats, df, .in_ref_col = TRUE)
#> $n
#> n 
#> 5 
#> 
#> $mean_sd
#>     mean       sd 
#> 2.200000 4.438468 
#> 
#> $diff_mean_ci
#> NULL
#> 
```
