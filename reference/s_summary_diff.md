# Descriptive Statistics for Univariate Data with Optional Reference Comparison

**\[experimental\]**

Computes descriptive statistics for a single variable `df[[.var]]` using
[`tern::s_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html),
which dispatches type-specific methods depending on the S3 class of the
input (e.g., character, factor, logical, numeric).

Optionally, it computes estimates and associated inferential statistics
for the difference in population means between two underlying
distributions, based on observed samples `df[[.var]]` and
`.ref_group[[.var]]` using
[`s_diff_means()`](https://johnsonandjohnson.github.io/junco/reference/diff_means.md).
These statistics are applicable only to numeric data.

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
  [`tern::s_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html).
  If difference-in-means statistics are requested, `df[[.var]]` must be
  numeric.

- .stats:

  (`character` or `NULL`)  
  Names of statistics to be computed. For numerical data, the names of
  supported statistics are listed via
  [`tern::get_stats()`](https://pharmaverse.github.io/tern/latest-tag/reference/default_stats_formats_labels.html)
  for statistics for a single variable and
  `junco_get_stats("diff_means")` for difference-in-means statistics. If
  `NULL`, all available statistics for numerical data are computed.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`logical`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- control:

  (`list`)  
  List of control options passed to
  [`tern::s_summary()`](https://pharmaverse.github.io/tern/latest-tag/reference/analyze_variables.html).
  If difference-in-means statistics are requested, `control$conf_level`
  specifies the confidence level of the interval.

- ...:

  Additional named arguments passed to
  [`s_diff_means()`](https://johnsonandjohnson.github.io/junco/reference/diff_means.md),
  except `df1`, `df2`, `.var`, and `conf.level`, which are already
  passed.

## Value

A named `list` with the requested statistics.

## Author

WW

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

.stats <- c("n", "mean_sd", "diff_means_n1", "diff_means_n2", "diff_means_est_ci")

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
#> $diff_means_n1
#> diff_means_n1 
#>             5 
#> attr(,"label")
#> [1] "Difference in Means Sample Size (Group 1)"
#> 
#> $diff_means_n2
#> diff_means_n2 
#>             5 
#> attr(,"label")
#> [1] "Difference in Means Sample Size (Group 2)"
#> 
#> $diff_means_est_ci
#>    diff_means_est diff_means_ci_lwr diff_means_ci_upr 
#>          0.400000         -5.632096          6.432096 
#> attr(,"conf.level")
#> [1] 0.95
#> attr(,"label")
#> [1] "Difference in Means (95% CI)"
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
#> $diff_means_n1
#> NULL
#> 
#> $diff_means_n2
#> NULL
#> 
#> $diff_means_est_ci
#> NULL
#> 
```
