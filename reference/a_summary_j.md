# Wrapper around `tern::a_summary()` with junco-specific defaults

**\[experimental\]**

This function wraps
[`tern::a_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html)
and applies junco-specific defaults for formatting-related arguments
when they are not explicitly provided by the user.

In particular, default values are generated for:

- `.labels` via
  [`junco_get_labels_from_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md)

- `.formats` via
  [`junco_get_formats_from_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md)

- `.indent_mods` via
  [`junco_get_indents_from_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md)

If `.stats` is not provided or is `NULL`, the default statistics from
[`tern::get_stats()`](https://insightsengineering.github.io/tern/latest-tag/reference/default_stats_formats_labels.html)
are used.

## Usage

``` r
a_summary_j(
  x,
  ...,
  .stats = NULL,
  .stat_names = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)
```

## Arguments

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- ...:

  additional arguments passed to
  [`s_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html),
  including:

  - `denom`: (`string`) See parameter description below.

  - `.N_row`: (`numeric(1)`) Row-wise N (row group count) for the group
    of observations being analyzed (i.e. with no column-based
    subsetting).

  - `.N_col`: (`numeric(1)`) Column-wise N (column count) for the full
    column being tabulated within.

  - `verbose`: (`flag`) Whether additional warnings and messages should
    be printed. Mainly used to print out information about factor
    casting. Defaults to `TRUE`. Used for `character`/`factor` variables
    only.

- .stats:

  (`character`)  
  statistics to select for the table.

  Options for numeric variables are:
  `'n', 'sum', 'mean', 'sd', 'se', 'mean_sd', 'mean_se', 'mean_ci', 'mean_sei', 'mean_sdi', 'mean_pval', 'median', 'mad', 'median_ci', 'quantiles', 'iqr', 'range', 'min', 'max', 'median_range', 'cv', 'geom_mean', 'geom_sd', 'geom_mean_sd', 'geom_mean_ci', 'geom_cv', 'median_ci_3d', 'mean_ci_3d', 'geom_mean_ci_3d'`

  Options for non-numeric variables are:
  `'n', 'count', 'count_fraction', 'count_fraction_fixed_dp', 'fraction', 'n_blq'`

- .stat_names:

  (`character`)  
  names of the statistics that are passed directly to name single
  statistics (`.stats`). This option is visible when producing
  [`rtables::as_result_df()`](https://insightsengineering.github.io/rtables/latest-tag/reference/data.frame_export.html)
  with `make_ard = TRUE`.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `"auto"` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Each element of the vector should be
  a name-value pair with name corresponding to a statistic specified in
  `.stats` and value the indentation for that statistic's row label.

## Value

Returns the same type of output as
[`tern::a_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html),
with optional junco-based default formatting applied.

## Details

User-supplied values for `.labels`, `.formats`, and `.indent_mods` are
used as-is and only completed where needed by the corresponding junco
helper functions. No modification is performed if these arguments are
fully specified.

## See also

[`tern::a_summary()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html),
[`tern::get_stats()`](https://insightsengineering.github.io/tern/latest-tag/reference/default_stats_formats_labels.html)

## Examples

``` r
.stats <- c("n", "mean_sd", "median_range")
tern::a_summary(1:10, .stats = .stats)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>       row_name   formatted_cell indent_mod          row_label
#> 1            n               10          0                  n
#> 2      mean_sd        5.5 (3.0)          0          Mean (SD)
#> 3 median_range 5.5 (1.0 - 10.0)          0 Median (Min - Max)
a_summary_j(1:10, .stats = .stats)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>       row_name   formatted_cell indent_mod         row_label
#> 1            n               10          0                 n
#> 2      mean_sd     5.50 (3.028)          0         Mean (SD)
#> 3 median_range 5.50 (1.0, 10.0)          0 Median (min, max)
a_summary_j(1:10, .stats = .stats, .formats = c(mean_sd = "xx (xx.x)"))
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>       row_name   formatted_cell indent_mod         row_label
#> 1            n               10          0                 n
#> 2      mean_sd        5.5 (3.0)          0         Mean (SD)
#> 3 median_range 5.50 (1.0, 10.0)          0 Median (min, max)
a_summary_j(1:10)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>           row_name     formatted_cell indent_mod                   row_label
#> 1                n                 10          0                           n
#> 2              sum               55.0          0                         Sum
#> 3             mean               5.50          0                        Mean
#> 4               sd              3.028          0                          SD
#> 5               se              0.957          0                          SE
#> 6          mean_sd       5.50 (3.028)          0                   Mean (SD)
#> 7          mean_se       5.50 (0.957)          0                   Mean (SE)
#> 8          mean_ci       (3.33, 7.67)          0                 Mean 95% CI
#> 9         mean_sei       (4.54, 6.46)          0               Mean -/+ 1xSE
#> 10        mean_sdi       (2.47, 8.53)          0               Mean -/+ 1xSD
#> 11       mean_pval             <0.001          0 Mean p-value (H0: mean = 0)
#> 12          median               5.50          0                      Median
#> 13             mad                0.0          0   Median Absolute Deviation
#> 14       median_ci       (2.00, 9.00)          0               Median 95% CI
#> 15       quantiles         3.00, 8.00          0             25% and 75%-ile
#> 16             iqr                5.0          0                         IQR
#> 17           range          1.0, 10.0          0                    Min, max
#> 18             min                1.0          0                     Minimum
#> 19             max               10.0          0                     Maximum
#> 20    median_range   5.50 (1.0, 10.0)          0           Median (min, max)
#> 21              cv              55.05          0                      CV (%)
#> 22       geom_mean                4.5          0              Geometric Mean
#> 23         geom_sd              2.081          0                Geometric SD
#> 24    geom_mean_sd       4.53 (2.081)          0         Geometric Mean (SD)
#> 25    geom_mean_ci       (2.68, 7.65)          0       Geometric Mean 95% CI
#> 26         geom_cv               84.3          0         CV % Geometric Mean
#> 27    median_ci_3d  5.50 (2.00, 9.00)          0             Median (95% CI)
#> 28      mean_ci_3d 5.50 (3.33 - 7.67)          0               Mean (95% CI)
#> 29 geom_mean_ci_3d 4.53 (2.68 - 7.65)          0     Geometric Mean (95% CI)
```
