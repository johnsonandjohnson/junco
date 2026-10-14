# Difference in Means

**\[stable\]**

Computes estimates and associated inferential statistics for the
estimand defined as the difference in population means between two
distributions.

The following quantities are available:

- diff_means_n1:

  Sample size of Group 1.

- diff_means_n2:

  Sample size of Group 2.

- diff_means_est:

  Point estimate of the difference in population means.

- diff_means_se:

  Standard error of the estimator of the difference in population means.

- diff_means_est_se:

  The point estimate and the standard error.

- diff_means_ci:

  Confidence interval for the difference in population means.

- diff_means_est_ci:

  The point estimate and the confidence interval.

## Usage

``` r
s_diff_means(df1, df2, .var, paired = FALSE, paired_by = NULL, ...)

a_diff_means(
  df,
  .var,
  .spl_context,
  ref_path = NULL,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
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
  Name of the column in the dataset(s) containing observed sample
  values.

- paired:

  (`logical(1)`)  
  Whether the samples are paired.

- paired_by:

  (`character` or `NULL`)  
  Column name(s) in `df1` and `df2` used to match observations between
  datasets. Required when `paired = TRUE` and must uniquely identify
  each pair in both datasets.

- ...:

  Additional named arguments passed to `s_diff_means()` and
  `safe_t_test()`.

- df:

  (`data.frame`)  
  Dataset for the first sample.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- ref_path:

  (`character`)  
  Global reference group specification; see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).
  This specifies the column path for the second sample. Specifically,
  the second sample is `.ref_group[[.var]]`, where `.ref_group` is
  computed by
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
  using `.spl_context` and `ref_path`.

  This is a workaround for an
  [rtables](https://insightsengineering.github.io/rtables/latest-tag/reference/rtables-package.html)
  limitation in supporting `.ref_group` / `.in_ref_col` in the presence
  of stacked headers. Normally, `.ref_group` and `.in_ref_col` are
  provided by the
  [rtables](https://insightsengineering.github.io/rtables/latest-tag/reference/rtables-package.html)
  tabulation framework when the current column split is within the
  reference variable.

- .stats:

  (`character`)  
  Statistics to include in the table.

  Available options are:
  `'diff_means_n1', 'diff_means_n2', 'diff_means_est', 'diff_means_se', 'diff_means_est_se', 'diff_means_ci', 'diff_means_est_ci'`

  If `NULL`, all statistics are included.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

## Value

- `s_diff_means()` returns a named `list` containing the quantities
  described in the Description section.

&nbsp;

- `a_diff_means()` returns formatted
  [`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
  result with the quantities requested in `.stats`.

## Details

Supports both non-paired and paired samples. For non-paired samples, the
estimator is defined as the difference between sample means. For paired
samples, the estimator is defined as the mean of within-pair
differences.

Inferential statistics, including standard errors and confidence
intervals, are obtained using a t-distribution framework via the
internal `safe_t_test()` function.

It supports only numeric variables. Only complete (non-missing)
observations are used in statistical computations. Non-finite values
(i.e., `Inf`, `-Inf`) are not allowed.

## Functions

- `s_diff_means()`: Statistical function that computes estimates of the
  difference in means. The first sample is taken from `df1[[.var]]` and
  the second from `df2[[.var]]`.

  If `paired = FALSE`, missing values are removed separately from each
  sample prior to statistical computations. If `paired = TRUE`,
  observations are matched using `paired_by` prior to statistical
  analysis, and only complete pairs are used.

  Data extraction, alignment, and missing-value handling are delegated
  to the internal `extract_vectors()` function, which prepares cleaned
  numeric vectors for both paired and unpaired settings.

  Inferential statistics are then computed using `safe_t_test()` applied
  to the processed vectors.

- `a_diff_means()`: Formatted analysis function which is used as `afun`
  in
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

  The sample from the first distribution is `df[[.var]]`, and the sample
  from the second distribution is `.ref_group[[.var]]`, where
  `.ref_group` is computed by
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md)
  using `.spl_context` and `ref_path`.

  If `ref_path = NULL`, then `a_diff_means()` returns an
  [`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
  result with an empty `formatted_cell` for every statistic specified in
  `.stats`.

## See also

`safe_t_test()`, `extract_vectors()`

## Author

WW

## Examples

``` r

## Statistics function:

df1 <- data.frame(id = c("A", "B", "C", "D"), value = 1:4)
df2 <- data.frame(id = c("A", "C", "D", "E", "F"), value = c(3:1, NA, 16))

# Unpaired samples.
s_diff_means(df1, df2, "value", conf.level = .8)
#> $diff_means_n1
#> diff_means_n1 
#>             4 
#> attr(,"label")
#> [1] "Difference in Means Sample Size (Group 1)"
#> 
#> $diff_means_n2
#> diff_means_n2 
#>             4 
#> attr(,"label")
#> [1] "Difference in Means Sample Size (Group 2)"
#> 
#> $diff_means_est
#> diff_means_est 
#>             -3 
#> attr(,"label")
#> [1] "Difference in Means"
#> 
#> $diff_means_se
#> diff_means_se 
#>      3.582364 
#> attr(,"label")
#> [1] "Difference in Means SE"
#> 
#> $diff_means_est_se
#> diff_means_est  diff_means_se 
#>      -3.000000       3.582364 
#> attr(,"label")
#> [1] "Difference in Means (SE)"
#> 
#> $diff_means_ci
#> diff_means_ci_lwr diff_means_ci_upr 
#>         -8.768895          2.768895 
#> attr(,"conf.level")
#> [1] 0.8
#> attr(,"label")
#> [1] "Difference in Means 80% CI"
#> 
#> $diff_means_est_ci
#>    diff_means_est diff_means_ci_lwr diff_means_ci_upr 
#>         -3.000000         -8.768895          2.768895 
#> attr(,"conf.level")
#> [1] 0.8
#> attr(,"label")
#> [1] "Difference in Means (80% CI)"
#> 

# Paired samples.
s_diff_means(df1, df2, "value", paired = TRUE, paired_by = "id")
#> $diff_means_n1
#> diff_means_n1 
#>             3 
#> attr(,"label")
#> [1] "Difference in Means Sample Size (Group 1)"
#> 
#> $diff_means_n2
#> diff_means_n2 
#>             3 
#> attr(,"label")
#> [1] "Difference in Means Sample Size (Group 2)"
#> 
#> $diff_means_est
#> diff_means_est 
#>      0.6666667 
#> attr(,"label")
#> [1] "Difference in Means"
#> 
#> $diff_means_se
#> diff_means_se 
#>      1.452966 
#> attr(,"label")
#> [1] "Difference in Means SE"
#> 
#> $diff_means_est_se
#> diff_means_est  diff_means_se 
#>      0.6666667      1.4529663 
#> attr(,"label")
#> [1] "Difference in Means (SE)"
#> 
#> $diff_means_ci
#> diff_means_ci_lwr diff_means_ci_upr 
#>         -5.584943          6.918276 
#> attr(,"conf.level")
#> [1] 0.95
#> attr(,"label")
#> [1] "Difference in Means 95% CI"
#> 
#> $diff_means_est_ci
#>    diff_means_est diff_means_ci_lwr diff_means_ci_upr 
#>         0.6666667        -5.5849428         6.9182761 
#> attr(,"conf.level")
#> [1] 0.95
#> attr(,"label")
#> [1] "Difference in Means (95% CI)"
#> 


## Analysis  function:

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by(
    "PARAMCD",
    labels_var = "PARAM",
    split_fun = keep_split_levels(c("SYSBP", "DIABP"))
  ) |>
  split_rows_by(
    "AVISIT",
    split_fun = keep_split_levels(c("BASELINE", "WEEK 1 DAY 8"))
  ) |>
  analyze(
    "AVAL",
    afun = tern::a_summary,
    extra_args = list(.stats = c("n", "mean")),
    show_labels = "hidden"
  ) |>
  analyze(
    "AVAL",
    afun = a_diff_means,
    table_names = "aval_diff",
    extra_args = list(
      .stats = "diff_means_est_ci",
      ref_path = c("ARM", "B: Placebo")
    ),
    show_labels = "hidden"
  )

tbl <- build_table(lyt, formatters::ex_advs)
tbl
#>                                         A: Drug X        B: Placebo     C: Combination   
#> —————————————————————————————————————————————————————————————————————————————————————————
#> Systolic Blood Pressure                                                                  
#>   BASELINE                                                                               
#>     n                                      134              134               132        
#>     Mean                                  49.4              50.2             48.5        
#>     Difference in Means (95% CI)   -0.84 (-2.88, 1.21)                -1.75 (-3.65, 0.15)
#>   WEEK 1 DAY 8                                                                           
#>     n                                      134              134               132        
#>     Mean                                  50.7              49.3             49.6        
#>     Difference in Means (95% CI)   1.44 (-0.71, 3.59)                 0.38 (-1.63, 2.40) 
#> Diastolic Blood Pressure                                                                 
#>   BASELINE                                                                               
#>     n                                      134              134               132        
#>     Mean                                  48.6              50.4             51.1        
#>     Difference in Means (95% CI)   -1.84 (-3.75, 0.07)                0.67 (-1.24, 2.57) 
#>   WEEK 1 DAY 8                                                                           
#>     n                                      134              134               132        
#>     Mean                                  50.3              49.7             48.9        
#>     Difference in Means (95% CI)   0.59 (-1.25, 2.42)                 -0.82 (-2.71, 1.07)
```
