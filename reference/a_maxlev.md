# Calculate Count and Percentage of the Maximum Level of an Ordered Factor per Subject.

A formatted analysis function used as an `afun` in
[analyze](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html)
and as a `cfun` in
[summarize_row_groups](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

It computes count and proportion statistics for the maximum level of an
ordered factor, `df[[.var]]`, for each unique subject in `df[[id]]`.
Specifically, for each subject, the function identifies the highest
level of `df[[.var]]`, producing one value per subject. Then, if
`any_level = TRUE`, the function reports the total number of maximum
values, excluding those specified in `any_level_exclude`. Otherwise, it
tabulates the frequency of each maximum level across all subjects.

This function is particularly useful for identifying the maximum
severity of adverse events in a treatment sequence, where the most
severe event experienced by a subject is used for reporting.

## Usage

``` r
a_maxlev(
  df,
  labelstr = NULL,
  .var,
  .spl_context,
  id = "USUBJID",
  .alt_df_full = NULL,
  any_level = FALSE,
  any_level_exclude = "Missing",
  ...
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

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- id:

  (`string`)  
  subject variable name.

- .alt_df_full:

  (`dataframe`)  
  A dataset used to compute the denominator for proportions. This is
  required when the same subject appears multiple times in the dataset
  due to treatment sequences. `colnames(.alt_df_full)` must be a
  superset of `id`. This argument gets populated by the rtables split
  machinery (see
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html)).

- any_level:

  (`flag`)  
  Should be set to `TRUE` when the function is used as a `cfun`.

- any_level_exclude:

  (`character`)  
  Applicable only when `any_level = TRUE`. Specifies levels of
  `df[[.var]]` to exclude from the statistic (default = "Missing").

- ...:

  additional arguments for the lower level functions.

## Value

A `RowsVerticalSection` object.

## Details

For each unique subject, only the maximum level of the ordered factor
`df[[.var]]` is included in the final count and percentage statistics.

## Note

The denominator for proportions is computed using the `denom_df`
argument. This serves as a temporary workaround until the next version
of `rtables` is released, which will support `.alt_count_df` for use in
`afun`/`cfun`.

## Examples

``` r
treatments <- factor(c("a", "b", "c"))
ae_severities <- c("Missing", "Mild", "Moderate", "Severe")
ae_severities <- ordered(ae_severities, levels = ae_severities)
my_adae <- data.frame(
  ID = c(1, 1, 1, 2, 2, 3, 3, 3, 3, 4),
  TRT = factor(c("a", "b", "b", "b", "c", "c", "a", "c", "b", "b")),
  AESEV = ae_severities[c(4L, 1L, 2L, 1L, 2L, 1L, 2L, 3L, 1L, 2L)]
)
my_adsl <- data.frame(
  ID = rep(1:5, each = 3),
  TRT = factor(rep(c("a", "b", "c"), times = 5))
)

aesevall_spf <- make_combo_splitfun(
  nm = "AESEV_ALL",
  label = "Any AE",
  levels = NULL,
)

lyt <- basic_table() |>
  split_cols_by("TRT") |>
  add_overall_col("Total") |>
  split_rows_by("AESEV", split_fun = aesevall_spf) |>
  summarize_row_groups(
    "AESEV",
    cfun = a_maxlev,
    extra_args = list(id = "ID", any_level = TRUE)
  ) |>
  analyze(
    "AESEV",
    afun = a_maxlev,
    extra_args = list(id = "ID")
  )
build_table(lyt, my_adae, alt_counts_df = my_adsl)
#>                  a           b           c         Total  
#> ——————————————————————————————————————————————————————————
#> Any AE       2 (40.0%)   2 (40.0%)   2 (40.0%)   4 (80.0%)
#>   Missing        0       2 (40.0%)       0           0    
#>   Mild       1 (20.0%)   2 (40.0%)   1 (20.0%)   2 (40.0%)
#>   Moderate       0           0       1 (20.0%)   1 (20.0%)
#>   Severe     1 (20.0%)       0           0       1 (20.0%)
```
