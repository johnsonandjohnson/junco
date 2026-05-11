# Descriptive Statistics for Multiple Univariate Variables with Optional Reference-Based Comparison

**\[experimental\]**

An analysis function for computing descriptive statistics for multiple
univariate variables. It uses
[`s_summary_diff()`](https://johnsonandjohnson.github.io/junco/reference/s_summary_diff.md)
to compute statistics defined in `stats_vars$stat` for variables
specified in `stats_vars$var`. Essentially, each row in `stats_vars`
defines one (variable, statistic) pair. The output is ordered according
to `stats_vars`.

Optionally, statistics may be computed with respect to a reference
group, enabling differences in means (for numeric variables) via a
reference dataset derived from `ref_path` and `.spl_context`.

## Usage

``` r
a_summary_diff_mvars(
  df,
  .var = NULL,
  .spl_context = NULL,
  ref_path = NULL,
  na_rm_var = TRUE,
  stats_vars,
  labels_vars = NULL,
  formats_vars = NULL,
  indent_mods_vars = NULL,
  ...
)

a_summary_diff_mvars_label(
  df,
  .var = NULL,
  .spl_context = NULL,
  ref_path = NULL,
  na_rm_var = TRUE,
  stats_vars,
  labels_vars = NULL,
  formats_vars = NULL,
  indent_mods_vars = NULL,
  label,
  label_indent_mod = 0L,
  ...
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`character(1)` or `NULL`)  
  Column name in `df` used only for row filtering when
  `na_rm_var = TRUE`. It does not define the variables for which
  statistics are computed (these are specified in `stats_vars$var`). If
  `na_rm_var = TRUE`, `.var` must be provided and exist in `df`;
  otherwise, it is ignored.

- .spl_context:

  (`data.frame` or `NULL`)  
  Information about ancestor split states passed by **rtables**. It is
  ignored if `ref_path` is `NULL`.

- ref_path:

  (`character` or `NULL`)  
  Global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).
  It is used to construct `.ref_group` and `.in_ref_col`, which are
  passed to
  [`s_summary_diff()`](https://johnsonandjohnson.github.io/junco/reference/s_summary_diff.md)
  to compute the comparison statistics for variables in `df` and the
  reference `.ref_group` data set.

- na_rm_var:

  (`logical(1)`)  
  If `TRUE`, rows with missing values in `.var` are removed from `df`
  before computing statistics defined in `stats_vars`. In this case,
  `.var` must be provided and must exist in `df`.

- stats_vars:

  (`data.frame`)  
  Specification of statistics to compute for each variable. Must contain
  two columns:

  - `stat` - statistics to compute (allowed values defined by
    [`s_summary_diff()`](https://johnsonandjohnson.github.io/junco/reference/s_summary_diff.md)).

  - `var` - variable names in `df` for which the statistics are
    computed.

- labels_vars:

  (`list` or `NULL`)  
  Optional custom labels for statistics. Must be a named list with names
  matching a subset of `stats_vars$var`. The format of each element is
  defined by the `labels_in` argument in
  [`junco_get_labels_from_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md).

- formats_vars:

  (`list` or `NULL`)  
  Optional custom formats for statistics. Must be a named list with
  names matching a subset of `stats_vars$var`. The format of each
  element is defined by the `formats_in` argument in
  [`junco_get_formats_from_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md).

- indent_mods_vars:

  (`list` or `NULL`)  
  Optional custom indentation modifiers for statistics. Must be a named
  list with names matching a subset of `stats_vars$var`. The format of
  each element is defined by the `indents_in` argument in
  [`junco_get_indents_from_stats()`](https://johnsonandjohnson.github.io/junco/reference/default_stats_formats_labels.md).

- ...:

  Additional arguments passed on to `a_summary_diff_mvars()`.

- label:

  (`character(1)` or `function`)  
  Label to be displayed for the section. If a function, it must accept a
  single argument `.spl_context` and return a character string.

- label_indent_mod:

  (`integer(1)`)  
  Indentation level applied to the label row.

## Value

`RowsVerticalSection` with computed values of chosen statistics for the
specified variables.

A `RowsVerticalSection` object with a prepended section label.

## Functions

- `a_summary_diff_mvars_label()`: A wrapper around
  `a_summary_diff_mvars()` that prepends a label to the resulting table
  section containing the computed statistics.

## Examples

``` r
df <- data.frame(
  USUBJID = rep(1:6, each = 2),
  AVISIT = rep(c("Baseline", "Day 1"), 6),
  AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32),
  ABLFL = rep(c(TRUE, FALSE), 6),
  BASE = rep(c(1, 2, 13, 15, 43, 24), each = 2),
  CHG = c(0, 2, 0, 7, 0, 6, 0, 8, 0, 13, 0, 8)
)
df
#>    USUBJID   AVISIT AVAL ABLFL BASE CHG
#> 1        1 Baseline    1  TRUE    1   0
#> 2        1    Day 1    3 FALSE    1   2
#> 3        2 Baseline    2  TRUE    2   0
#> 4        2    Day 1    9 FALSE    2   7
#> 5        3 Baseline   13  TRUE   13   0
#> 6        3    Day 1   19 FALSE   13   6
#> 7        4 Baseline   15  TRUE   15   0
#> 8        4    Day 1   23 FALSE   15   8
#> 9        5 Baseline   43  TRUE   43   0
#> 10       5    Day 1   56 FALSE   43  13
#> 11       6 Baseline   24  TRUE   24   0
#> 12       6    Day 1   32 FALSE   24   8

stats_vars <- data.frame(
  stat = c("n", "mean_sd", "mean_sd"),
  var = c("CHG", "BASE", "CHG")
)

labels_vars <- list(BASE = c(mean_sd = "Baseline Mean (SD)"))

a_summary_diff_mvars(
  df,
  stats_vars = stats_vars,
  na_rm_var = FALSE,
  labels_vars = labels_vars
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>       row_name formatted_cell indent_mod          row_label
#> 1        CHG.n             12          0                  n
#> 2 BASE.mean_sd 16.33 (14.914)          0 Baseline Mean (SD)
#> 3  CHG.mean_sd   3.67 (4.519)          0          Mean (SD)

a_summary_diff_mvars_label(
  df,
  stats_vars = stats_vars,
  na_rm_var = FALSE,
  labels_vars = labels_vars,
  label = "Change from Baseline"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>       row_name formatted_cell indent_mod            row_label
#> 1                                      0 Change from Baseline
#> 2        CHG.n             12          0                    n
#> 3 BASE.mean_sd 16.33 (14.914)          0   Baseline Mean (SD)
#> 4  CHG.mean_sd   3.67 (4.519)          0            Mean (SD)

label_change <- function(spl_cntxt) {
  last_split <- length(spl_cntxt$split)
  paste("Change from Baseline to", spl_cntxt$value[last_split])
}

library(rtables)

lyt <- basic_table() |>
  append_topleft("Parameter") |>
  split_cols_by("ARM") |>
  split_rows_by(
    "PARAMCD",
    split_fun = keep_split_levels(c("DIABP", "SYSBP")),
    labels_var = "PARAM",
    child_labels = "visible",
    section_div = " "
  ) |>
  summarize_row_groups(
    "AVAL",
    cfun = c_summary_subset_label,
    extra_args = list(
      subset_expr = expression(ABLFL == "Y"),
      .stats = c("n", "mean_sd"),
      .indent_mods = c(n = 1L, mean_sd = 1L),
      label = "BASELINE"
    )
  ) |>
  split_rows_by(
    "AVISIT",
    split_fun = keep_split_levels(c("WEEK 1 DAY 8", "WEEK 2 DAY 15")),
    indent_mod = -1
  ) |>
  analyze(
    "AVAL",
    afun = rtables::simple_analysis,
    show_labels = "hidden"
  ) |>
  analyze(
    "CHG",
    afun = a_summary_diff_mvars_label,
    extra_args = list(
      stats_vars = data.frame(
        stat = c("n", "mean_sd", "mean_sd"),
        var = c("CHG", "BASE", "CHG")
      ),
      labels_vars = list(BASE = c(mean_sd = "Baseline Mean (SD)")),
      label = label_change,
      label_indent_mod = -1L
    ),
    show_labels = "hidden"
  )

tbl <- build_table(lyt, formatters::ex_advs)
tbl
#> Parameter                                   A: Drug X        B: Placebo     C: Combination
#> ——————————————————————————————————————————————————————————————————————————————————————————
#> Diastolic Blood Pressure                                                                  
#>   BASELINE                                                                                
#>     n                                          134              134              132      
#>     Mean (SD)                             48.60 (7.962)    50.44 (7.948)    51.11 (7.790) 
#>   WEEK 1 DAY 8                                                                            
#>     Mean                                      50.26            49.67            48.86     
#>   Change from Baseline to WEEK 1 DAY 8                                                    
#>     n                                          134              134              132      
#>     Baseline Mean (SD)                    48.60 (7.962)    50.44 (7.948)    51.11 (7.790) 
#>     Mean (SD)                             1.66 (10.774)    -0.77 (10.947)   -2.25 (10.380)
#>   WEEK 2 DAY 15                                                                           
#>     Mean                                      50.84            49.72            49.98     
#>   Change from Baseline to WEEK 2 DAY 15                                                   
#>     n                                          134              134              132      
#>     Baseline Mean (SD)                    48.60 (7.962)    50.44 (7.948)    51.11 (7.790) 
#>     Mean (SD)                             2.23 (11.752)    -0.72 (12.444)   -1.13 (11.639)
#>                                                                                           
#> Systolic Blood Pressure                                                                   
#>   BASELINE                                                                                
#>     n                                          134              134              132      
#>     Mean (SD)                             49.41 (8.467)    50.25 (8.531)    48.49 (7.184) 
#>   WEEK 1 DAY 8                                                                            
#>     Mean                                      50.70            49.26            49.65     
#>   Change from Baseline to WEEK 1 DAY 8                                                    
#>     n                                          134              134              132      
#>     Baseline Mean (SD)                    49.41 (8.467)    50.25 (8.531)    48.49 (7.184) 
#>     Mean (SD)                             1.30 (12.814)    -0.98 (11.221)   1.15 (10.185) 
#>   WEEK 2 DAY 15                                                                           
#>     Mean                                      49.37            49.38            49.26     
#>   Change from Baseline to WEEK 2 DAY 15                                                   
#>     n                                          134              134              132      
#>     Baseline Mean (SD)                    49.41 (8.467)    50.25 (8.531)    48.49 (7.184) 
#>     Mean (SD)                             -0.04 (11.936)   -0.87 (10.968)    0.77 (9.716) 
```
