# Combined Proportion, Risk Difference, and Risk Difference Test Analysis Function with Method Selection Based on the Mantel-Fleiss Criterion

Performs a combined analysis of proportions, treatment-vs-control risk
differences, and tests of risk differences.

The method used for the risk difference and risk difference test is
selected based on the Mantel-Fleiss criterion. If the Mantel-Fleiss (MF)
criterion is satisfied, the Cochran-Mantel-Haenszel (CMH) method with
Sato variance is used for both the risk difference and the corresponding
statistical test.

If the Mantel-Fleiss criterion is not satisfied, an exact method is
used. Specifically, the exact unconditional method is used for
estimation of the risk difference, and Fisher's exact test is used for
the hypothesis test.

## Usage

``` r
a_combo_prop_diff_pval_mf(
  df,
  .var,
  .df_row,
  .spl_context,
  val,
  colpaths,
  id = "USUBJID",
  trt_var,
  ctrl_group,
  variables = list(strata = NULL),
  mf_threshold = NULL,
  conf_level = 0.95,
  alternative = c("two.sided", "less", "greater"),
  exact_footnote = rtables::RefFootnote("Exact Inference", 1L, "+"),
  .stats = NULL,
  .formats = NULL,
  ...
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`character(1)`)  
  Name of a response variable in `df` and `.df_row`. The variable must
  be a factor.

- .df_row:

  (`data.frame`)  
  Data set containing all analysis variables across all columns for the
  given row split. It is required for risk difference and risk
  difference test calculations to obtain the reference data.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- val:

  (`character(1)`)  
  The value used to identify the response of interest. An observation is
  considered a response of interest when `df[[.var]] == val` (and
  `.df_row[[.var]] == val` for the reference group). Since `df[[.var]]`
  and `.df_row[[.var]]` must be factors, `val` must be a `character`
  value matching one of their levels.

- colpaths:

  (`list(3)`)  
  A named list containing elements defining the column paths
  corresponding to the three types of analysis. Supported names are
  `"prop"`, `"diff"`, and `"pval"`. Each element must contain the column
  path used to identify the corresponding column(s). Each column path
  must contain at least two elements and have an even length. For
  example, valid column paths include `c("ARM", "Placebo")` or
  `c("ARM", "*")`. See
  [`rtables::col_paths()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_col_row_df.html)
  for details on column path specification.

- id:

  (`string`)  
  subject variable name.

- trt_var:

  (`character(1)`)  
  The name of the treatment variable. This variable is used to identify
  the control group in `.df_row`. It is used only for risk difference
  and risk difference test calculations.

- ctrl_group:

  (`character(1)`)  
  The value of `trt_var` identifying the control treatment group. It is
  used only for risk difference and risk difference test calculations.

- variables:

  (`list` or `NULL`)  
  Optional stratification variables used by the Cochran-Mantel-Haenszel
  analysis. These are specified as `list(strata = <strata_vars>)`, where
  `<strata_vars>` is a character vector containing column names in `df`.
  All specified strata variables must be factors. Strata variables are
  used only for risk difference and risk difference test calculations.

- mf_threshold:

  (`numeric(1)` or `NULL`)  
  Threshold for the Mantel-Fleiss criterion. If `NULL`, the default from
  [`tern::mantel_fleiss_crit()`](https://pharmaverse.github.io/tern/latest-tag/reference/mantel_fleiss_crit.html)
  is used.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- alternative:

  (`string`)  
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- exact_footnote:

  (`character(1)` or `RefFootnote`)  
  Footnote used to indicate that the unconditional exact method was
  actually used for the analysis.

- .stats:

  (`list`)  
  A named list of at most three elements specifying the statistic(s) to
  calculate for each type of analysis. Supported names are `"prop"`,
  `"diff"`, and `"pval"`. The supported values for each analysis type
  are determined by the `.stats` argument of
  [`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md),
  [`a_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md),
  and
  [`a_test_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/test_proportion_diff_mf.md),
  respectively.

  If a statistic is not supplied for any of the three analysis types,
  the following defaults are used:

  - `prop = "count_unique_denom_fraction"`

  - `diff = "diff_est_ci"`

  - `pval = "pval"`

- .formats:

  (`list`)  
  A named list of at most three elements specifying the formats for the
  statistics specified in `.stats`. Supported names are `"prop"`,
  `"diff"`, and `"pval"`. The format specified for each analysis type is
  passed to the corresponding analysis function.

- ...:

  Additional arguments reserved for compatibility with the rtables
  analysis function interface.

## Value

Returns the corresponding `CellValue` or `RowsVerticalSection` object
with formatted results.

## Details

The analysis function determines which type of result to calculate based
on the current column path specified by `colpaths`.

- `"prop"`: calculates and displays the proportion for the current
  analysis facet.

- `"diff"`: calculates the treatment-vs-control proportion difference.

- `"pval"`: calculates the treatment-vs-control p-value.

For proportion columns,
[`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
is used. For risk difference and p-value columns, the control group is
identified from `.df_row` using `trt_var` and `ctrl_group`, and the
corresponding
[`a_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md)
or
[`a_test_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/test_proportion_diff_mf.md)
function is called.

The current column is identified using
[`in_column()`](https://johnsonandjohnson.github.io/junco/reference/cur_col_split_path_utils.md)
against each element of `colpaths` list. If the current column does not
correspond to any of the configured `prop`, `diff`, or `pval` paths, an
empty
[`rtables::rcell()`](https://insightsengineering.github.io/rtables/latest-tag/reference/rcell.html).

The function first determines the current row label from `.spl_context`.
If the current split represents the root of the analysis, the label
`"Full analysis set"` is used; otherwise, the value of the leaf split is
used.

## See also

[`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md),
[`a_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/proportion_diff_mf.md),
[`a_test_proportion_diff_mf()`](https://johnsonandjohnson.github.io/junco/reference/test_proportion_diff_mf.md),

## Author

WW

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

adrs_f <- formatters::ex_adrs |>
  filter(
    ARM %in% c("A: Drug X", "B: Placebo"),
    PARAMCD == "BESRSPI"
  ) |>
  droplevels() |>
  mutate(
    rsp = factor(ifelse(AVALC == "CR", "Y", "N")),
    ARM_FOR_RISKCOLS = ARM # Required for Risk Difference and p-value columns
  )

riskcols_combodf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "diff", "Risk Difference", "A: Drug X", list(),
  "pval", "p-value", "B: Placebo", list()
)

col_split_fun <- add_combo_levels(
  riskcols_combodf,
  keep_levels = riskcols_combodf$valname
)

# Additional arguments for a_combo_prop_diff_pval_mf().
extra_args <- list(
  colpaths = list(
    prop = c("ARM", "*"),
    diff = c("ARM_FOR_RISKCOLS", "diff"),
    pval = c("ARM_FOR_RISKCOLS", "pval")
  ),
  id = "SUBJID",
  trt_var = "ARM",
  ctrl_group = "B: Placebo",
  val = "Y",
  variables = list(strata = "BMRKR2"),
  .stats = list(diff = "diff")
)

lyt <- basic_table(top_level_section_div = " ") |>
  split_cols_by("ARM") |>
  split_cols_by(
    "ARM_FOR_RISKCOLS",
    split_fun = col_split_fun,
    nested = FALSE
  ) |>
  analyze(
    "rsp",
    afun = a_combo_prop_diff_pval_mf,
    extra_args = extra_args
  ) |>
  split_rows_by(
    var = "SEX",
    split_label = "Gender",
    child_labels = "hidden",
    label_pos = "visible"
  ) |>
  analyze(
    vars = "rsp",
    afun = a_combo_prop_diff_pval_mf,
    extra_args = extra_args,
    show_labels = "hidden"
  )

tbl <- build_table(lyt, adrs_f)
tbl
#>                        A: Drug X        B: Placebo     Risk Difference    p-value  
#> ———————————————————————————————————————————————————————————————————————————————————
#> Full analysis set    78/134 (58.2%)   55/134 (41.0%)        17.2           >0.999  
#>                                                                                    
#> Gender                                                                             
#>   F                  44/79 (55.7%)    29/77 (37.7%)         17.7           >0.999  
#>   M                  32/51 (62.7%)    26/55 (47.3%)         15.5           >0.999  
#>   U                   2/3 (66.7%)          0/2            66.7 {+}       >0.999 {+}
#>   UNDIFFERENTIATED        0/1               -              NA {+}        >0.999 {+}
#> ———————————————————————————————————————————————————————————————————————————————————
#> 
#> {+} - Exact Inference
#> ———————————————————————————————————————————————————————————————————————————————————
#> 
```
