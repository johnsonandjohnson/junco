# Two Tier Analysis Function

The analysis function used as an `afun` in
[analyze](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).
This function simulates a final additional level of nesting with a
traditional analyze call inside it.

This makes it possible to create what *appear to be* group summary or
content rows and to *optionally or conditionally* generate one or more
"detail" rows underneath it.

For example, in a disposition table, one might want counts for completed
and ongoing patients with no further detail underneath, but a breakdown
of specific reasons beneath the count of patients who discontinued
treatment.

## Usage

``` r
a_two_tier(
  df,
  labelstr = NULL,
  .var,
  .N_col,
  .df_row,
  inner_var,
  drill_down_levs,
  .spl_context,
  use_all_levels = FALSE,
  grp_fun,
  detail_fun,
  .alt_df_full = NULL,
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

- .N_col:

  (`integer`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- inner_var:

  (`string`)  
  single variable name to use when generating the detail rows.

- drill_down_levs:

  (`character`)  
  the level(s) of `.var` under which detail rows should be generated.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- use_all_levels:

  (`flag`)  
  controls which factor levels will be present for `inner_var` (both in
  `df`/`x` and in `.df_row`) when calling `detail_fun`. If `TRUE`, all
  levels (those present on the factor `.df_row[[inner_var]]`,
  \*regardless if the level is observed in the row group or not) will be
  present when creating detail rows. Otherwise (the default), only the
  levels observed *anywhere in the row group, i.e., within `.df_row`*
  will be present.

- grp_fun:

  (`function`)  
  analysis function to be used when generating the "group summary" outer
  rows.

- detail_fun:

  (`function`)  
  analysis function to be used when generating "detail" inner rows.

- .alt_df_full:

  (`dataframe`)  
  denominator dataset for fraction and relative risk calculations.  
  this argument gets populated by the rtables split machinery (see
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html)).

- ...:

  additional arguments passed directly to `grp_fun` and `detail_fun`.

## Value

A `RowsVerticalSection` object including both the group row and all
detail rows, if applicable, for the facet.

## Details

Both the analysis variable and `inner_var` must be factors. Detail rows
are differentiated by having an indent mod of one, causing them to hang
indented under their corresponding group row.

## Note

In its current form, this function may not retain any formatting or
labeling instructions added by `grp_fun` or `detail_fun`, and it will
override any `.indent_mods` values specified by them. This behavior may
change in future versions.

## Author

GB, WW.

## Examples

``` r
# Example 1

lyt_obs_levels <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("EOSSTT", child_labels = "hidden") |>
  analyze("EOSSTT",
    afun = a_two_tier,
    extra_args = list(
      grp_fun = simple_analysis,
      detail_fun = simple_analysis,
      inner_var = "DCSREAS",
      drill_down_levs = "DISCONTINUED"
    )
  )

tbl <- build_table(lyt_obs_levels, ex_adsl)
tbl
#>                                   A: Drug X   B: Placebo   C: Combination
#> —————————————————————————————————————————————————————————————————————————
#> COMPLETED                            69           69             72      
#> DISCONTINUED                         38           43             39      
#>   ADVERSE EVENT                       6           6              7       
#>   LACK OF EFFICACY                   11           10             6       
#>   PHYSICIAN DECISION                  3           8              6       
#>   PROTOCOL VIOLATION                  6           9              6       
#>   WITHDRAWAL BY PARENT/GUARDIAN       8           2              4       
#>   WITHDRAWAL BY SUBJECT               4           8              10      
#> ONGOING                              27           22             21      

lyt_all_levels <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("EOSSTT", child_labels = "hidden") |>
  analyze("EOSSTT",
    afun = a_two_tier,
    extra_args = list(
      grp_fun = simple_analysis,
      detail_fun = simple_analysis,
      inner_var = "DCSREAS",
      drill_down_levs = "DISCONTINUED",
      use_all_levels = TRUE
    )
  )

adsl_subset <- subset(ex_adsl, DCSREAS != "ADVERSE EVENT")
levels(adsl_subset$DCSREAS)
#> [1] "ADVERSE EVENT"                 "LACK OF EFFICACY"             
#> [3] "PHYSICIAN DECISION"            "PROTOCOL VIOLATION"           
#> [5] "WITHDRAWAL BY PARENT/GUARDIAN" "WITHDRAWAL BY SUBJECT"        

tbl_all_levels <- build_table(lyt_all_levels, adsl_subset)
tbl_all_levels
#>                                   A: Drug X   B: Placebo   C: Combination
#> —————————————————————————————————————————————————————————————————————————
#> COMPLETED                             0           0              0       
#> DISCONTINUED                         32           37             32      
#>   ADVERSE EVENT                       0           0              0       
#>   LACK OF EFFICACY                   11           10             6       
#>   PHYSICIAN DECISION                  3           8              6       
#>   PROTOCOL VIOLATION                  6           9              6       
#>   WITHDRAWAL BY PARENT/GUARDIAN       8           2              4       
#>   WITHDRAWAL BY SUBJECT               4           8              10      
#> ONGOING                               0           0              0       

tbl_obs_levels <- build_table(lyt_obs_levels, adsl_subset)
tbl_obs_levels
#>                                   A: Drug X   B: Placebo   C: Combination
#> —————————————————————————————————————————————————————————————————————————
#> COMPLETED                             0           0              0       
#> DISCONTINUED                         32           37             32      
#>   LACK OF EFFICACY                   11           10             6       
#>   PHYSICIAN DECISION                  3           8              6       
#>   PROTOCOL VIOLATION                  6           9              6       
#>   WITHDRAWAL BY PARENT/GUARDIAN       8           2              4       
#>   WITHDRAWAL BY SUBJECT               4           8              10      
#> ONGOING                               0           0              0       

# Example 2

library(dplyr)

trtvar <- "ARM"
ctrl_grp <- "B: Placebo"

adsl <- ex_adsl |> select(c("USUBJID", "STRATA1", "EOSSTT", "DCSREAS", all_of(trtvar)))
adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)
adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

colspan_trt_map <- create_colspan_map(
  df = adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

a_freq_j_args <- list(
  .stats = "count_unique_fraction",
  denom = "n_altdf",
  ref_path = c("colspan_trt", " ", trtvar, ctrl_grp)
)

two_tier_args <- list(
  grp_fun = a_freq_j,
  detail_fun = a_freq_j,
  inner_var = "DCSREAS",
  drill_down_levs = "DISCONTINUED"
)

lyt_rrisk <- basic_table() |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
  split_cols_by(trtvar) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) |>
  split_rows_by("STRATA1") |>
  split_rows_by("EOSSTT", child_labels = "hidden") |>
  analyze("EOSSTT", afun = a_two_tier, extra_args = c(two_tier_args, a_freq_j_args))

adsl_subset <- subset(
  adsl,
  EOSSTT != "COMPLETED" & (is.na(DCSREAS) | DCSREAS != "PROTOCOL VIOLATION")
)

tbl_rrisk <- build_table(lyt_rrisk, adsl_subset, alt_counts_df = adsl_subset)
tbl_rrisk
#>                                         Active Study Agent                                  Risk Difference (%) (95% CI)             
#>                                     A: Drug X    C: Combination   B: Placebo   A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> A                                                                                                                                    
#>   COMPLETED                             0              0              0            0.0 (0.0, 0.0)               0.0 (0.0, 0.0)       
#>   DISCONTINUED                      9 (15.3%)      12 (22.2%)     10 (17.9%)     -2.6 (-16.2, 11.0)           4.4 (-10.6, 19.3)      
#>     ADVERSE EVENT                    3 (5.1%)       1 (1.9%)       3 (5.4%)       -0.3 (-8.4, 7.9)            -3.5 (-10.4, 3.4)      
#>     LACK OF EFFICACY                 4 (6.8%)       3 (5.6%)       1 (1.8%)       5.0 (-2.3, 12.3)             3.8 (-3.3, 10.8)      
#>     PHYSICIAN DECISION               1 (1.7%)       1 (1.9%)       3 (5.4%)       -3.7 (-10.4, 3.1)           -3.5 (-10.4, 3.4)      
#>     WITHDRAWAL BY PARENT/GUARDIAN    1 (1.7%)       2 (3.7%)       1 (1.8%)       -0.1 (-4.9, 4.7)             1.9 (-4.2, 8.0)       
#>     WITHDRAWAL BY SUBJECT               0           5 (9.3%)       2 (3.6%)       -3.6 (-8.4, 1.3)             5.7 (-3.4, 14.8)      
#>   ONGOING                            5 (8.5%)       5 (9.3%)      8 (14.3%)       -5.8 (-17.4, 5.8)           -5.0 (-17.0, 7.0)      
#> B                                                                                                                                    
#>   COMPLETED                             0              0              0            0.0 (0.0, 0.0)               0.0 (0.0, 0.0)       
#>   DISCONTINUED                      10 (16.9%)     10 (18.5%)     14 (25.0%)      -8.1 (-22.9, 6.8)           -6.5 (-21.8, 8.9)      
#>     ADVERSE EVENT                       0           4 (7.4%)       1 (1.8%)       -1.8 (-5.3, 1.7)             5.6 (-2.2, 13.4)      
#>     LACK OF EFFICACY                 5 (8.5%)       2 (3.7%)      7 (12.5%)       -4.0 (-15.2, 7.2)           -8.8 (-18.8, 1.2)      
#>     PHYSICIAN DECISION               1 (1.7%)       3 (5.6%)       2 (3.6%)       -1.9 (-7.7, 4.0)             2.0 (-5.8, 9.8)       
#>     WITHDRAWAL BY PARENT/GUARDIAN    4 (6.8%)          0           1 (1.8%)       5.0 (-2.3, 12.3)             -1.8 (-5.3, 1.7)      
#>     WITHDRAWAL BY SUBJECT               0           1 (1.9%)       3 (5.4%)       -5.4 (-11.3, 0.5)           -3.5 (-10.4, 3.4)      
#>   ONGOING                           12 (20.3%)     6 (11.1%)      8 (14.3%)       6.1 (-7.7, 19.8)            -3.2 (-15.6, 9.2)      
#> C                                                                                                                                    
#>   COMPLETED                             0              0              0            0.0 (0.0, 0.0)               0.0 (0.0, 0.0)       
#>   DISCONTINUED                      13 (22.0%)     11 (20.4%)     10 (17.9%)      4.2 (-10.4, 18.8)           2.5 (-12.2, 17.2)      
#>     ADVERSE EVENT                    3 (5.1%)       2 (3.7%)       2 (3.6%)        1.5 (-5.9, 8.9)             0.1 (-6.9, 7.1)       
#>     LACK OF EFFICACY                 2 (3.4%)       1 (1.9%)       2 (3.6%)       -0.2 (-6.9, 6.5)             -1.7 (-7.8, 4.3)      
#>     PHYSICIAN DECISION               1 (1.7%)       2 (3.7%)       3 (5.4%)       -3.7 (-10.4, 3.1)            -1.7 (-9.4, 6.1)      
#>     WITHDRAWAL BY PARENT/GUARDIAN    3 (5.1%)       2 (3.7%)          0           5.1 (-0.5, 10.7)             3.7 (-1.3, 8.7)       
#>     WITHDRAWAL BY SUBJECT            4 (6.8%)       4 (7.4%)       3 (5.4%)       1.4 (-7.3, 10.1)             2.1 (-7.1, 11.2)      
#>   ONGOING                           10 (16.9%)     10 (18.5%)     6 (10.7%)       6.2 (-6.3, 18.8)             7.8 (-5.3, 21.0)      
```
