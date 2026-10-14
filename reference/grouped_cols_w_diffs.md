# Standard Column Structure With Grouped Treatments and Difference Columns

Standard Column Structure With Grouped Treatments and Difference Columns

## Usage

``` r
grouped_cols_w_diffs(
  lyt,
  colspan_trt_map,
  combo_map_df = NULL,
  comp_map = NULL,
  diff_cols = TRUE,
  diffs_label = "Risk Differences",
  .main_pre = list(),
  .main_post = list(),
  .rr_pre = list(),
  .rr_post = list()
)

grouped_cols_w_subgrps(
  lyt,
  colspan_trt_map = NULL,
  combo_map_df = NULL,
  trtvar = names(colspan_trt_map)[2],
  subgrp_var = NULL,
  subgrp_lbl = subgrp_var,
  total_facet = TRUE,
  total_lbl = "Total",
  .pre = list(),
  .post = list()
)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`). The layout to modify. This should virtually
  always be the object returned by `basic_table`.

- colspan_trt_map:

  (`data.frame`). The spanning label map for the main columns, as given
  by `create_colspan_map`.

- combo_map_df:

  (`data.frame` or `NULL`). A combination data frame as defined by
  [`add_combo_levels()`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_level.html)
  with an additional `is_control` column indicating whether the virtual
  level will act as a reference (`TRUE`) or active (`FALSE`) group.

- comp_map:

  (`data.frame` or `NULL`). A data.frame with columns `"active"`,
  `"comparator"`, `"active_is_combo"` and `"comparator_is_combo"`, or
  `NULL` indicating the default comparison behavior (See Details).

- diff_cols:

  (`logical(1)`). Whether the risk difference column structure should be
  included (`TRUE`, the default) or not (`FALSE`).

- diffs_label:

  (`character(1)`). The spanning label for the risk difference section
  of columns

- .main_pre:

  (`list` of `function`s). Passed to
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
  as `pre` for treatment split in main structure.

- .main_post:

  (`list` of `function`s). Passed to
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
  as `post` for treatment split in main structure.

- .rr_pre:

  (`list` of `function`s). Passed to
  [`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md)
  as `.pre` for risk difference faceting.

- .rr_post:

  (`list` of `function`s). Passed to
  [`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md)
  as `.post` for risk difference faceting.

- trtvar:

  (`character(1)` or `NULL`)  
  the treatment variable to split by. Defaults to the treatment variable
  in `colspan_trt_map`.

- subgrp_var:

  (`character(1)` or `NULL`)  
  the name of the subgroup variable to split by within the `trtvar`
  split

- subgrp_lbl:

  (`character(1)` or `NULL`)  
  the spanning label to place over the subgroups, if different than
  `subgrp_var`

- total_facet:

  (`logical(1)`)  
  should an overall facet be prepended to the partition defined by
  `subgrp_var`? Defaults to `TRUE`.

- total_lbl:

  (`character(1)`)  
  Label to be used for the overall facet, if applicable. Defaults to
  `"Total"`; ignored if `total_facet` is `FALSE`.

- .pre:

  (`list` of `function`s)  
  Passed to
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
  as `pre` for treatment splitting.

- .post:

  (`list` of `function`s)  
  Passed to
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
  as `post` for treatment splitting after the standard column-structure
  processing.

## Value

`lyt` updated with the specified main and risk difference column
structures added

## Details

This function combines multiple `rtables` column splitting instructions
with customized split functions to create a column structure with
treatment columns for each treatment arm (optionally including
combination arms), grouped by active and non-active, with risk
difference columns comparing active arm(s) against one or more
non-active controls. It is intended for use in layouts that will use
[`a_freq_j()`](https://johnsonandjohnson.github.io/junco/reference/a_freq_j.md)
or similar `junco`-style analysis functions which support risk
difference columns and accept a `ref_path` argument.

It is equivalent to the following sequence of layout instructions:

1.  splitting on a colspan labeling variable with
    [`rtables::trim_levels_to_map()`](https://insightsengineering.github.io/rtables/latest-tag/reference/trim_levels_to_map.html)
    as the split function;

2.  splitting on treatment;

3.  adding a (non-nested) overall column acting as the risk difference
    spanning label; and finally

4.  splitting on treatment using
    [`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md)
    as the split function

In addition, it supports:

- comparison against multiple control groups (as specified by
  `colspan_trt_map` and/or `comp_map`),

- virtual combination-levels as active an/or control "treatments" (via
  `combo_map_df`),

- full control of which comparisons are performed, and their order (via
  `comp_map`).

If combination levels are declared via `combo_map_df` but none appear in
`colspan_trt_map`, all combinations will be added to the appropriate
group within the map based on `combo_map_df$is_control` (assumed to be
`FALSE` if the column is missing), with a warning.

If some combination levels *do* appear in `combo_map_df` but others do
not, a warning will be thrown but the missing combination levels will
*not* be added to the treatment map.

By default (when `comp_map` is `NULL`), all active treatments, including
active combinations, will be compared against all control groups.

The risk difference section of the structure is declared using
[`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md).
Reference paths are inferred automatically from `colspan_trt_map` (after
combination levels have been added if necessary).

For the purposes of pathin in the resulting structure, `diffs_label`
will be both the split name and split value of the parent containing the
individual risk difference columns.

`grouped_cols_w_subgrps` creates a hierarchical column structure that
splits by `trtvar`, underneath which is a spanning label over a split
with a Total column along with columns for each level of `subgrp_var`.

## See also

Other std_col_struct:
[`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md),
[`shift_tbl_col_struct()`](https://johnsonandjohnson.github.io/junco/reference/shift_tbl_col_struct.md)

## Examples

``` r
colspan_var <- create_colspan_var(
  data.frame(TRT01A = factor(c("Placebo", "Active 1", "Active 2"))),
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = "Control",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)
colspan_trt_map <- create_colspan_map(
  colspan_var,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = "Control",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)

lyt <- basic_table() |>
  grouped_cols_w_diffs(colspan_trt_map) |>
  analyze("TRT01A", afun = function(x, ...) length(x))

build_table(lyt, colspan_var)
#>           Active Treatment     Control               Risk Differences             
#>          Active 1   Active 2   Placebo   Active 1 vs Placebo   Active 2 vs Placebo
#> ——————————————————————————————————————————————————————————————————————————————————
#> TRT01A      1          1          1               1                     1         
dat <- create_colspan_var(
  data.frame(
    TRT01A = factor(rep(c("Placebo", "Active 1"), each = 2)),
    SEX = factor(rep(c("Female", "Male"), 2))
  ),
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = "Control",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)
colspan_trt_map <- create_colspan_map(
  dat,
  non_active_grp = "Placebo",
  non_active_grp_span_lbl = "Control",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)

lyt <- basic_table() |>
  grouped_cols_w_subgrps(
    colspan_trt_map,
    subgrp_var = "SEX",
    subgrp_lbl = "SUB_*"
  ) |>
  analyze("TRT01A", afun = function(x, ...) length(x))

build_table(lyt, dat)
#>             Active Treatment             Control       
#>                 Active 1                 Placebo       
#>                  (N=2)                    (N=2)        
#>                  SUB_*                    SUB_*        
#>          Total    Female   Male   Total   Female   Male
#> ———————————————————————————————————————————————————————
#> TRT01A     2        1       1       2       1       1  
```
