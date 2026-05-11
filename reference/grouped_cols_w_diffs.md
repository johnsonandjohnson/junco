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
  [`rtables::add_combo_levels()`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_level.html)
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

## See also

Other riskdiff_col_struct:
[`make_multicomp_splfun()`](https://johnsonandjohnson.github.io/junco/reference/make_multicomp_splfun.md)
