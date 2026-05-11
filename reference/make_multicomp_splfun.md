# Make Multi-comparator Split Function

Create a custom splitting function suitable for creating risk difference
columns against one or more comparators. This is used within
`col_struct_w_risk_diffs`.

## Usage

``` r
make_multicomp_splfun(
  colspan_trt_map,
  combo_levels_map = NULL,
  comp_level_map = NULL,
  .pre = list(),
  .post = list()
)

make_dflt_comp_map(df, spl_var, ref_lvls, combo_map)
```

## Arguments

- colspan_trt_map:

  (`data.frame`)  
  A data.frame defining the active and non-active groups of treatment
  arms, including combination arms defined in `combo_levels_map`, as
  returned by
  [`create_colspan_map()`](https://johnsonandjohnson.github.io/junco/reference/colspan_map.md).

- combo_levels_map:

  (`data.frame` or `NULL`)  
  NULL (the default) or a data.frame indicating combination levels to
  added to some or all blocks of comparisons. See Details.

- comp_level_map:

  (`data.frame` or `NULL`)  
  A data.frame with columns `active` and `comparator` indivating which
  risk difference comparisons to include in the column structure, or
  `NULL` (the default), indicating all active vs non-active pairwise
  comparisons as defined `colspan_trt_map` treatment groupings.

- .pre:

  `(list)`  
  A list of additional preprocessing functions to be provided to
  `make_split_fun`. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

- .post:

  `(list)`  
  A list of additional post-processing functions to be provided to
  `make_split_fun` *after* those which provide this function's primary
  multi-comparator functionality. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html)

- df:

  (`data.frame`)  
  Data used to derive the available levels for the splitting variable
  `spl_var` and to construct the default comparison map when
  `comp_level_map` is not provided.

- spl_var:

  (`character(1)`)  
  Name of the splitting variable (typically the treatment variable)
  whose levels define active and comparator groups.

- ref_lvls:

  (`character`)  
  The level names to be treated as reference (control) groups.
  Comparisons will be formed against these levels.

- combo_map:

  (`data.frame`)  
  A combination levels map (i.e., the value passed to
  `combo_levels_map`) used to include virtual combination levels in the
  default comparison map.

## Value

A split function suitable for use in both `split_rows_by` and
`split_cols_by`.

For `make_dflt_comp_map()`, a `data.frame` with columns `active`,
`comparator`, `active_is_combo`, and `comparator_is_combo`, listing the
comparisons to keep and whether either side is a virtual combination
level.

## Details

This split function is intended to create a set of risk difference or
similar columns. As such it will automatically exclude the facet for
each comparator level (e.g., Placebo vs Placebo) as determined by the
last element of each element of `comp_level_paths`.

Further control of facets is provided by `comp_level_map`. If `NULL`
(the default), all non-control/reference groups will be compared
pairwise with all control/reference groups as defined by the grouping in
`colspan_trt_map`.

If specified, `comp_level_map` must be a `data.frame` (including
`tbl_df`) with three columns:

- `active` - (character) the value to be compared to a reference level,

- `comparator` - (character) the level that should be compared against,
  and

- `active_is_combo` - (logical) is the level specified in `active` a
  virtual combination level.

- `comparator_is_combo` - (logical) is the level specified in
  `comparator` a virtual combination level.

If a `data.frame` with only the `active` and `comparator` columns is
given for `comp_level_map`, `active_is_combo` and `comparator_is_combo`
are inferred from `colspan_trt_map`.

If any rows of `comp_level_map` have `active_is_combo == TRUE` or
`comparator_is_combo`, the relevant values in those rows *must* also
appear in `combo_levels_map` with the correct level for comp_level (or
the `select_all_levels` sentinel value which indicates inclusion for all
comparators).

If specified, `combo_levels_map` must be a `data.frame` (including
`tbl_df`) with the following columns:

- `valname` - (`character`) The name(s) for the combination level(s),

- `label` - (`character`) the label(s) for the combination level(s),

- `levelcombo` - (`list` of `character`) the levels of the split
  variable to be combined, or `select_all_levels` for all levels,

- `exargs` - (`list`) the extra_args values for each combo level. If not
  present this will be assumed to be
  [`list()`](https://rdrr.io/r/base/list.html) for all combo levels.

- `compare_against` - (`list` of `character`) Optional. The reference
  level(s) the combo level should be compared against, or
  `select_all_levels` for inclusion against all comparators.

- `is_control` - (`logical`) Optional. Is this combination level going
  to be used as a reference level (must appear as the last element in
  one of `comp_level_paths` if so).

When specifying `combo_levels_map` if the `compare_against` column is
omitted, comparison against all reference levels will be performed for
all combination levels. If `is_control` is omitted, it will be assumed
as `FALSE` for all combination levels.

Order of combination levels when multiple are present for a single
comparator, as well as their position relative to non-combination
comparisons, is determined by row order in `combo_levels_map`.

Labels and names of comparison columns involving combination levels will
be automatically computed in the form of
`"<combo level name/label> vs <ref group name>"`. Note currently ref
group *name* is always used as it needs to be inferable from
`colspan_trt_map`.

The comparator reference path is calculated based on `colspan_trt_map`
and then added as `ref_path` to the extra_args associated with generated
facet. As such, analysis (or content) functions used underneath a split
using the generated split function must accept either `ref_path` or
`...`.

## Note

It is not currently possible to use a virtual combination level as a
comparator/reference group. If you need this functionality please
contact the maintainers by filing an issue at
https://github.com/johnsonandjohnson/junco/issues

## See also

Other riskdiff_col_struct:
[`grouped_cols_w_diffs()`](https://johnsonandjohnson.github.io/junco/reference/grouped_cols_w_diffs.md)
