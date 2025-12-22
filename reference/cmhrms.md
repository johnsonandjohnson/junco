# Cochran-Mantel-Haenszel Row Mean Scores test

See <https://psiaims.github.io/CAMIS/Comp/r-sas_cmh.html> for a general
comparison overview between R and SAS.

## Usage

``` r
a_cmhrms_j(
  df,
  .var,
  ref_path,
  .spl_context,
  .ref_group,
  .in_ref_col,
  .df_row,
  ...,
  variables,
  collapse_combo = TRUE,
  .stats = NULL,
  .formats = NULL,
  .indent_mods = NULL,
  .labels = NULL
)

s_cmhrms_j(
  df,
  .var,
  .ref_group,
  .in_ref_col,
  ...,
  .df_row,
  variables,
  collapse_combo = FALSE
)

a_cmhrms_j_with_exclude(
  df,
  exclude_levels,
  .var,
  .spl_context,
  .ref_group,
  .in_ref_col,
  .df_row,
  ...,
  .stats = NULL,
  .formats = NULL,
  .indent_mods = NULL,
  .labels = NULL
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- ref_path:

  (`character`)  
  global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- .ref_group:

  (`data.frame` or `vector`)  
  the data corresponding to the reference group.

- .in_ref_col:

  (`logical`)  
  `TRUE` when working with the reference level, `FALSE` otherwise.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- ...:

  additional arguments for the lower level functions.

- variables:

  (`list`)  
  list with arm and strata variable names.

- collapse_combo:

  (`logical`)  
  If TRUE, multiple arm levels from df will be combined into 1 level.

- .stats:

  (`character`)  
  statistics to select for the table.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- exclude_levels:

  (`list`)  
  A named list where names correspond to split variables and values are
  vectors of levels to exclude.

## Value

- `s_cmhrms_j` a single element list containing the p-value from row
  mean score test.

- `a_cmhrms_j` a `VerticalRowsSection` object (single row).

## Functions

- `a_cmhrms_j()`: Formatted analysis function which is used as `afun`.

- `s_cmhrms_j()`: Statistics function for the calculation of the p-value
  based upon the row mean scores test.

- `a_cmhrms_j_with_exclude()`: Wrapper for the `afun` which can exclude
  row split levels from producing the analysis. These have to be
  specified in the `exclude_levels` argument, see
  [`?do_exclude_split`](https://johnsonandjohnson.github.io/junco/reference/do_exclude_split.md)
  for details.
