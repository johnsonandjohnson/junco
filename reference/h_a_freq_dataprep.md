# A Frequency Data Preparation Function

Prepares frequency data for analysis.

## Usage

``` r
h_a_freq_dataprep(
  df,
  labelstr = NULL,
  .var = NA,
  val = NULL,
  drop_levels = FALSE,
  excl_levels = NULL,
  new_levels = NULL,
  new_levels_after = FALSE,
  addstr2levs = NULL,
  .df_row,
  .spl_context,
  .N_col,
  id = "USUBJID",
  denom = c("N_col", "n_df", "n_altdf", "N_colgroup", "n_rowdf", "n_parentdf"),
  variables,
  label = NULL,
  label_fstr = NULL,
  label_map = NULL,
  .alt_df_full = NULL,
  denom_by = NULL,
  .stats
)
```

## Arguments

- df:

  Data frame to prepare.

- labelstr:

  Label string.

- .var:

  Variable name.

- val:

  Values for analysis.

- drop_levels:

  Boolean, indicating if levels should be dropped.

- excl_levels:

  Levels to exclude.

- new_levels:

  New levels to add.

- new_levels_after:

  Boolean for adding new levels after existing ones.

- addstr2levs:

  String to add to new levels.

- .df_row:

  Current data frame row.

- .spl_context:

  Current split context.

- .N_col:

  Number of columns.

- id:

  Identifier variable.

- denom:

  Denominator types.

- variables:

  Variables to include in the analysis.

- label:

  Label string.

- label_fstr:

  Formatted label string.

- label_map:

  Mapping for labels.

- .alt_df_full:

  Alternative full data frame.

- denom_by:

  Denominator grouping variable.

- .stats:

  Statistics to compute.

## Value

List containing prepared data frames and values.
