# Update Data Frame Row

Updates a row in the data frame based on various parameters.

## Usage

``` r
h_upd_dfrow(
  df_row,
  .var,
  val,
  excl_levels,
  drop_levels,
  new_levels,
  new_levels_after,
  addstr2levs,
  label,
  label_map,
  labelstr,
  label_fstr,
  .spl_context
)
```

## Arguments

- df_row:

  Data frame row to update.

- .var:

  Variable name to update.

- val:

  Values to keep.

- excl_levels:

  Levels to exclude from the factor.

- drop_levels:

  Boolean, indicating if levels should be dropped.

- new_levels:

  New levels to add.

- new_levels_after:

  Boolean, indicating if new levels should be added after existing
  levels.

- addstr2levs:

  String to add to new levels.

- label:

  Label string.

- label_map:

  Mapping for labels.

- labelstr:

  Label string to replace.

- label_fstr:

  Format string for labels.

- .spl_context:

  Current split context.

## Value

List containing updated data frames and values.
