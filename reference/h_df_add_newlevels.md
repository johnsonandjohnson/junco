# Add New Levels to Data Frame

Adds new factor levels to a specified variable in the data frame.

## Usage

``` r
h_df_add_newlevels(df, .var, new_levels, addstr2levs = NULL, new_levels_after)
```

## Arguments

- df:

  Data frame to update.

- .var:

  Variable to which new levels will be added.

- new_levels:

  List of new levels to add.

- addstr2levs:

  String to add to new levels.

- new_levels_after:

  Boolean, indicating if new levels should be added after existing
  levels.

## Value

Updated data frame.
