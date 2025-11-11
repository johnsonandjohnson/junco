# Formatted Analysis Function for Proportion Analysis (TEFCGIS08 e.g.)

This function applies to a factor `x` when a column split was prepared
with
[`prop_split_fun()`](https://johnsonandjohnson.github.io/junco/reference/prop_post_fun.md)
before.

## Usage

``` r
prop_table_afun(x, .spl_context, formats, add_total_level = FALSE)
```

## Arguments

- x:

  (`factor`)  
  factor variable to analyze.

- .spl_context:

  (`environment`)  
  split context environment.

- formats:

  (`list`)  
  formats for the statistics.

- add_total_level:

  (`flag`)  
  whether to add a total level.

## Value

A `VerticalRowsSection` as returned by
[rtables::in_rows](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html).

## Details

In the column named `n`, the counts of the categories as well as an
optional `Total` count will be shown. In the column named `percent`, the
percentages of the categories will be shown, with an optional blank
entry for `Total`. In the column named `cum_percent`, the cumulative
percentages will be shown instead.
