# Content Row Function for Counts of Subgroups in Response Tables (RESP01)

Content Row Function for Counts of Subgroups in Response Tables (RESP01)

## Usage

``` r
resp01_counts_cfun(df, labelstr, .spl_context, .alt_df, label_fstr)
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

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- .alt_df:

  (`data.frame`)  
  alternative data frame used for denominator calculation.

- label_fstr:

  (`string`)  
  format string for the label.

## Value

The correct
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
result.

## Examples

``` r
fake_spl_context <- data.frame(
  cur_col_split_val = I(list(c(ARM = "A: Drug X", count_prop = "count_prop")))
)
resp01_counts_cfun(
  df = DM,
  labelstr = "Blue",
  .spl_context = fake_spl_context,
  .alt_df = DM,
  label_fstr = "Color: %s"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>      row_name formatted_cell indent_mod   row_label
#> 1 Color: Blue            356          0 Color: Blue
```
