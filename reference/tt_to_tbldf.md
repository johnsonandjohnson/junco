# Create TableTree as DataFrame via gentlg

Create TableTree as DataFrame via gentlg

## Usage

``` r
tt_to_tbldf(
  tt,
  fontspec = font_spec("Times", 9L, 1),
  string_map = default_str_map,
  markup_df = dps_markup_df,
  round_type = obj_round_type(tt),
  validate = TRUE
)
```

## Arguments

- tt:

  (`TableTree`)  
  TableTree object to convert to a data frame

- fontspec:

  (`font_spec`)  
  Font specification object

- string_map:

  (`list`)  
  Unicode mapping for special characters

- markup_df:

  (`data.frame`)  
  Data frame containing markup information

- round_type:

  (`character(1)`)  
  the type of rounding to perform. See
  [`formatters::format_value()`](https://insightsengineering.github.io/formatters/latest-tag/reference/format_value.html)
  for more details.

- validate:

  logical(1). Whether to validate the table structure using
  [`rtables::validate_table_struct()`](https://insightsengineering.github.io/rtables/latest-tag/reference/validate_table_struct.html).
  Defaults to `TRUE`. If `FALSE`, a message will be displayed instead of
  stopping with an error when validation fails.

## Value

`tt` represented as a `tbl` data.frame suitable for passing to
[tidytlg::gentlg](https://pharmaverse.github.io/tidytlg/main/reference/gentlg.html)
via the `huxme` argument.
