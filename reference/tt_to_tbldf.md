# Create TableTree as DataFrame via gentlg

Create TableTree as DataFrame via gentlg

## Usage

``` r
tt_to_tbldf(
  tt,
  fontspec = font_spec("Times", 9L, 1),
  string_map = default_str_map,
  markup_df = dps_markup_df
)
```

## Arguments

- tt:

  TableTree object to convert to a data frame

- fontspec:

  Font specification object

- string_map:

  Unicode mapping for special characters

- markup_df:

  Data frame containing markup information

## Value

`tt` represented as a "tbl" data.frame suitable for passing to
[tidytlg::gentlg](https://pharmaverse.github.io/tidytlg/main/reference/gentlg.html)
via the `huxme` argument.
