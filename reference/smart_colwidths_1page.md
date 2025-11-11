# Colwidths for all columns to be forced on one page

Colwidths for all columns to be forced on one page

## Usage

``` r
smart_colwidths_1page(
  tt,
  fontspec,
  col_gap = 6L,
  rowlabel_width = inches_to_spaces(2, fontspec),
  print_width_ins = ifelse(landscape, 11, 8.5) - 2.12,
  landscape = FALSE,
  lastcol_gap = TRUE
)
```

## Arguments

- tt:

  TableTree object to calculate column widths for

- fontspec:

  Font specification object

- col_gap:

  Column gap in spaces

- rowlabel_width:

  Width of row labels in spaces

- print_width_ins:

  Print width in inches

- landscape:

  Whether the output is in landscape orientation

- lastcol_gap:

  Whether to include a gap after the last column
