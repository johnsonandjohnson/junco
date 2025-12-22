# Colwidths for all columns to be forced on one page.

Colwidths for all columns to be forced on one page.

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

  (`TableTree`)  
  TableTree object to calculate column widths for

- fontspec:

  (`font_spec`)  
  Font specification object

- col_gap:

  (`numeric`)  
  Column gap in spaces

- rowlabel_width:

  (`numeric`)  
  Width of row labels in spaces

- print_width_ins:

  (`numeric`)  
  Print width in inches

- landscape:

  (`logical`)  
  Whether the output is in landscape orientation

- lastcol_gap:

  (`logical`)  
  Whether to include a gap after the last column
