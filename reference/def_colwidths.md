# Define Column Widths

`def_colwidths` uses heuristics to determine suitable column widths
given a table or listing, and a font.

## Usage

``` r
listing_column_widths(
  mpf,
  incl_header = TRUE,
  col_gap = 0.5,
  pg_width_ins = 8.88,
  fontspec = font_spec("Times", 8, 1.2),
  verbose = FALSE
)

def_colwidths(
  tt,
  fontspec,
  label_width_ins = 2,
  col_gap = ifelse(type == "Listing", 0.5, 3),
  type = tlg_type(tt)
)
```

## Arguments

- mpf:

  (`listing_df` or `MatrixPrintForm` derived thereof)  
  The listing calculate column widths for.

- incl_header:

  (`logical(1)`)  
  Should the constraint to not break up individual words be extended to
  words in the column labels? Defaults to `TRUE`

- col_gap:

  Column gap in spaces. Defaults to `.5` for listings and `3` for
  tables.

- pg_width_ins:

  (`numeric(1)`)  
  Number of inches in width for *the portion of the page the listing
  will be printed to*. Defaults to `8.88` which corresponds to landscape
  orientation on a standard page after margins.

- fontspec:

  Font specification

- verbose:

  (`logical(1)`)  
  Should additional information messages be displayed during the
  calculation of the column widths? Defaults to `FALSE`.

- tt:

  input Tabletree

- label_width_ins:

  Label Width in Inches.

- type:

  Type of the table tree, used to determine column width calculation
  method.

## Value

A vector of column widths suitable to use in `tt_to_tlgrtf` and other
exporters.

a vector of column widths (including the label row pseudo-column in the
table case) suitable for use rendering `tt` in the specified font.

## Details

Listings are assumed to be rendered landscape on standard A1 paper, such
that all columns are rendered on one page. Tables are allowed to be
horizontally paginated, and column widths are determined based only on
required word wrapping. See the `Automatic Column Widths` vignette for a
detailed discussion of the algorithms used.
