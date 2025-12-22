# TableTree to .rtf Conversion

A function to convert TableTree to .rtf

## Usage

``` r
tt_to_tlgrtf(
  tt,
  file = NULL,
  orientation = c("portrait", "landscape"),
  colwidths = def_colwidths(tt, fontspec, col_gap = col_gap, label_width_ins =
    label_width_ins, type = tlgtype),
  label_width_ins = 2,
  watermark = NULL,
  pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
  fontspec = font_spec("Times", 9L, 1.2),
  pg_width = pg_width_by_orient(orientation == "landscape"),
  margins = c(0, 0, 0, 0),
  paginate = tlg_type(tt) == "Table",
  col_gap = ifelse(tlgtype == "Listing", 0.5, 3),
  nosplitin = list(row = character(), col = character()),
  verbose = FALSE,
  tlgtype = tlg_type(tt),
  string_map = default_str_map,
  markup_df = dps_markup_df,
  combined_rtf = FALSE,
  one_table = TRUE,
  border_mat = make_header_bordmat(obj = tt),
  round_type = obj_round_type(tt),
  alignments = list(),
  validate = TRUE,
  ...
)
```

## Arguments

- tt:

  (`TableTree`)  
  TableTree object to convert to RTF

- file:

  (`character(1)`)  
  File to create, including path, but excluding .rtf extension.

- orientation:

  (`character`)  
  Orientation of the output ("portrait" or "landscape")

- colwidths:

  (`numeric` vector)  
  Column widths for the table

- label_width_ins:

  (`numeric`)  
  Label width in inches

- watermark:

  (optional) String containing the desired watermark for RTF outputs.
  Vectorized.

- pagenum:

  (`logical`)  
  Whether to add page numbers to the output. Only applicable to listings
  (i.e. it is ignored for tables and figures).

- fontspec:

  (`font_spec`)  
  Font specification object

- pg_width:

  (`numeric`)  
  Page width in inches

- margins:

  (`numeric` vector)  
  Margins in inches (top, right, bottom, left)

- paginate:

  (`logical`)  
  Whether to paginate the output

- col_gap:

  (`numeric`)  
  Column gap in spaces

- nosplitin:

  (`list`)  
  list(row=, col=). Path elements whose children should not be paginated
  within if it can be avoided. e.g., list(col="TRT01A") means don't
  split within treatment arms unless all the associated columns don't
  fit on a single page.

- verbose:

  (`logical`)  
  Whether to print verbose output

- tlgtype:

  (`character`)  
  Type of the output (Table, Listing, or Figure)

- string_map:

  (`data.frame`)  
  Unicode mapping for special characters

- markup_df:

  (`data.frame`)  
  Data frame containing markup information

- combined_rtf:

  (`logical(1)`)  
  In the case where the result is broken up into multiple parts due to
  width, should a combined rtf file also be created. Defaults to
  `FALSE`.

- one_table:

  (`logical(1)`)  
  If `tt` is a (non-`MatrixPrintForm`) list, should the parts be added
  to the rtf within a single table (`TRUE`, the default) or as separate
  tables. End users will not generally need to set this.

- border_mat:

  (`matrix`)  
  A `m x k` matrix where m is the number of columns of `tt` and k is the
  number of lines the header takes up. See
  [tidytlg::add_bottom_borders](https://pharmaverse.github.io/tidytlg/main/reference/add_bottom_borders.html)
  for what the matrix should contain. Users should only specify this
  when the default behavior does not meet their needs.

- round_type:

  (`character(1)`)  
  the type of rounding to perform. See
  [`formatters::format_value()`](https://insightsengineering.github.io/formatters/latest-tag/reference/format_value.html)
  for more details.

- alignments:

  (`list`)  
  List of named lists. Vectorized. (Default =
  [`list()`](https://rdrr.io/r/base/list.html)) Used to specify
  individual column or cell alignments. Each named list contains `row`,
  `col`, and `value`, which are passed to
  [`huxtable::set_align()`](https://hughjonesd.github.io/huxtable/reference/align.html)
  to set the alignments.

- validate:

  logical(1). Whether to validate the table structure using
  [`rtables::validate_table_struct()`](https://insightsengineering.github.io/rtables/latest-tag/reference/validate_table_struct.html).
  Defaults to `TRUE`. If `FALSE`, a message will be displayed when
  validation fails.

- ...:

  Additional arguments passed to gentlg

## Value

If `file` is non-NULL, this is called for the side-effect of writing one
or more RTF files. Otherwise, returns a list of `huxtable` objects.

## Details

This function aids in converting the rtables TableTree into the desired
.rtf file.

## Note

`file` should always include path. Path will be extracted and passed
separately to `gentlg`.

When `one_table` is `FALSE`, only the width of the row label
pseudocolumn can be directly controlled due to a limitation in
[`tidytlg::gentlg`](https://pharmaverse.github.io/tidytlg/main/reference/gentlg.html).
The proportion of the full page that the first value in colwidths would
take up is preserved and all other columns equally split the remaining
available width. This will cause, e.g., the elements within the allparts
rtf generated when `combined_rtf` is `TRUE` to differ visually from the
content of the individual part rtfs.
