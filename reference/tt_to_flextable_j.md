# Convert a TableTree or a listing_df object to a flextable

This function is based on
[`rtables.officer::tt_to_flextable()`](https://insightsengineering.github.io/rtables.officer/latest-tag/reference/tt_to_flextable.html).

## Usage

``` r
tt_to_flextable_j(
  tt,
  tblid = NULL,
  theme = theme_docx_default_j(font = "Times New Roman", font_size = 9L, bold = NULL),
  border = flextable::fp_border_default(width = 0.875, color = "black"),
  titles_as_header = TRUE,
  bold_titles = TRUE,
  integrate_footers = TRUE,
  counts_in_newline = FALSE,
  paginate = tlg_type(tt) == "Table",
  fontspec = formatters::font_spec("Times", 9L, 1.2),
  colwidths = NULL,
  label_width_ins = 2,
  total_page_width = pg_width_by_orient(orientation == "landscape"),
  orientation = "portrait",
  nosplitin = list(row = character(), col = character()),
  string_map = default_str_map,
  markup_df_docx = dps_markup_df_docx,
  reduce_first_col_indentation = FALSE,
  tlgtype = tlg_type(tt),
  col_gap = ifelse(tlgtype == "Listing", 0.5, 3),
  round_type = formatters::obj_round_type(tt),
  alignments = list(),
  border_mat = make_header_bordmat(obj = tt),
  validate = TRUE,
  ...
)
```

## Arguments

- tt:

  (`TableTree` or `listing_df`)  
  the object to convert to flextable.

- tblid:

  (`character`)  
  output ID to be displayed in the title and last line of footer. When
  exporting, it will also be used as the output filename.  
  If NULL, a temp file will be created, its dirname will replace
  argument `output_dir`, and its basename will replace argument
  `tblid`.  
  (optional) Default = NULL.

- theme:

  (function factory)  
  the theme to apply to the flextable.  
  (optional) Default =
  [`theme_docx_default_j()`](https://johnsonandjohnson.github.io/junco/reference/theme_docx_default_j.md).  
  See
  [`theme_docx_default_j()`](https://johnsonandjohnson.github.io/junco/reference/theme_docx_default_j.md)
  or
  [`rtables.officer::theme_docx_default()`](https://insightsengineering.github.io/rtables.officer/latest-tag/reference/tt_to_flextable.html)
  for more details.

- border:

  (`fp_border`)  
  border to use.  
  Default =
  `flextable::fp_border_default(width = 0.875, color = "black")`.

- titles_as_header:

  (`logical`)  
  (optional) Default = TRUE.

- bold_titles:

  (`logical`)  
  (optional) Default = TRUE.

- integrate_footers:

  (`logical`)  
  (optional) Default = TRUE.

- counts_in_newline:

  (`logical`)  
  (optional) Default = FALSE.

- paginate:

  (`logical`)  
  (optional) Default = TRUE for TableTree and FALSE otherwise.

- fontspec:

  (`font_spec`)  
  font specification object.

- colwidths:

  (`numeric`)  
  column widths for the table.  
  (optional) Default = NULL.

- label_width_ins:

  (`numeric`)  
  label width in inches.  
  (optional) Default = 2.

- total_page_width:

  (`numeric`)  
  no need to be specified by end user.  
  (optional) Default = 6.38 ("portrait") or 8.88 ("landscape").

- orientation:

  (`character`)  
  one of: "portrait", "landscape".  
  (optional) Default = "portrait".

- nosplitin:

  (`list`)  
  path elements whose children should not be paginated within if it can
  be avoided. The list should have the format list(row=, col=).  
  E.g., list(col="TRT01A") means don't split within treatment arms
  unless all the associated columns don't fit on a single page.  
  (optional) Default = list(row = character(), col = character()).

- string_map:

  (`tibble`)  
  (optional) Default = default_str_map.

- markup_df_docx:

  (`tibble`)  
  (optional) Default = dps_markup_df_docx.

- reduce_first_col_indentation:

  (`logical`)  
  whether to reduce by 1 the indentation if we have vertical pagination.
  No need to be specified by the end user.  
  (optional) Default = FALSE.

- tlgtype:

  (`character`)  
  (optional) No need to be specified by end user.

- col_gap:

  (`numeric`)  
  (optional) Default = 3 (Tables) or 0.5 (Listings).

- round_type:

  (`"iec"` or `"sas"`)  
  the type of rounding to perform. iec, the default, performs rounding
  compliant with IEC 60559, while sas performs nearest-value rounding
  consistent with rounding within SAS. See
  `[formatters::format_value()]` for more details.

- alignments:

  (`list`)  
  list of named lists. Vectorized. Used to specify individual column or
  cell alignments. Each named list contains `row`, `col`, and `value`.  
  (optional) Default = [`list()`](https://rdrr.io/r/base/list.html).

- border_mat:

  (`matrix`)  
  a `m x k` matrix where m is the number of columns of the input
  Table/Listing and k is the number of lines the header takes up.  
  See
  [tidytlg::add_bottom_borders](https://pharmaverse.github.io/tidytlg/main/reference/add_bottom_borders.html)
  for what the matrix should contain. Users should only specify this
  when the default behavior does not meet their needs.

- validate:

  (`logical(1)`)  
  Whether to validate the table structure using
  [`rtables::validate_table_struct()`](https://insightsengineering.github.io/rtables/latest-tag/reference/validate_table_struct.html).
  Defaults to `TRUE`. If `FALSE`, a message will be displayed when
  validation fails.

- ...:

  other parameters.

## Value

a flextable object.

## Note

The following features are not implemented in `flextable`, and as a
result they will only be visible when exporting to docx using
[`export_TLG_as_docx()`](https://johnsonandjohnson.github.io/junco/reference/export_TLG_as_docx.md):

- watermark

- hanging indents

- caption style

- page numbering

## Note

This function may be removed from junco in the future if the
functionality is merged into `rtables.officer`.

For more information, refer to the vignette
`table_and_listing_customizations` (`browseVignettes("junco")`)
