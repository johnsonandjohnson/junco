# Export a TLG (Table, Listing, Graph) to .docx format

Export a TLG (Table, Listing, Graph) to .docx format

## Usage

``` r
export_TLG_as_docx(
  obj = NULL,
  tblid = NULL,
  output_dir = NULL,
  theme = theme_docx_default_j(font = "Times New Roman", font_size = 9L, bold = NULL),
  add_page_break = FALSE,
  titles_as_header = TRUE,
  integrate_footers = TRUE,
  section_properties = officer::prop_section(page_size = officer::page_size(width = 11,
    height = 8.5, orient = orientation), page_margins = officer::page_mar(bottom = 1, top
    = 1, right = 1, left = 1, gutter = 0, footer = 1, header = 1)),
  doc_metadata = NULL,
  template_file = NULL,
  orientation = "portrait",
  paginate = tlgtype == "Table",
  nosplitin = list(row = character(), col = character()),
  string_map = default_str_map,
  markup_df_docx = dps_markup_df_docx,
  combined_docx = FALSE,
  tlgtype = ifelse(is.null(obj), "Figure", tlg_type(obj)),
  col_gap = ifelse(tlgtype == "Listing", 0.5, 3),
  pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
  round_type = ifelse(tlgtype %in% c("Table", "Listing"),
    formatters::obj_round_type(obj), "iec"),
  alignments = list(),
  border = flextable::fp_border_default(width = 0.875, color = "black"),
  border_mat = NULL,
  export_csv = FALSE,
  output_csv_directory = NULL,
  markup_df = dps_markup_df,
  validate = TRUE,
  watermark = NULL,
  plotnames = NULL,
  title = NULL,
  footers = NULL,
  plotwidth = 8,
  plotheight = 5.51,
  units = c("in", "cm", "mm", "px")[1],
  ...
)
```

## Arguments

- obj:

  (`TableTree`, `listing_df` or `ggplot2`)  
  the object to export.

- tblid:

  (`character`)  
  output ID to be displayed in the title and last line of footer. When
  exporting, it will also be used as the output filename.  
  If NULL, a temp file will be created, its dirname will replace
  argument `output_dir`, and its basename will replace argument
  `tblid`.  
  (optional) Default = NULL.

- output_dir:

  (`character`)  
  a directory path to save the docx.  
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

- add_page_break:

  (`logical`)  
  (optional) Default = FALSE.

- titles_as_header:

  (`logical`)  
  (optional) Default = TRUE.

- integrate_footers:

  (`logical`)  
  (optional) Default = TRUE.

- section_properties:

  (`prop_section`)  
  (optional) A "prop_section" object containing information about page
  size, orientation, margins, etc. See
  [`officer::prop_section()`](https://davidgohel.github.io/officer/reference/prop_section.html)
  for more details. No need to be specified by end user.

- doc_metadata:

  (list of `string`)  
  any value that can be used as metadata by
  [`officer::set_doc_properties()`](https://davidgohel.github.io/officer/reference/set_doc_properties.html).
  Important text values are title, subject, creator, and description,
  while created is a date object.  
  (optional) Default = NULL.

- template_file:

  (`character`)  
  Template file that `officer` will use as a starting point for the
  final document. Document attaches the table and uses the defaults
  defined in the template file. Paragraph styles are inherited from this
  file.  
  If NULL, this function will use an internal template.  
  (optional) Default = NULL.

- orientation:

  (`character`)  
  one of: "portrait", "landscape".  
  (optional) Default = "portrait".

- paginate:

  (`logical`)  
  (optional) Default = TRUE for TableTree and FALSE otherwise.

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

- combined_docx:

  (`logical`)  
  whether to also export an "allparts" docx version. Only applies when
  exporting a Table or Listing.  
  (optional) Default = FALSE.

- tlgtype:

  (`character`)  
  (optional) No need to be specified by end user.

- col_gap:

  (`numeric`)  
  (optional) Default = 3 (Tables) or 0.5 (Listings).

- pagenum:

  (`logical`)  
  whether to display page numbers. Only applicable to listings (i.e. for
  tables and figures this argument is ignored).  
  (optional) Default = TRUE for Listings and FALSE otherwise.

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

- border:

  (`fp_border`)  
  border to use.  
  Default =
  `flextable::fp_border_default(width = 0.875, color = "black")`.

- border_mat:

  (`matrix`)  
  a `m x k` matrix where m is the number of columns of the input
  Table/Listing and k is the number of lines the header takes up.  
  See
  [tidytlg::add_bottom_borders](https://pharmaverse.github.io/tidytlg/main/reference/add_bottom_borders.html)
  for what the matrix should contain. Users should only specify this
  when the default behavior does not meet their needs.

- export_csv:

  (`logical(1)`)  
  Whether to export the object as a csv representation. Default = FALSE.

- output_csv_directory:

  (`character(1)`)  
  the directory to export the csv. Default = NULL. Only used if
  export_csv = TRUE. If NULL or attempting to export in a non-existent
  directory, the csv will be exported in the same directory as the .docx
  file.

- markup_df:

  (`data.frame`)  
  Data frame containing markup information. Only used if export_csv =
  TRUE.

- validate:

  (`logical(1)`)  
  Whether to validate the table structure using
  [`rtables::validate_table_struct()`](https://insightsengineering.github.io/rtables/latest-tag/reference/validate_table_struct.html).
  Defaults to `TRUE`. If `FALSE`, a message will be displayed when
  validation fails.

- watermark:

  (`character`)  
  the watermark (text) to display in the output docx file.  
  If NULL, no watermark will be displayed. (optional) Default = NULL.

- plotnames:

  (`character`)  
  a file path, or a list of them, to previously saved .png files. These
  will be opened and exported in the output file. When exporting a
  Graph, at least `obj` (of class `ggplot2`) or `plotnames` must be
  provided. If both are provided, `obj` precedes and `plotnames` will be
  ignored.  
  (optional) Default = NULL.

- title:

  (`character`)  
  character, or list of them, with the titles to be displayed.  
  (optional) Default = NULL.

- footers:

  (`character`)  
  a list of footers to be displayed.  
  (optional) Default = NULL.

- plotwidth:

  (`numeric`)  
  plot size in units expressed by the units argument. If not supplied,
  uses the size of the current graphics device.  
  (optional) Default = 8.

- plotheight:

  (`numeric`)  
  plot size in units expressed by the units argument. If not supplied,
  uses the size of the current graphics device.  
  (optional) Default = 5.51.

- units:

  (`character`)  
  one of the following units in which the plotwidth and plotheight
  arguments are expressed: "in", "cm", "mm" or "px".  
  (optional) Default = "in".

- ...:

  other parameters.

## Note

This function may be removed from junco in the future if the
functionality is merged into `rtables.officer`.

For more information, refer to the vignette
`table_and_listing_customizations` (`browseVignettes("junco")`)

## Examples

``` r
adsl <- ex_adsl
adae <- ex_adae
extra_args_1 <- list(
  .stats = c("count_unique_denom_fraction")
)
lyt1 <- basic_table(show_colcounts = TRUE) |>
split_cols_by("ARM") |>
analyze(
  vars = "COUNTRY",
  afun = a_freq_j,
  extra_args = extra_args_1
)
tbl1 <- build_table(lyt1, adsl)
tab_titles <- list(
  "title" = "This is the main Title",
  "subtitles" = NULL,
  "main_footer" = c(
  "footer 1",
  "footer 2"
  ),
  "prov_footer" = NULL)
tbl1b <- set_titles(tbl1, tab_titles)

export_TLG_as_docx(
  obj = tbl1b,
  tblid = "test",
  output_dir = tempdir(),
  theme = theme_docx_default_j(), add_page_break = FALSE,
  titles_as_header = TRUE, integrate_footers = TRUE,
  section_properties = officer::prop_section(
    page_size = officer::page_size(width = 11, height = 8.5, orient = "portrait"),
    page_margins = officer::page_mar(
                             bottom = 1,
                             top = 1,
                             right = 1,
                             left = 1,
                             gutter = 0,
                             footer = 1,
                             header = 1)
  ),
  doc_metadata = NULL,
  template_file = NULL,
  orientation = "portrait",
  paginate = FALSE,
  nosplitin = list(
    row = character(),
    col = character()
  ),
  string_map = default_str_map,
  markup_df_docx = junco:::dps_markup_df_docx,
  combined_docx = FALSE,
  tlgtype = "Table",
  col_gap = 3,
  pagenum = FALSE,
  round_type = "iec",
  alignments = list(),
  border = flextable::fp_border_default(width = 0.875, color = "black"),
  border_mat = NULL,
  watermark = NULL,
  plotnames = NULL,
  title = NULL,
  footers = NULL,
  plotwidth = 8,
  plotheight = 5.51,
  units = "in"
)
```
