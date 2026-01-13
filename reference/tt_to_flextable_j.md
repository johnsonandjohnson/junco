# Convert a VTableTree or a listing_df object to a flextable

**\[experimental\]**

This function is based on
[`rtables.officer::tt_to_flextable()`](https://insightsengineering.github.io/rtables.officer/latest-release/reference/tt_to_flextable.html).
See notes to understand why this is experimental.

## Usage

``` r
tt_to_flextable_j(
  tt,
  tblid,
  theme = theme_docx_default_j(font = "Times New Roman", font_size = 9L, bold = NULL),
  border = flextable::fp_border_default(width = 0.75, color = "black"),
  indent_size = NULL,
  titles_as_header = TRUE,
  bold_titles = TRUE,
  integrate_footers = TRUE,
  counts_in_newline = FALSE,
  paginate = FALSE,
  fontspec = formatters::font_spec("Times", 9L, 1.2),
  lpp = NULL,
  cpp = NULL,
  ...,
  colwidths = NULL,
  tf_wrap = !is.null(cpp),
  max_width = cpp,
  total_page_height = 10,
  total_page_width = my_pg_width_by_orient(orientation),
  autofit_to_page = TRUE,
  orientation = "portrait",
  nosplitin = character(),
  string_map = junco::default_str_map,
  markup_df_docx = dps_markup_df_docx,
  reduce_first_col_indentation = FALSE,
  tlgtype = (utils::getFromNamespace("tlg_type", "junco"))(tt),
  col_gap = ifelse(tlgtype == "Listing", 0.5, 3),
  pagenum = ifelse(tlgtype == "Listing", TRUE, FALSE),
  round_type = formatters::obj_round_type(tt),
  alignments = list(),
  border_mat = make_header_bordmat(obj = tt)
)
```

## Arguments

- tt:

  a VTableTree or a listing_df object

- tblid:

  Character. Output ID to be displayed in the Title and last line of
  footer.

- theme:

  (optional) a function factory. See theme_docx_default_j() or
  rtables.officer::theme_docx_default() for more details.

- border:

  (optional) an `fp_border` object.

- indent_size:

  (optional) Numeric. Not used and set to 9 points internally.

- titles_as_header:

  (optional) Default = TRUE.

- bold_titles:

  (optional) Default = TRUE.

- integrate_footers:

  (optional) Default = TRUE.

- counts_in_newline:

  (optional) Default = FALSE.

- paginate:

  (optional) Default = FALSE.

- fontspec:

  (optional) a font_spec object.

- lpp:

  (optional) Default = NULL. Not used.

- cpp:

  (optional) Default = NULL. Not used.

- ...:

  other arguments.

- colwidths:

  (optional) Default = NULL.

- tf_wrap:

  (optional) Default = FALSE. Not used.

- max_width:

  (optional) Default = NULL. Not used.

- total_page_height:

  (optional) Default = 10. Not used.

- total_page_width:

  (optional). No need to be specified by end user. Set to 6.38
  ("portrait") or 8.88 ("landscape").

- autofit_to_page:

  (optional) Default = TRUE. Not used and set to FALSE internally.

- orientation:

  (optional) Default = "portrait". One of: "portrait", "landscape".

- nosplitin:

  (optional) Default = character(). Named list.

- string_map:

  (optional) Default = default_str_map.

- markup_df_docx:

  (optional) Default = dps_markup_df_docx.

- reduce_first_col_indentation:

  (optional) Default = FALSE.

- tlgtype:

  (optional). No need to be specified by end user.

- col_gap:

  (optional). Default = 3 (Tables) or 0.5 (Listings).

- pagenum:

  (optional). Default = FALSE (Tables) or TRUE (Listings).

- round_type:

  (`"iec"` or `"sas"`)  
  the type of rounding to perform. iec, the default, performs rounding
  compliant with IEC 60559, while sas performs nearest-value rounding
  consistent with rounding within SAS. See
  `[formatters::format_value()]` for more details.

- alignments:

  (`list`)  
  List of named lists. Vectorized. (Default =
  [`list()`](https://rdrr.io/r/base/list.html)) Used to specify
  individual column or cell alignments. Each named list contains `row`,
  `col`, and `value`.

- border_mat:

  (`matrix`)  
  A `m x k` matrix where m is the number of columns of `tt` and k is the
  number of lines the header takes up. See
  [tidytlg::add_bottom_borders](https://pharmaverse.github.io/tidytlg/main/reference/add_bottom_borders.html)
  for what the matrix should contain. Users should only specify this
  when the default behavior does not meet their needs.

## Value

a flextable object.

## Note

This function has been tested for common use cases but may not work or
have unexpected or undesired behavior in corner cases. As such it is not
considered fully production ready and is being made available for
further testing and early adoption. Please report any issues you
encounter to the developers. This function may be removed from junco in
the future if the functionality is merged into `rtables.officer`.
