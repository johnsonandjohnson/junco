# Obtain the default theme for the docx

This function is based on
[`rtables.officer::theme_docx_default()`](https://insightsengineering.github.io/rtables.officer/latest-tag/reference/tt_to_flextable.html).

## Usage

``` r
theme_docx_default_j(
  font = "Times New Roman",
  font_size = 9L,
  cell_margins = c(0, 0, 0, 0),
  bold = c("header", "content_rows", "label_rows", "top_left"),
  bold_manual = NULL,
  border = flextable::fp_border_default(width = 0.875, color = "black")
)
```

## Arguments

- font:

  (`string`)  
  (optional) Default = "Times New Roman".

- font_size:

  (`integer(1)`)  
  (optional) Default = 9.

- cell_margins:

  (`numeric(1) or numeric(4)`)  
  a numeric or a vector of four numbers indicating c("left", "right",
  "top", "bottom").  
  (optional) Default = c(0, 0, 0, 0).

- bold:

  (`character`)  
  parts of the table text that should be in bold. Can be any combination
  of c("header", "content_rows", "label_rows", "top_left"). The first
  one renders all column names bold (not topleft content). The second
  and third option use
  [`formatters::make_row_df()`](https://insightsengineering.github.io/formatters/latest-tag/reference/make_row_df.html)
  to render content or/and label rows as bold.  
  (optional) Default =
  `c("header", "content_rows", "label_rows", "top_left")`.

- bold_manual:

  (`named list`)  
  list of index lists. Accepted groupings/names are c("header",
  "body").  
  See examples in
  [`rtables.officer::theme_docx_default()`](https://insightsengineering.github.io/rtables.officer/latest-tag/reference/tt_to_flextable.html).  
  (optional) Default = NULL.

- border:

  (`fp_border`)  
  border to use.  
  Default =
  `flextable::fp_border_default(width = 0.875, color = "black")`.

## Value

A function that applies the given theme to a flextable.

## Note

This function may be removed from junco in the future if the
functionality is merged into `rtables.officer`.

For more information, refer to the vignette
`table_and_listing_customizations` (`browseVignettes("junco")`)
