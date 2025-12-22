# Obtain the default theme for the docx

**\[experimental\]**

This function is based on
[`rtables.officer::theme_docx_default()`](https://insightsengineering.github.io/rtables.officer/latest-tag/reference/tt_to_flextable.html).
See notes to understand why this is experimental.

## Usage

``` r
theme_docx_default_j(
  font = "Times New Roman",
  font_size = 9L,
  cell_margins = c(0, 0, 0, 0),
  bold = c("header", "content_rows", "label_rows", "top_left"),
  bold_manual = NULL,
  border = flextable::fp_border_default(width = 0.75, color = "black")
)
```

## Arguments

- font:

  (`string`)  
  font. Defaults to "Times New Roman".

- font_size:

  (`integer(1)`)  
  font size. Defaults to 9.

- cell_margins:

  (`numeric(1) or numeric(4)`)  
  a numeric or a vector of four numbers indicating c("left", "right",
  "top", "bottom"). It defaults to 0mm in Word pt to all 4 margins.

- bold:

  (`character`)  
  parts of the table text that should be in bold. Can be any combination
  of c("header", "content_rows", "label_rows", "top_left"). The first
  one renders all column names bold (not topleft content). The second
  and third option use formatters::make_row_df() to render content
  or/and label rows as bold.

- bold_manual:

  (`named list or NULL`)  
  list of index lists. See example for needed structure. Accepted
  groupings/names are c("header", "body").

- border:

  (`fp_border`)  
  border to use. Defaults to width = 0.75 and color = "black"

## Value

a function that applies the given theme to a flextable.

## Note

This function has been tested for common use cases but may not work or
have unexpected or undesired behavior in corner cases. As such it is not
considered fully production ready and is being made available for
further testing and early adoption. Please report any issues you
encounter to the developers. This function may be removed from junco in
the future if the functionality is merged into `rtables.officer`.
