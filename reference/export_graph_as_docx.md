# export_graph_as_docx

Export graph in DOCX format.

## Usage

``` r
export_graph_as_docx(
  g = NULL,
  plotnames = NULL,
  tblid = NULL,
  output_dir = NULL,
  title = NULL,
  footers = NULL,
  orientation = "portrait",
  plotwidth = 8,
  plotheight = 5.51,
  units = c("in", "cm", "mm", "px")[1],
  border = flextable::fp_border_default(width = 0.875, color = "black"),
  watermark = NULL
)
```

## Arguments

- g:

  (`ggplot2`)  
  a `ggplot2` object, or a list of them, to export. At least one of `g`
  or `plotnames` must be provided. If both are provided, `g` precedes
  and `plotnames` will be ignored.  
  (optional) Default = NULL.

- plotnames:

  (`list`)  
  a file path, or a list of them, to previously saved .png files. These
  will be opened and exported in the output file. At least `g` (of class
  `ggplot2`) or `plotnames` must be provided. If both are provided, `g`
  precedes and `plotnames` will be ignored.  
  (optional) Default = NULL.

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

- title:

  (`character`)  
  character, or list of them, with the titles to be displayed.  
  (optional) Default = NULL.

- footers:

  (`character`)  
  a list of footers to be displayed.  
  (optional) Default = NULL.

- orientation:

  (`character`)  
  one of: "portrait", "landscape".  
  (optional) Default = "portrait".

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

- border:

  (`fp_border`)  
  border to use.  
  Default =
  `flextable::fp_border_default(width = 0.875, color = "black")`.

- watermark:

  (`character`)  
  the watermark (text) to display in the output docx file.  
  If NULL, no watermark will be displayed. (optional) Default = NULL.

## Note

This function may be removed from junco in the future if the
functionality is merged into `rtables.officer`.

For more information, refer to the vignette
`table_and_listing_customizations` (`browseVignettes("junco")`)
