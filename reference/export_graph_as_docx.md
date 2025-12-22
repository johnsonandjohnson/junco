# export_graph_as_docx

**\[experimental\]**

Export graph in DOCX format. See notes to understand why this is
experimental.

## Usage

``` r
export_graph_as_docx(
  g = NULL,
  plotnames = NULL,
  tblid,
  output_dir,
  title = NULL,
  footers = NULL,
  orientation = "portrait",
  plotwidth = 8,
  plotheight = 5.51,
  units = c("in", "cm", "mm", "px")[1],
  border = flextable::fp_border_default(width = 0.75, color = "black")
)
```

## Arguments

- g:

  (optional) Default = NULL. A `ggplot2` object, or a list of them, to
  export. At least one of `g` or `plotnames` must be provided. If both
  are provided, 'g' precedes and 'plotnames' will be ignored.

- plotnames:

  (optional) Default = NULL. A file path, or a list of them, to
  previously saved .png files. These will be opened and exported in the
  output file. At least one of `g` or `plotnames` must be provided. If
  both are provided, 'g' precedes and 'plotnames' will be ignored.

- tblid:

  Character. Output ID that will appear in the Title and footer.

- output_dir:

  Character. File path where to save the output.

- title:

  (optional) Default = NULL. Character, or list of them, with the titles
  to be displayed.

- footers:

  (optional) Default = NULL. Character, or list of them, with the
  footers to be displayed.

- orientation:

  (optional) Default = "portrait". One of: "portrait", "landscape".

- plotwidth:

  (optional) Default = 8. Plot size in units expressed by the units
  argument. If not supplied, uses the size of the current graphics
  device.

- plotheight:

  (optional) Default = 5.51. Plot size in units expressed by the units
  argument. If not supplied, uses the size of the current graphics
  device.

- units:

  (optional) Default = "in". One of the following units in which the
  plotwidth and plotheight arguments are expressed: "in", "cm", "mm" or
  "px".

- border:

  (optional). An `fp_border` object to use as borders for the Title and
  Footers.

## Note

This function has been tested for common use cases but may not work or
have unexpected or undesired behavior in corner cases. As such it is not
considered fully production ready and is being made available for
further testing and early adoption. Please report any issues you
encounter to the developers. This function may be removed from junco in
the future if the functionality is merged into `rtables.officer`.
