# Split Function for Proportion Analysis Columns (TEFCGIS08 e.g.)

Here we just split into 3 columns `n`, `%` and `Cum %`.

## Usage

``` r
prop_post_fun(ret, spl, fulldf, .spl_context)

prop_split_fun(df, spl, vals = NULL, labels = NULL, trim = FALSE, .spl_context)
```

## Arguments

- ret:

  (`list`)  
  return value from the previous split function.

- spl:

  (`list`)  
  split information.

- fulldf:

  (`data.frame`)  
  full data frame.

- .spl_context:

  (`environment`)  
  split context environment.

- df:

  A data frame that contains all analysis variables.

- vals:

  A character vector that contains values to use for the split.

- labels:

  A character vector that contains labels for the statistics (without
  indent).

- trim:

  A single logical that indicates whether to trim the values.

## Value

a split function for use in
[rtables::split_rows_by](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html).

## Note

This split function is used in the proportion table TEFCGIS08 and
similar ones.

## See also

[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
describing the requirements for this kind of post-processing function.
