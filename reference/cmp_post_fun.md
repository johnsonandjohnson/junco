# Split Function for Compliance Columns (TEFSCNCMP01 e.g.)

Here we just split into 3 columns for expected, received and missing
visits.

## Usage

``` r
cmp_post_fun(ret, spl, fulldf, .spl_context)

cmp_split_fun(df, spl, vals = NULL, labels = NULL, trim = FALSE, .spl_context)
```

## Arguments

- ret:

  (`list`)  
  result from previous split function steps.

- spl:

  (`split`)  
  split object.

- fulldf:

  (`data.frame`)  
  full data frame.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- vals:

  (`character`)  
  values to use for the split.

- labels:

  (named `character`)  
  labels for the statistics (without indent).

- trim:

  (`logical`)  
  whether to trim the values.

## Value

a split function for use with
[rtables::split_rows_by](https://insightsengineering.github.io/rtables/latest-tag/reference/split_rows_by.html)
when creating proportion-based tables with compliance columns.

## Note

This split function is used in the proportion table TEFSCNCMP01 and
similar ones.

## See also

[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
describing the requirements for this kind of post-processing function.
