# c_function for proportion of `TRUE` in logical vector

A simple statistics function which prepares the numbers with percentages
in the required format, for use in a split content row. The denominator
here is from the column N. Note that we don't use here .alt_df because
that might not have required row split variables available.

## Usage

``` r
c_proportion_logical(x, labelstr, label_fstr, format, .N_col)
```

## Arguments

- x:

  (`logical`)  
  binary variable we want to analyze.

- labelstr:

  (`string`)  
  label string.

- label_fstr:

  (`string`)  
  format string for the label.

- format:

  (`character` or `list`)  
  format for the statistics.

- .N_col:

  (`numeric`)  
  number of columns.

## Value

The
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
result with the proportion statistics.

## See also

[`s_proportion_logical()`](https://johnsonandjohnson.github.io/junco/reference/s_proportion_logical.md)
for the related statistics function.
