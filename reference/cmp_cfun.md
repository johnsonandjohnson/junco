# Summary Analysis Function for Compliance Columns

A simple statistics function which prepares the numbers with percentages
in the required format, for use in a split content row. The denominator
here is from the expected visits column.

## Usage

``` r
cmp_cfun(df, labelstr, .spl_context, variables, formats)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`character`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- variables:

  (`list`)  
  with variable names of logical columns for `expected`, `received` and
  `missing` visits.

- formats:

  (`list`)  
  with the `count_percent` format to use for the received and missing
  visits columns.

## Value

The
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
result with the counts and proportion statistics.

## See also

[`cmp_post_fun()`](https://johnsonandjohnson.github.io/junco/reference/cmp_post_fun.md)
for the corresponding split function.
