# s_function for proportion of `TRUE` in logical vector

A simple statistics function which prepares the numbers with percentages
in the required format. The denominator here is from the alternative
counts data set in the given row and column split.

## Usage

``` r
s_proportion_logical(x, label = "Responders", .alt_df)
```

## Arguments

- x:

  (`logical`)  
  binary variable we want to analyze.

- label:

  (`string`)  
  label to use.

- .alt_df:

  (`data.frame`)  
  alternative data frame used for denominator calculation.

## Value

The
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
result with the proportion statistics.

## See also

[`s_proportion_factor()`](https://johnsonandjohnson.github.io/junco/reference/s_proportion_factor.md)
for tabulating factor `x`.
