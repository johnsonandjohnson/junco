# s_function for proportion of factor levels

A simple statistics function which prepares the numbers with percentages
in the required format. The denominator here is from the alternative
counts data set in the given row and column split.

If a total row is shown, then here just the total number is shown
(without 100%).

## Usage

``` r
s_proportion_factor(
  x,
  .alt_df,
  use_alt_counts = TRUE,
  show_total = c("none", "top", "bottom"),
  total_label = "Total"
)
```

## Arguments

- x:

  (`factor`)  
  categorical variable we want to analyze.

- .alt_df:

  (`data.frame`)  
  alternative data frame used for denominator calculation.

- use_alt_counts:

  (`flag`)  
  whether the `.alt_df` should be used for the total, i.e. the
  denominator. If not, then the number of non-missing values in `x` is
  used.

- show_total:

  (`string`)  
  show the total level optionally on the top or in the bottom of the
  factor levels.

- total_label:

  (`string`)  
  which label to use for the optional total level.

## Value

The
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
result with the proportion statistics.

## See also

[`s_proportion_logical()`](https://johnsonandjohnson.github.io/junco/reference/s_proportion_logical.md)
for tabulating logical `x`.
