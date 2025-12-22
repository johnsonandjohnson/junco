# Shortcut for Creating Custom Column Splits

This is a shortcut for a common use of
[`rtables::make_split_result()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_result.html)
where you need to create custom column splits with different labels but
using the same full dataset for each column. It automatically sets up
the values, datasplit (using the same full dataset for each column), and
subset_exprs (using TRUE for all subsets) parameters for
make_split_result().

## Usage

``` r
short_split_result(..., fulldf)
```

## Arguments

- ...:

  sequence of named labels for the columns.

- fulldf:

  (`data.frame`)  
  The `fulldf` which will be used for each column.

## Value

The result from
[`rtables::make_split_result()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_result.html).
