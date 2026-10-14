# Prepend a Label Row to a RowsVerticalSection Object

**\[stable\]**

Prepends a label row to a `RowsVerticalSection` object.

`RowsVerticalSection` objects are returned by analysis functions used
within the **rtables** framework and are typically created with
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html).

This function is typically used to introduce section headers (e.g.,
"Descriptive Statistics") in tabular reports and other reporting
outputs.

## Usage

``` r
prepend_label_cell(x, label = "", label_indent_mod = 0L)
```

## Arguments

- x:

  (`RowsVerticalSection`)  
  Analysis result object.

- label:

  (`character(1)`)  
  Label to insert as the first row.

- label_indent_mod:

  (`integer(1)`)  
  Indentation modifier applied to the label row.

## Value

A `RowsVerticalSection` object with a label row prepended.

## Note

The `row_na_strs` and `row_footnotes` attributes of the
`RowsVerticalSection` object are not preserved. Since an additional row
is added, be aware of recycling rules; that is, the length of all
attributes of `x` should be equal to `length(x)`.

## Examples

``` r
rvs <- rtables::in_rows(
  Mean = rtables::rcell(5),
  Range = rtables::rcell(c(1, 8))
)

prepend_label_cell(
  rvs,
  "Descriptive Statistics",
  label_indent_mod = 1L
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod              row_label
#> 1  __label                         1 Descriptive Statistics
#> 2     Mean              5          0                   Mean
#> 3    Range           1, 8          0                  Range
```
