# Prepend Label Row to Analysis Output

**\[experimental\]**

Adds a label row at the beginning of analysis output objects, such as
`CellValue`, `list` of `CellValue`s, or `RowsVerticalSection` objects.
These objects are returned by analysis functions used within the
**rtables** framework and are typically created via
[`rtables::rcell()`](https://insightsengineering.github.io/rtables/latest-tag/reference/rcell.html)
or
[`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
functions.

This is typically used to introduce section headers (e.g., "Descriptive
Statistics") in tabular or reporting outputs.

## Usage

``` r
prepend_label_cell(x, label = "", label_indent_mod = 0L)
```

## Arguments

- x:

  (`list` or `CellValue` or `RowsVerticalSection`)  
  Analysis result object.

- label:

  (`character(1)`)  
  Label to be inserted as the first row.

- label_indent_mod:

  (`integer(1)`)  
  Indentation level applied to the label row.

## Value

A `RowsVerticalSection` object with the label row prepended.

## Note

If `x` is of class `RowsVerticalSection`, the attributes `row_formats`,
`row_na_strs`, and `row_footnotes` are not preserved.

## Examples

``` r
rvs <- rtables::in_rows(Mean = rtables::rcell(5), Range = rtables::rcell(c(1, 8)))
prepend_label_cell(rvs, "Descriptive Statistics", label_indent_mod = 1L)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>   row_name formatted_cell indent_mod              row_label
#> 1                                  1 Descriptive Statistics
#> 2     Mean              5          0                   Mean
#> 3    Range           1, 8          0                  Range
```
