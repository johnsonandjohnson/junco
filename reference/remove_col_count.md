# Removal of Unwanted Column Counts

Remove the N=xx column headers for specified span_label_var columns -
default is 'rrisk_header'.

## Usage

``` r
remove_col_count(obj, span_label_var = "rrisk_header")
```

## Arguments

- obj:

  (`TableTree`)  
  TableTree object.

- span_label_var:

  (`character`)  
  The spanning header text variable value for which column headers will
  be removed from.

## Value

TableTree object with column counts in specified columns removed.

## Details

This works for only the lowest level of column splitting (since
colcounts is used).
