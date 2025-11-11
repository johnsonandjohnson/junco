# Default String Mapping for Special Characters

A tibble that maps special characters to their UTF-8 equivalents for use
in RTF output. Currently it maps "\>=" and "\<=" to the Unicode
characters.

## Usage

``` r
default_str_map
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 2
rows and 2 columns.

## Value

A tibble with columns 'pattern' and 'value', where 'pattern' contains
the string to be replaced and 'value' contains the replacement.
