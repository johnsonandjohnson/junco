# Default String Mapping for Special Characters

A tibble that maps special characters to their UTF-8 equivalents for use
in RTF output. Currently it maps "\>=" and "\<=" to the Unicode
characters.

## Usage

``` r
default_str_map
```

## Value

A tibble with columns 'pattern' and 'value', where 'pattern' contains
the string to be replaced and 'value' contains the replacement.
