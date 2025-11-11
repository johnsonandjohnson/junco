# Check Word Wrapping

Check a set of column widths for word-breaking wrap behavior

## Usage

``` r
check_wrap_nobreak(tt, colwidths, fontspec)
```

## Arguments

- tt:

  TableTree

- colwidths:

  numeric. Column widths (in numbers of spaces under `fontspec`)

- fontspec:

  font_spec.

## Value

`TRUE` if the wrap is able to be done without breaking words, `FALSE` if
wordbreaking is required to apply `colwidths`
