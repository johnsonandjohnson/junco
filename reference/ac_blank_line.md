# Analysis and Content Summary Function Producing Blank Line

Analysis and Content Summary Function Producing Blank Line

## Usage

``` r
ac_blank_line(df, labelstr = "")
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`character`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://rdrr.io/pkg/rtables/man/summarize_row_groups.html)
  for more information.
