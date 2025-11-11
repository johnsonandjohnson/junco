# Insertion of Blank Lines in a Layout

This is a hack for `rtables` in order to be able to add row gaps, i.e.
blank lines. In particular, by default this function needs to maintain a
global state for avoiding duplicate table names. The global state
variable is hidden by using a dot in front of its name. However, this
likely won't work with parallelisation across multiple threads and also
causes non-reproducibility of the resulting `rtables` object. Therefore
also a custom table name can be used.

## Usage

``` r
insert_blank_line(lyt, table_names = NULL)
```

## Arguments

- lyt:

  (`layout`)  
  input layout where analyses will be added to.

- table_names:

  (`character`)  
  this can be customized in case that the same `vars` are analyzed
  multiple times, to avoid warnings from `rtables`.

## Value

The modified layout now including a blank line after the current row
content.

## Examples

``` r
ADSL <- ex_adsl

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_rows_by("STRATA1") |>
  analyze(vars = "AGE", afun = function(x) {
    in_rows(
      "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)")
    )
  }) |>
  insert_blank_line() |>
  analyze(vars = "AGE", table_names = "AGE_Range", afun = function(x) {
    in_rows(
      "Range" = rcell(range(x), format = "xx.xx - xx.xx")
    )
  })
build_table(lyt, ADSL)
#>                   A: Drug X      B: Placebo     C: Combination
#> ——————————————————————————————————————————————————————————————
#> A                                                             
#>   AGE                                                         
#>     Mean (sd)   33.08 (5.70)    35.11 (7.92)     34.23 (6.18) 
#>                                                               
#>   AGE                                                         
#>     Range       24.00 - 46.00   23.00 - 62.00   20.00 - 47.00 
#> B                                                             
#>   AGE                                                         
#>     Mean (sd)   33.85 (7.24)    36.00 (9.08)     36.33 (8.40) 
#>                                                               
#>   AGE                                                         
#>     Range       23.00 - 48.00   21.00 - 58.00   21.00 - 64.00 
#> C                                                             
#>   AGE                                                         
#>     Mean (sd)   34.22 (6.57)    35.18 (6.65)     35.63 (8.25) 
#>                                                               
#>   AGE                                                         
#>     Range       21.00 - 50.00   23.00 - 51.00   24.00 - 69.00 
```
