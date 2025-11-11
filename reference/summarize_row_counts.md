# Layout Creating Function Adding Row Counts

This is a simple wrapper of
[`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
and the main additional value is that we can choose whether we want to
use the alternative (usually ADSL) data set for the counts (default) or
use the original data set.

## Usage

``` r
summarize_row_counts(lyt, label_fstr = "%s", alt_counts = TRUE)
```

## Arguments

- lyt:

  (`layout`)  
  input layout where analyses will be added to.

- label_fstr:

  (`string`)  
  a `sprintf` style format string. It can contain up to one `%s` which
  takes the current split value and generates the row label.

- alt_counts:

  (`flag`)  
  whether row counts should be taken from `alt_counts_df` (`TRUE`) or
  from `df` (`FALSE`).

## Value

A modified layout where the latest row split now has a row group
summaries (as created by
[rtables::summarize_row_groups](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
for the counts. for the counts.

## Examples

``` r
basic_table() |>
  split_cols_by("ARM") |>
  add_colcounts() |>
  split_rows_by("RACE", split_fun = drop_split_levels) |>
  summarize_row_counts(label_fstr = "RACE value - %s") |>
  analyze("AGE", afun = list_wrap_x(summary), format = "xx.xx") |>
  build_table(DM, alt_counts_df = rbind(DM, DM))
#>                                          A: Drug X   B: Placebo   C: Combination
#>                                           (N=242)     (N=212)        (N=258)    
#> ————————————————————————————————————————————————————————————————————————————————
#> RACE value - ASIAN                          158         136            168      
#>   Min.                                     20.00       21.00          22.00     
#>   1st Qu.                                  29.00       28.00          30.00     
#>   Median                                   33.00       32.50          33.50     
#>   Mean                                     34.20       32.68          34.63     
#>   3rd Qu.                                  38.50       36.00          38.00     
#>   Max.                                     58.00       55.00          53.00     
#> RACE value - BLACK OR AFRICAN AMERICAN      56           48             54      
#>   Min.                                     23.00       21.00          24.00     
#>   1st Qu.                                  29.00       28.75          29.00     
#>   Median                                   33.00       30.00          32.00     
#>   Mean                                     34.68       31.71          34.00     
#>   3rd Qu.                                  37.25       36.25          39.00     
#>   Max.                                     60.00       42.00          51.00     
#> RACE value - WHITE                          28           28             36      
#>   Min.                                     30.00       25.00          28.00     
#>   1st Qu.                                  38.00       31.00          30.25     
#>   Median                                   40.50       37.50          35.00     
#>   Mean                                     39.36       36.93          35.11     
#>   3rd Qu.                                  43.50       40.00          37.50     
#>   Max.                                     47.00       55.00          47.00     
```
