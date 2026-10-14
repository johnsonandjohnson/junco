# Statistics within the column space

A function used for obtaining statistics within the columns of your
table. Used in change from baseline tables. This takes the visit names
as its row labels.

## Usage

``` r
column_stats(
  df,
  .var,
  .spl_context,
  .formats = NULL,
  formats_var = NULL,
  exclude_visits = c("Baseline (DB)"),
  var_names = c("AVAL", "CHG", "BASE")
)
```

## Arguments

- df:

  (`data.frame` or `tibble`)  
  dataset.

- .var:

  (`string`)  
  single variable name that is passed by `rtables`

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. When set to `"default"` `formats_var`
  argument will be used for format setup.

- formats_var:

  (`string` or `NULL`)  
  `NULL` (the default) or the name of the list column containing named
  lists of default formats to use. These will not override .formats
  (other than `"default"`).

- exclude_visits:

  Visits to exclude for the second/third part of the columns ("BASE",
  "CHG").

- var_names:

  (`character vector`)  
  Vector of variable names to use instead of the default AVAL, CHG,
  BASE. By default, the function expects these specific variable names
  in your data, but you can customize them to match your dataset's
  column names.

## Value

An analysis function (for use with
[rtables::analyze](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html))
implementing the specified statistics.  
Typically used in a layout that has been setup by
[`stats_in_cols_setup()`](https://johnsonandjohnson.github.io/junco/reference/stats_in_cols_setup.md)

## Examples

``` r
# example code
library(dplyr)
advs <- ex_advs |>
  filter(AVISIT %in% toupper(c("Screening", "Baseline", "Week 1 Day 8", "Week 2 Day 15"))) |>
  mutate(AVISIT = droplevels(AVISIT)) |>
  filter(PARAMCD %in% c("DIABP", "SYSBP"))
advs_fmt <- tribble(
  ~PARAMCD, ~fmt_d,
  "DIABP", list(
    mean = "xx.xx", sd = "xx.xxx", se = "xx.xxx", median = "xx.xx",
    min = "xx.x", max = "xx.x", mean_sd = "xx.xx (xx.xxx)"
  ),
  "SYSBP", list(
    mean = "xx.x", sd = "xx.xx", se = "xx.xx", median = "xx.x",
    min = "xx.", max = "xx.", mean_sd = "xx.x (xx.xx)"
  )
)

advs <- left_join(advs, advs_fmt)
#> Joining with `by = join_by(PARAMCD)`

mysplitfun <- make_split_fun(
  post = list(stats_in_cols_setup())
)

lyt <- basic_table() |>
  split_cols_by_multivar(
    c("AVAL", "BASE", "CHG"),
    varlabels = c("Visit Values", " ", "Change from Baseline")
  ) |>
  split_cols_by("STUDYID", split_fun = mysplitfun) |>
  split_rows_by("PARAM", split_fun = drop_split_levels) |>
  split_rows_by("ARMCD", labels_var = "ARM") |>
  analyze("AVISIT", afun = column_stats, extra_args = list(
    exclude_visits = toupper(c("Baseline", "Screening")),
    .formats = "default",
    formats_var = "fmt_d"
  ))
result <- build_table(lyt, advs, round_type = "sas")
head(result, 20)
#>                                              Visit Values                                                      Change from Baseline                
#>                             N    Mean     SD      SE      Med    Min    Max    Base Mean (SD)    N    Mean      SD      SE      Med     Min    Max 
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Diastolic Blood Pressure                                                                                                                           
#>   A: Drug X                                                                                                                                        
#>     SCREENING              134   49.97   7.197   0.622   49.71   31.7   71.2                                                                       
#>     BASELINE               134   48.60   7.962   0.688   48.42   27.7   64.6                                                                       
#>     WEEK 1 DAY 8           134   50.26   7.514   0.649   50.09   33.0   69.0   48.60 (7.962)    134   1.66    10.774   0.931   0.61    -20.4   29.9
#>     WEEK 2 DAY 15          134   50.84   7.850   0.678   51.44   31.9   70.3   48.60 (7.962)    134   2.23    11.752   1.015   3.13    -23.9   39.2
#>   B: Placebo                                                                                                                                       
#>     SCREENING              134   50.75   8.429   0.728   50.09   29.3   69.2                                                                       
#>     BASELINE               134   50.44   7.948   0.687   50.18   21.7   67.5                                                                       
#>     WEEK 1 DAY 8           134   49.67   7.743   0.669   49.75   33.7   66.5   50.44 (7.948)    134   -0.77   10.947   0.946   -1.11   -25.8   28.4
#>     WEEK 2 DAY 15          134   49.72   8.445   0.730   50.23   30.6   68.1   50.44 (7.948)    134   -0.72   12.444   1.075   -0.71   -28.4   37.0
#>   C: Combination                                                                                                                                   
#>     SCREENING              132   50.19   7.627   0.664   49.64   26.9   70.0                                                                       
#>     BASELINE               132   51.11   7.790   0.678   50.80   29.7   71.4                                                                       
#>     WEEK 1 DAY 8           132   48.86   7.888   0.687   47.68   30.4   67.0   51.11 (7.790)    132   -2.25   10.380   0.903   -2.23   -22.1   22.4
#>     WEEK 2 DAY 15          132   49.98   8.338   0.726   51.01   24.8   65.6   51.11 (7.790)    132   -1.13   11.639   1.013   -2.27   -29.2   30.7
#> Systolic Blood Pressure                                                                                                                            
#>   A: Drug X                                                                                                                                        
#>     SCREENING              134   49.6    8.32    0.72    49.6     24     71                                                                        
#>     BASELINE               134   49.4    8.47    0.73    48.3     24     71                                                                        
```
