# Function factory to be used as Post-processing split function for setup of stats in column structure

This helper is designed to be used in the `post` argument of
[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
to expand a column facet (e.g. AVAL/BASE/CHG) into the specific
statistics to be analyzed for each subfacet. It returns a split result
instructing rtables which values/labels/subsets to create.

Typical usage:

- construct a split function like
  `mysplitfun <- rtables::make_split_fun(post = list(stats_in_cols_setup()))`  
  Then use `mysplitfun` in your table layout where you split columns by
  a variable whose levels are one of "AVAL", "BASE", or "CHG" and want
  to analyze different statistics for each.

- column layout setup from
  [`rtables::split_cols_by_multivar()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html),
  followed by split_cols_by using `stats_in_cols_setup` in conjunction
  with an analyze call with `afun` =
  [`column_stats()`](https://johnsonandjohnson.github.io/junco/reference/column_stats.md)

See examples for typical usage.

## Usage

``` r
stats_in_cols_setup(
  stats_list = list(AVAL = c("n", "mean", "sd", "se", "median", "min", "max"), BASE =
    c("mean_sd"), CHG = c("n", "mean", "sd", "se", "median", "min", "max")),
  statlbls_list = c(median = "Med", min = "Min", max = "Max", mean_sd = "Base Mean (SD)",
    n = "N")
)
```

## Arguments

- stats_list:

  `(list)` Named list with statistics to present for each variable. Any
  statistic from tern::s_summary can be selected.

- statlbls_list:

  “ Named vector with labels to use for the statistic in column header.
  If NULL, or a statistic name is not included, the default label for
  the statistic will be used.

## Value

A function that can be used inside
[`rtables::make_split_result()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_result.html)
statistics for the current column level.

## See also

[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html),
[`rtables::make_split_result()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_result.html).

## Examples

``` r
# example code
library(dplyr)
advs <- ex_advs |>
  filter(AVISIT %in% toupper(c("Screening", "Baseline", "Week 1 Day 8", "Week 2 Day 15"))) |>
  mutate(AVISIT = droplevels(AVISIT))

mysplitfun <- make_split_fun(
  post = list(stats_in_cols_setup())
)
lyt <- basic_table() |>
  split_cols_by_multivar(
    c("AVAL", "BASE", "CHG"),
    varlabels = c("Visit Values", " ", "Change from Baseline")
  ) |>
  split_cols_by("STUDYID", split_fun = mysplitfun) |>
  split_rows_by("PARAM") |>
  split_rows_by("ARMCD", labels_var = "ARM") |>
  analyze("AVISIT",
   afun = column_stats,
   extra_args = list(exclude_visits = toupper(c("Baseline", "Screening")))
  )
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
#> Pulse Rate                                                                                                                                         
#>   A: Drug X                                                                                                                                        
#>     SCREENING              134   49.60   8.981   0.776   49.87   25.9   68.8                                                                       
#>     BASELINE               134   51.86   8.077   0.698   51.23   28.5   72.9                                                                       
```
