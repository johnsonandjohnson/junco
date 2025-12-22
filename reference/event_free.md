# Workaround statistics function to time point survival estimate with CI

This is a workaround for
[`tern::s_surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html),
which adds a statistic containing the time point specific survival
estimate together with the confidence interval.

## Usage

``` r
a_event_free(
  df,
  .var,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_event_free(
  df,
  .var,
  time_point,
  time_unit,
  is_event,
  percent = FALSE,
  control = control_surv_timepoint()
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

- ...:

  additional arguments for the lower level functions.

- .stats:

  (`character`)  
  statistics to select for the table.

- .formats:

  (named `character` or `list`)  
  formats for the statistics. See Details in `analyze_vars` for more
  information on the `'auto'` setting.

- .labels:

  (named `character`)  
  labels for the statistics (without indent).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- time_point:

  (`numeric`)  
  time point at which to estimate survival.

- time_unit:

  (`string`)  
  unit of time for the time point.

- is_event:

  (`character`)  
  variable name storing Logical values: `TRUE` if event, `FALSE` if time
  to event is censored.

- percent:

  (`flag`)  
  whether to return in percent or not.

- control:

  (`list`)  
  relevant list of control options.

## Value

- `s_event_free` returns a list as returned by the
  [`tern::s_surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html)
  with an additional three-dimensional statistic `event_free_ci` which
  combines the `event_free_rate` and `rate_ci` statistics.

- `a_event_free` is analogous to
  [tern::a_surv_timepoint](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html)
  but with the additional three-dimensional statistic described above
  available via `.stats`.

## Functions

- `a_event_free()`: Formatted analysis function which is used as `afun`.

- `s_event_free()`: Statistics function which works like
  [`tern::s_surv_timepoint()`](https://insightsengineering.github.io/tern/latest-tag/reference/survival_timepoint.html),
  the difference is that it returns the additional statistic
  `event_free_ci`.

## Examples

``` r
adtte_f <- tern::tern_ex_adtte |>
  dplyr::filter(PARAMCD == "OS") |>
  dplyr::mutate(
    AVAL = tern::day2month(AVAL),
    is_event = CNSR == 0
  )

basic_table() |>
  split_cols_by(var = "ARMCD") |>
  analyze(
    vars = "AVAL",
    afun = a_event_free,
    show_labels = "hidden",
    na_str = tern::default_na_str(),
    extra_args = list(
      time_unit = "week",
      time_point = 3,
      is_event = "is_event"
    )
  ) |>
  build_table(df = adtte_f)
#>                                             ARM A               ARM B               ARM C      
#> ———————————————————————————————————————————————————————————————————————————————————————————————
#> Patients remaining at risk                   60                  65                  50        
#> Event Free Rate (%)                         92.64               89.04               86.21      
#>   Standard Error of Event Free Rate         3.17                3.66                4.53       
#>   95% CI                               (86.42, 98.85)      (81.88, 96.21)      (77.33, 95.08)  
#> 3-week event-free rate (95% CI)       0.93 (0.86, 0.99)   0.89 (0.82, 0.96)   0.86 (0.77, 0.95)
adtte_f <- tern::tern_ex_adtte |>
  dplyr::filter(PARAMCD == "OS") |>
  dplyr::mutate(
    AVAL = tern::day2month(AVAL),
    is_event = CNSR == 0
  )

s_event_free(
  df = adtte_f,
  .var = "AVAL",
  time_point = 6,
  is_event = "is_event",
  time_unit = "month"
)
#> $pt_at_risk
#> [1] 159
#> attr(,"label")
#> [1] "Patients remaining at risk"
#> 
#> $event_free_rate
#> [1] 82.21113
#> attr(,"label")
#> [1] "Event Free Rate (%)"
#> 
#> $rate_se
#> [1] 2.72844
#> attr(,"label")
#> [1] "Standard Error of Event Free Rate"
#> 
#> $rate_ci
#> [1] 76.86349 87.55878
#> attr(,"label")
#> [1] "95% CI"
#> 
#> $event_free_rate_3d
#> [1] 82.21113 76.86349 87.55878
#> attr(,"label")
#> [1] "Event Free Rate (95% CI)"
#> 
#> $event_free_ci
#> [1] 0.8221113 0.7686349 0.8755878
#> attr(,"label")
#> [1] "6-month event-free rate (95% CI)"
#> 
```
