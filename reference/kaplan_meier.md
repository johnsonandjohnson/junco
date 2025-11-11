# Survival time analysis

**\[stable\]**

The analyze function `kaplan_meier()` creates a layout element to
analyze survival time by calculating survival time median, 2 quantiles,
each with their confidence intervals, and range (for all, censored, or
event patients). The primary analysis variable `vars` is the time
variable and the secondary analysis variable `is_event` indicates
whether or not an event has occurred.

## Usage

``` r
a_kaplan_meier(
  df,
  .var,
  ...,
  .stats = NULL,
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL
)

s_kaplan_meier(df, .var, is_event, control = control_surv_time())
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

- is_event:

  (`character`)  
  variable name storing Logical values: `TRUE` if event, `FALSE` if time
  to event is censored.

- control:

  (`list`)  
  parameters for comparison details, specified by using the helper
  function
  [`tern::control_surv_time()`](https://insightsengineering.github.io/tern/latest-tag/reference/control_surv_time.html).
  Some possible parameter options are:

  - `conf_level` (`proportion`)  
    confidence level of the interval for survival time.

  - `conf_type` (`string`)  
    confidence interval type. Options are 'plain' (default), 'log', or
    'log-log', see more in
    [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).
    Note option 'none' is not supported.

  - `quantiles` (`numeric`)  
    vector of length two to specify the quantiles of survival time.

## Value

- `a_kaplan_meier()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

&nbsp;

- `s_kaplan_meier()` returns the following statistics:

  - `quantiles_lower`: Lower quantile estimate and confidence interval
    for it.

  - `median_ci_3d`: Median survival time and confidence interval for it.

  - `quantiles_upper`: Upper quantile estimate and confidence interval
    for it.

  - `range_with_cens_info`: Survival time range with censoring
    information.

## Functions

- `a_kaplan_meier()`: Formatted analysis function which is used as
  `afun`

- `s_kaplan_meier()`: Statistics function which analyzes survival times
  using Kaplan-Meier.

## Note

These functions have been forked from the `tern` package file
`survival_time.R`. Here we have the additional features:

- Additional statistics `quantiles_lower`, `quantiles_upper`,
  `range_with_cens_info` are returned.

## Examples

``` r
library(dplyr)
library(tern)
adtte_f <- tern::tern_ex_adtte |>
  filter(PARAMCD == "OS") |>
  mutate(
    AVAL = tern::day2month(AVAL),
    is_event = CNSR == 0
  )
df <- adtte_f |> filter(ARMCD == "ARM A")
a_kaplan_meier(
  df,
  .var = "AVAL",
  is_event = "is_event"
)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>               row_name        formatted_cell indent_mod
#> 1      quantiles_lower 17.37 (10.13 - 22.51)          0
#> 2         median_ci_3d  32.02 (22.51, 49.31)          0
#> 3      quantiles_upper  65.28 (49.31, 87.21)          0
#> 4 range_with_cens_info        (0.34, 155.50)          0
#>                  row_label
#> 1 25th percentile (95% CI)
#> 2          Median (95% CI)
#> 3 75th percentile (95% CI)
#> 4                 Min, max

basic_table() |>
  split_cols_by(var = "ARMCD") |>
  add_colcounts() |>
  analyze(
    vars = "AVAL",
    afun = a_kaplan_meier,
    var_labels = "Kaplan-Meier estimate of time to event (months)",
    show_labels = "visible",
    extra_args = list(
      is_event = "is_event",
      control = control_surv_time(conf_level = 0.9, conf_type = "log-log")
    )
  ) |>
  build_table(df = adtte_f)
#>                                                           ARM A                  ARM B                  ARM C        
#>                                                          (N=69)                  (N=73)                 (N=58)       
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Kaplan-Meier estimate of time to event (months)                                                                      
#>   25th percentile (90% CI)                        17.37 (10.13 - 21.48)   9.82 (4.81 - 13.59)    7.31 (3.53 - 11.84) 
#>   Median (90% CI)                                 32.02 (22.57, 46.51)    23.91 (18.26, 29.19)   20.77 (12.91, 25.86)
#>   75th percentile (90% CI)                        65.28 (49.31, 87.21)    41.98 (37.97, 53.41)   37.10 (25.86, 47.60)
#>   Min, max                                           (0.34, 155.50)          (0.07, 154.09)         (0.63, 80.69)    
```
