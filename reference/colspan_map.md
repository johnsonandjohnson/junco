# Creation of Column Spanning Mapping Dataframe

A function used for creating a data frame containing the map that is
compatible with rtables split function `trim_levels_to_map`

## Usage

``` r
create_colspan_map(
  df,
  non_active_grp = c("Placebo"),
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A",
  active_first = TRUE
)
```

## Arguments

- df:

  The name of the data frame in which the spanning variable is to be
  appended to

- non_active_grp:

  The value(s) of the treatments that represent the non-active or
  comparator treatment groups default value = c('Placebo')

- non_active_grp_span_lbl:

  The assigned value of the spanning variable for the non-active or
  comparator treatment groups default value = ”

- active_grp_span_lbl:

  The assigned value of the spanning variable for the active treatment
  group(s) default value = 'Active Study Agent'

- colspan_var:

  The desired name of the newly created spanning variable default value
  = 'colspan_trt'

- trt_var:

  The name of the treatment variable that is used to determine which
  spanning treatment group value to apply. default value = 'TRT01A'

- active_first:

  whether the active columns come first.

## Value

a data frame that contains the map to be used with rtables split
function `trim_levels_to_map`

## Details

This function creates a data frame containing the map that is compatible
with rtables split function `trim_levels_to_map`. The levels of the
specified trt_var variable will be stored within the trt_var variable
and the colspan_var variable will contain the corresponding spanning
header value for each treatment group.

## Examples

``` r
library(tibble)

df <- tribble(
  ~TRT01A,
  "Placebo",
  "Active 1",
  "Active 2"
)

df$TRT01A <- factor(df$TRT01A, levels = c("Placebo", "Active 1", "Active 2"))

colspan_map <- create_colspan_map(
  df = df,
  non_active_grp = c("Placebo"),
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)

colspan_map
#>          colspan_trt   TRT01A
#> 1 Active Study Agent Active 1
#> 2 Active Study Agent Active 2
#> 3                     Placebo
```
