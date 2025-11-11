# Creation of Column Spanning Variables

A function used for creating a spanning variable for treatment groups

## Usage

``` r
create_colspan_var(
  df,
  non_active_grp = c("Placebo"),
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
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

## Value

a data frame that contains the new variable as specified in colspan_var

## Details

This function creates a spanning variable for treatment groups that is
intended to be used within the column space.

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

colspan_var <- create_colspan_var(
  df = df,
  non_active_grp = c("Placebo"),
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Treatment",
  colspan_var = "colspan_trt",
  trt_var = "TRT01A"
)

colspan_var
#> # A tibble: 3 × 2
#>   TRT01A   colspan_trt       
#>   <fct>    <fct>             
#> 1 Placebo  " "               
#> 2 Active 1 "Active Treatment"
#> 3 Active 2 "Active Treatment"
```
