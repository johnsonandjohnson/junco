# Analysis Function for Response Variables

This function calculates counts and percentages for response variables
(Y/N values), with optional risk difference calculations.

## Usage

``` r
a_freq_resp_var_j(
  df,
  .var,
  .df_row,
  .N_col,
  .spl_context,
  resp_var = NULL,
  id = "USUBJID",
  drop_levels = FALSE,
  riskdiff = TRUE,
  ref_path = NULL,
  variables = formals(s_proportion_diff)$variables,
  conf_level = formals(s_proportion_diff)$conf_level,
  method = c("wald", "waldcc", "cmh", "ha", "newcombe", "newcombecc", "strat_newcombe",
    "strat_newcombecc"),
  weights_method = formals(s_proportion_diff)$weights_method,
  ...
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .var:

  (`string`)  
  variable name that is passed by `rtables`.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .N_col:

  (`integer`)  
  column-wise N (column count) for the full column being analyzed.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states.

- resp_var:

  (`string`)  
  response variable name containing Y/N values.

- id:

  (`string`)  
  subject variable name.

- drop_levels:

  (`logical`)  
  if TRUE, non-observed levels will not be included.

- riskdiff:

  (`logical`)  
  if TRUE, risk difference calculations will be performed.

- ref_path:

  (`string`)  
  column path specifications for the control group.

- variables:

  (`list`)  
  variables to include in the analysis.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`character`)  
  method for calculating confidence intervals.

- weights_method:

  (`character`)  
  method for calculating weights.

- ...:

  Additional arguments passed to other functions.

## Value

Formatted analysis function which is used as `afun` in
[`analyze_vars()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html)
and as `cfun` in
[`summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html).

## Examples

``` r
library(dplyr)
ADSL <- ex_adsl |> select(USUBJID, ARM, SEX)

ADAE <- ex_adae |> select(USUBJID, ARM, SEX, AEBODSYS, AEDECOD)

ADAE <- ADAE |>
  mutate(TRTEMFL = "Y")

lyt <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("ARM") |>
  analyze("SEX",
    show_labels = "visible",
    afun = a_freq_resp_var_j,
    extra_args = list(resp_var = "TRTEMFL", riskdiff = FALSE)
  )

result <- build_table(lyt, df = ADAE, alt_counts_df = ADSL)

result
#>                        A: Drug X        B: Placebo     C: Combination
#>                         (N=134)          (N=134)          (N=132)    
#> —————————————————————————————————————————————————————————————————————
#> SEX                                                                  
#>   F                  72/72 (100.0%)   73/73 (100.0%)   61/61 (100.0%)
#>   M                  46/46 (100.0%)   48/48 (100.0%)   53/53 (100.0%)
#>   U                   3/3 (100.0%)     2/2 (100.0%)     4/4 (100.0%) 
#>   UNDIFFERENTIATED    1/1 (100.0%)          -           2/2 (100.0%) 
```
