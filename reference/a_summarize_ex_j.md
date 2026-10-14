# Tabulation for Exposure Tables

A function to create the appropriate statistics needed for exposure
table

## Usage

``` r
s_summarize_ex_j(
  df,
  .var,
  .df_row,
  .spl_context,
  comp_btw_group = TRUE,
  ref_path = NULL,
  ancova = FALSE,
  interaction_y,
  interaction_item,
  conf_level,
  daysconv,
  variables
)

a_summarize_ex_j(
  df,
  .var,
  .df_row,
  .spl_context,
  comp_btw_group = TRUE,
  ref_path = NULL,
  ancova = FALSE,
  interaction_y = FALSE,
  interaction_item = NULL,
  conf_level = 0.95,
  variables,
  .stats = c("mean_sd", "median", "range", "quantiles", "total_subject_years"),
  .formats = c(diff_mean_est_ci = jjcsformat_xx("xx.xx (xx.xx, xx.xx)")),
  .labels = c(quantiles = "Interquartile range"),
  .indent_mods = NULL,
  na_str = rep("NA", 3),
  daysconv = 1
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

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- comp_btw_group:

  (`logical`)  
  If TRUE, comparison between groups will be performed.  
  When ancova = FALSE, the estimate of between group difference (on CHG)
  will be based upon two-sample t-test.  
  When ancova = TRUE, the same ANCOVA model will be used for the
  estimate of between group difference (on CHG).

- ref_path:

  (`character`)  
  global reference group specification, see
  [`get_ref_info()`](https://johnsonandjohnson.github.io/junco/reference/get_ref_info.md).

- ancova:

  (`logical`)  
  If FALSE, only descriptive methods will be used.  
  If TRUE, ANCOVA methods will be used for each of the columns : AVAL,
  CHG, DIFF.  

- interaction_y:

  (`character`)  
  Will be passed onto the `tern` function `s_ancova`, when ancova =
  TRUE.

- interaction_item:

  (`character`)  
  Will be passed onto the `tern` function `s_ancova`, when ancova =
  TRUE.

- conf_level:

  (`proportion`)  
  Confidence level of the interval

- daysconv:

  (`numeric`)  
  conversion required to get the values into days (i.e 1 if original
  PARAMCD unit is days, 30.4375 if original PARAMCD unit is in months)

- variables:

  (named list of strings)  
  list of additional analysis variables, with expected elements:

  - arm (string)  
    group variable, for which the covariate adjusted means of multiple
    groups will be summarized. Specifically, the first level of arm
    variable is taken as the reference group.

  - covariates (character)  
    a vector that can contain single variable names (such as 'X1'),
    and/or interaction terms indicated by 'X1 \* X2'.

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

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

## Value

- `a_summarize_ex_j()` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Details

Creates statistics needed for standard exposure table. This includes
differences and 95% CI and total treatment years. This is designed to be
used as an analysis (afun in `analyze`) function.

## Functions

- `s_summarize_ex_j()`: Statistics function needed for the exposure
  tables.

- `a_summarize_ex_j()`: Formatted analysis function which is used as
  `afun`.

## Examples

``` r
library(dplyr)
ADEX <- ex_adsl |> select(USUBJID, ARM, TRTSDTM, EOSSTT, EOSDY)

trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
cutoffd <- as.Date("2023-09-24")

ADEX <- ADEX |>
  create_colspan_var(
    non_active_grp          = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl     = "Active Study Agent",
    colspan_var             = "colspan_trt",
    trt_var                 = trtvar
  ) |>
  mutate(
    diff_header = "Difference in Means (95% CI)",
    diff_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
    TRTDURY = case_when(
      !is.na(EOSDY) ~ EOSDY,
      TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
    )
  )

colspan_trt_map <- create_colspan_map(ADEX,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", "", trtvar, ctrl_grp)

lyt <- basic_table() |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar) |>
  split_cols_by("diff_header", nested = FALSE) |>
  split_cols_by(
    trtvar,
    split_fun = remove_split_levels(ctrl_grp),
    labels_var = "diff_label"
  ) |>
  analyze("EOSDY",
    afun = a_summarize_ex_j, var_labels = "Duration of treatment (Days)",
    show_labels = "visible",
    indent_mod = 0L,
    extra_args = list(
      daysconv = 1,
      ref_path = ref_path,
      variables = list(arm = trtvar, covariates = NULL),
      ancova = TRUE,
      comp_btw_group = TRUE
    )
  )

result <- build_table(lyt, ADEX, alt_counts_df = ADEX)
result
#>                                             Active Study Agent                                            Difference in Means (95% CI)             
#>                                        A: Drug X        C: Combination       B: Placebo      A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Duration of treatment (Days)                                                                                                                       
#>   Mean (SD)                         599.84 (222.837)   604.51 (212.388)   586.04 (215.985)    13.81 (-43.91, 71.52)       18.48 (-38.71, 75.66)    
#>   Median                                 731.00             731.00             731.00                                                              
#>   Min, max                            11.0, 731.0         6.0, 731.0        26.0, 731.0                                                            
#>   Interquartile range                520.00, 731.00     497.00, 731.00     447.00, 731.00                                                          
#>   Total treatment (subject years)   64183.0 (175.7)    67101.0 (183.7)    65636.0 (179.7)                                                          
```
