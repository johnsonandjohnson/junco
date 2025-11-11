# Exposure-Adjusted Incidence Rate

Statistical/Analysis Function for presenting Exposure-Adjusted Incidence
Rate summary data

## Usage

``` r
s_eair100_levii_j(
  levii,
  df,
  .df_row,
  .var,
  .alt_df_full = NULL,
  id = "USUBJID",
  diff = FALSE,
  conf_level = 0.95,
  trt_var = NULL,
  ctrl_grp = NULL,
  cur_trt_grp = NULL,
  inriskdiffcol = FALSE,
  fup_var,
  occ_var,
  occ_dy
)

a_eair100_j(
  df,
  labelstr = NULL,
  .var,
  .df_row,
  .spl_context,
  .alt_df_full = NULL,
  id = "USUBJID",
  drop_levels = FALSE,
  riskdiff = TRUE,
  ref_path = NULL,
  .stats = c("eair"),
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  na_str = rep("NA", 3),
  conf_level = 0.95,
  fup_var,
  occ_var,
  occ_dy
)
```

## Arguments

- levii:

  (`string`)  
  the specific level of the variable to calculate EAIR for.

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .var:

  (`string`)  
  variable name for analysis.

- .alt_df_full:

  (`dataframe`)  
  denominator dataset for calculations.

- id:

  (`string`)  
  subject variable name.

- diff:

  (`logical`)  
  if TRUE, risk difference calculations will be performed.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- trt_var:

  (`string`)  
  treatment variable name.

- ctrl_grp:

  (`string`)  
  control group value.

- cur_trt_grp:

  (`string`)  
  current treatment group value.

- inriskdiffcol:

  (`logical`)  
  flag indicating if the function is called within a risk difference
  column.

- fup_var:

  (`string`)  
  variable name for follow-up time.

- occ_var:

  (`string`)  
  variable name for occurrence.

- occ_dy:

  (`string`)  
  variable name for occurrence day.

- labelstr:

  (`string`)  
  label string for the row.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states.

- drop_levels:

  (`logical`)  
  if TRUE, non-observed levels will not be included.

- riskdiff:

  (`logical`)  
  if TRUE, risk difference calculations will be performed.

- ref_path:

  (`string`)  
  column path specifications for the control group.

- .stats:

  (`character`)  
  statistics to select for the table.

- .formats:

  (named 'character' or 'list')  
  formats for the statistics.

- .labels:

  (named 'character')  
  labels for the statistics.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels.

- na_str:

  (`string`)  
  string used to replace all NA or empty values in the output.

## Value

- `s_eair100_levii_j()` returns a list containing the following
  statistics:

  - n_event: Number of events

  - person_years: Total person-years of follow-up

  - eair: Exposure-adjusted incidence rate per 100 person-years

  - eair_diff: Risk difference in EAIR (if diff=TRUE and
    inriskdiffcol=TRUE)

  - eair_diff_ci: Confidence interval for the risk difference (if
    diff=TRUE and inriskdiffcol=TRUE)

  .  
  The list of available statistics (core columns) can also be viewed by
  running `junco_get_stats("a_eair100_j")`

&nbsp;

- `a_eair100_j` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).

## Functions

- `s_eair100_levii_j()`: calculates exposure-adjusted incidence rates
  (EAIR) per 100 person-years for a specific level of a variable.

- `a_eair100_j()`: Formatted analysis function for exposure adjusted
  incidence rate summary which is used as `afun` in `analyze` or `cfun`
  in `summarize_row_groups`.

## Examples

``` r
library(tern)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
cutoffd <- as.Date("2023-09-24")


adexsum <- ex_adsl %>%
  create_colspan_var(
    non_active_grp          = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl     = "Active Study Agent",
    colspan_var             = "colspan_trt",
    trt_var                 = trtvar
  ) %>%
  mutate(
    rrisk_header = "Risk Difference (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
    TRTDURY = case_when(
      !is.na(EOSDY) ~ EOSDY,
      TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
    )
  ) %>%
  select(USUBJID, !!rlang::sym(trtvar), colspan_trt, rrisk_header, rrisk_label, TRTDURY)

adexsum$TRTDURY <- as.numeric(adexsum$TRTDURY)

adae <- ex_adae %>%
  group_by(USUBJID, AEDECOD) %>%
  select(USUBJID, AEDECOD, ASTDY) %>%
  mutate(rwnum = row_number()) %>%
  mutate(AOCCPFL = case_when(
    rwnum == 1 ~ "Y",
    TRUE ~ NA
  )) %>%
  filter(AOCCPFL == "Y")

aefup <- left_join(adae, adexsum, by = "USUBJID")

colspan_trt_map <- create_colspan_map(adexsum,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)


lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx", top_level_section_div = " ") %>%
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>%
  split_cols_by(trtvar) %>%
  split_cols_by("rrisk_header", nested = FALSE) %>%
  split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) %>%
  analyze("TRTDURY",
    nested = FALSE,
    show_labels = "hidden",
    afun = a_patyrs_j
  ) %>%
  analyze(
    vars = "AEDECOD",
    nested = FALSE,
    afun = a_eair100_j,
    extra_args = list(
      fup_var = "TRTDURY",
      occ_var = "AOCCPFL",
      occ_dy = "ASTDY",
      ref_path = ref_path,
      drop_levels = TRUE
    )
  )

result <- build_table(lyt, aefup, alt_counts_df = adexsum)
head(result, 5)
#>                     Active Study Agent                                   Risk Difference (95% CI)               
#>                 A: Drug X   C: Combination   B: Placebo   A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#>                   N=134         N=132          N=134               N=134                       N=132            
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Patient years    96200.0       91753.0        91431.0                                                           
#>                                                                                                                 
#> dcd A.1.1.1.1      0.1           0.1            0.1         0.00 (-0.03, 0.04)           0.05 (0.01, 0.09)      
#> dcd A.1.1.1.2      0.1           0.1            0.1         0.00 (-0.03, 0.03)           0.01 (-0.02, 0.04)     
#> dcd B.1.1.1.1      0.1           0.1            0.1         -0.01 (-0.04, 0.02)         -0.02 (-0.05, 0.02)     
#> dcd B.2.1.2.1      0.1           0.1            0.1         0.01 (-0.02, 0.05)           0.03 (-0.01, 0.06)     
```
