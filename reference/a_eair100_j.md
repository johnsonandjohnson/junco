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
  fup_var,
  occ_var,
  occ_dy,
  num_p_year = 100,
  count_multiple_events = FALSE,
  conf_level = 0.95,
  conf_type_diff = c("wald", "waldcc", "mn", "scas"),
  conf_type_eair = c("exact", "normal", "normal_log", "byar"),
  vs_ref_group = FALSE,
  .in_ref_col,
  cur_col_expr,
  ref_col_expr,
  inriskdiffcol = FALSE
)

a_eair100_j(
  df,
  labelstr = NULL,
  .var,
  .df_row,
  .spl_context,
  .alt_df_full = NULL,
  id = "USUBJID",
  fup_var,
  occ_var,
  occ_dy = NULL,
  num_p_year = 100,
  count_multiple_events = FALSE,
  drop_levels = FALSE,
  riskdiff = TRUE,
  ref_path = NULL,
  .stats = c("eair"),
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  na_str = rep("NA", 3),
  conf_level = 0.95,
  conf_type_diff = c("wald", "waldcc", "mn", "scas"),
  conf_type_eair = c("normal", "normal_log", "exact", "byar"),
  row_labels_adj = FALSE,
  riskdiff_setup = c("horizontal", "vertical")
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
  variable name that is passed by `rtables`.

- .alt_df_full:

  (`dataframe`)  
  alternative dataset for calculations.

- id:

  (`string`)  
  subject variable name.

- fup_var:

  (`string`)  
  name of the variable containing the total follow-up duration for each
  subject, expressed in **years**. Used as the at-risk exposure time for
  subjects who did **not** experience the event.

- occ_var:

  (`string` or `NULL`)  
  name of the flag variable identifying the **first** occurrence of the
  event within each subject and level, encoded as `"Y"`. Only records
  where `occ_var == "Y"` contribute to the event count (`n_event`), and
  only one event record per subject is allowed.  
  If `NULL`, event occurrence filtering is skipped and time at risk is
  taken from `fup_var` in `alt_counts_df` for all subjects.

- occ_dy:

  (`string` or `NULL`)  
  When `occ_var` not `NULL`: name of the variable containing the
  relative day of the first event occurrence, expressed in **days**
  (e.g., `ASTDY`). For subjects with an event (`occ_var == "Y"`), this
  value is converted to years as `occ_dy / 365.25` and used as the
  at-risk exposure time instead of `fup_var`. Must be on the same time
  origin as `fup_var` (i.e., relative to start of exposure).

- num_p_year:

  (`numeric`)  
  scaling factor for the incidence rate, representing the number of
  person-years used as the denominator base. The EAIR is calculated as
  `num_p_year * n_event / total_person_years`. Defaults to `100`,
  yielding a rate per 100 person-years. For example, use `1` for a rate
  per person-year or `1000` for a rate per 1000 person-years.

- count_multiple_events:

  (`logical`)  
  if `TRUE`, counts the total number of events per subject rather than
  treating each subject as contributing 1 to `n_event`. Only considered
  when `occ_var = NULL`. Defaults to `FALSE`.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- conf_type_diff:

  (`string`)  
  method used to compute the confidence interval for the EAIR
  difference. One of `"wald"` (Wald without continuity correction),
  `"waldcc"` (Wald with continuity correction), `"mn"`
  (Miettinen-Nurminen score), or `"scas"` (skewness-corrected asymptotic
  score). Defaults to `"wald"`.

- conf_type_eair:

  (`string`)  
  method used to compute the confidence interval for the EAIR rate. One
  of `"exact"`, `"normal"`, `"normal_log"`, `"byar"`  
  See `?tern:::h_incidence_rate` and
  [`?tern::incidence_rate`](https://pharmaverse.github.io/tern/latest-tag/reference/incidence_rate.html),
  argument `control` for more information. Defaults to `"exact"`.

- vs_ref_group:

  (`logical`)  
  if TRUE, risk difference (against reference group) calculations will
  be performed.

- cur_col_expr:

  (`expression`)  
  current column expression taken from `rtables` `.spl_context`
  splitting machinery (`cur_col_expr`).

- ref_col_expr:

  (`expression` or `NULL`)  
  column expression for the reference group. Required to be an
  expression if `vs_ref_group` is `TRUE`

- inriskdiffcol:

  (`logical`)  
  flag indicating if the function is called within a risk difference
  column.

- labelstr:

  (`string`)  
  Not intended to be set by user, only present to be able to use
  function as a `cfun`.

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

- row_labels_adj:

  (`logical`)  
  If `TRUE` and number of requested statistics = 1, the row label on the
  table will reflect the level of the incoming variable rather than the
  label for the statistic.  
  Set to default `TRUE` for backward-compatibility reasons.

- riskdiff_setup:

  (`string`)  
  Are risk differences presented in horizontal way, or vertical way.  
  When horizontal, the statistics for `eair`, `n_eair`, `eair_n` will be
  replaced by `ear_diff_est_ci` in the risk difference columns. If
  `.stats` include any of difference in eair rates statistics, these
  will be removed as `ear_diff_est_ci` will be displayed in the risk
  difference columns

## Value

- `s_eair100_levii_j()` returns a list containing the following
  statistics:

  - n_event: Number of subjects with the event, or total event count if
    `count_multiple_events = TRUE`

  - person_years: Total person-years of follow-up

  - eair: Exposure-adjusted incidence rate per `num_p_year` person-years

  - n_eair: Combination of `n_event` and `eair` statistics.

  - eair_n: Combination of `eair` and `n_event` statistics.

  - eair_n_py: Combination of `eair`, `n_event` and `person_years`
    statistics.

  - eair_ci: Confidence interval for `eair`, method specified in
    `conf_type_eair`.

  - eair_est_ci: Combination of `eair` and `eair_ci`.

  - eair_diff_est: Difference in EAIR between current group and
    reference group (if `diff`=TRUE and `inriskdiffcol`=TRUE)

  - eair_diff_ci: Confidence interval for difference in EAIR between
    current group and reference group (if `diff`=TRUE and
    `inriskdiffcol`=TRUE)

  - eair_diff_est_ci: Combination of `eair_diff_est` and `eair_diff_ci`.

  The list of available statistics (core columns) can also be viewed by
  running `junco_get_stats("a_eair100_j")`.

&nbsp;

- `a_eair100_j` returns the corresponding list with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).  
  Within a risk difference column, the statistics `eair` and `n_eair`
  are replaced by `eair_diff_est_ci` (difference in EAIR between current
  group and reference group, together with it's confidence interval).

## Details

The exposure-adjusted incidence rate (EAIR) is defined as:

- When (`occ_var` not `NULL`) or `count_multiple_events = FALSE`:  
  the **number of subjects** with at least one occurrence of a specified
  adverse event divided by the total at-risk exposure time across all
  subjects.

- When `occ_var = NULL` and `count_multiple_events = TRUE`:  
  the total **number of events** across of a specified adverse event
  divided by the total at-risk exposure time across all subjects.

Total at-risk exposure time is calculated for each subject as:

- When `occ_var` not `NULL`

  - for subjects with the event: relative day (from the start of
    exposure) of the onset date of the **first** occurrence of the
    specified event (`occ_var` and `occ_dy`), transformed from days into
    years as `occ_dy/365.25`, or

  - for subjects without the event: total duration of follow-up
    (expressed in years) (e.g., treatment discontinuation, completion,
    or censoring) (`fup_var`).

- When `occ_var = NULL`  
  for all subjects: total duration of follow-up (expressed in years)
  (e.g., treatment discontinuation, completion, or censoring)
  (`fup_var`).

The EAIR is expressed as:

\$\$ \mathrm{EAIR} = \hat r = \frac{n}{T} \$\$

where:

- \\n\\ is the number of subjects with at least one occurrence of the
  event

- T = \\ \sum\_{i=1}^{N} t_i\\, with

- \\t_i\\ is the at-risk exposure time for subject \\i\\, expressed in
  subject-years

The difference between two EAIRs is \\\hat \theta = (\hat r_1 - \hat
r_2)\\.

Wald's method for the confidence interval approximation is used (Liu et
al., 2006), which uses the standard error \$\$ \mathrm{se}(\hat r_1 -
\hat r_2) = \sqrt{\frac{n_1}{T_1^2} + \frac{n_2}{T_2^2}} \$\$ and the
\\100(1-\alpha)\\\\ confidence interval is \$\$ (\hat r_1 - \hat r_2)
\pm z\_{1-\alpha/2}\\\mathrm{se}(\hat r_1 - \hat r_2) \$\$ where
\\z\_{1-\alpha/2}\\ is the standard normal quantile.
[1](https://metricgate.com/docs/poisson-rate-comparison/)  
  
Other methods for confidence interval for difference can be specified in
`conf_type_diff` argument.

EAIR is commonly reported per 100 subject-years:

\$\$ \mathrm{EAIR}\_{100} = 100 \times \hat r \$\$

## Functions

- `s_eair100_levii_j()`: calculates exposure-adjusted incidence rates
  (EAIR) per `num_p_year` person-years for a specific level of a
  variable.

- `a_eair100_j()`: Formatted analysis function for exposure adjusted
  incidence rate summary which is used as `afun` in `analyze` or `cfun`
  in `summarize_row_groups`.  

## References

Liu, G. F., Wang, J., Liu, K., and Snavely, D. B. (2006). Confidence
intervals for an exposure adjusted incidence rate difference with
applications to clinical trials. *Statistics in Medicine*, **25**(8),
1275–1286. [doi:10.1002/sim.2335](https://doi.org/10.1002/sim.2335)

## Examples

``` r
library(tern)
library(dplyr)
trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
cutoffd <- as.Date("2023-09-24")


adexsum <- ex_adsl |>
  create_colspan_var(
    non_active_grp          = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl     = "Active Study Agent",
    colspan_var             = "colspan_trt",
    trt_var                 = trtvar
  ) |>
  mutate(
    rrisk_header = "Risk Difference (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs", ctrl_grp),
    TRTDURY = case_when(
      !is.na(EOSDY) ~ EOSDY,
      TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
    )
  ) |>
  select(USUBJID, !!rlang::sym(trtvar), colspan_trt, rrisk_header, rrisk_label, TRTDURY)

adexsum$TRTDURY <- as.numeric(adexsum$TRTDURY)

adae <- ex_adae |>
  group_by(USUBJID, AEDECOD) |>
  select(USUBJID, AEDECOD, ASTDY) |>
  mutate(rwnum = row_number()) |>
  mutate(AOCCPFL = case_when(
    rwnum == 1 ~ "Y",
    TRUE ~ NA
  )) |>
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

################################################################################
# Define layout and build table:
################################################################################

lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx", top_level_section_div = " ") |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
  split_cols_by(trtvar) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) |>
  analyze("TRTDURY",
    show_labels = "hidden",
    afun = a_patyrs_j
  ) |>
  analyze(
    vars = "AEDECOD",
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
#>                                     Active Study Agent                                   Risk Difference (95% CI)               
#>                                 A: Drug X   C: Combination   B: Placebo   A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#>                                   N=134         N=132          N=134               N=134                       N=132            
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Patient years                    96200.0       91753.0        91431.0                                                           
#> AEDECOD                                                                                                                         
#>   eair (per 100 person years)      0.1           0.1            0.1         0.00 (-0.03, 0.04)           0.05 (0.01, 0.09)      
#>   eair (per 100 person years)      0.1           0.1            0.1         0.00 (-0.03, 0.03)           0.01 (-0.02, 0.04)     
#>   eair (per 100 person years)      0.1           0.1            0.1         -0.01 (-0.04, 0.02)         -0.02 (-0.05, 0.02)     
```
