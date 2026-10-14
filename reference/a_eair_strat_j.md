# Stratified Exposure-Adjusted Incidence Rate (EAIR)

Statistical/Analysis function for presenting a stratified
exposure-adjusted incidence rate (EAIR) and its Wald confidence interval
across strata (e.g., study size weighted exposure).

This function is an extension to the unstratified function
[`a_eair100_j()`](https://johnsonandjohnson.github.io/junco/reference/a_eair100_j.md).

## Usage

``` r
s_eair_strat_levii_j(
  levii,
  df,
  .df_row,
  .alt_df_full,
  .var,
  strata,
  id = "USUBJID",
  fup_var,
  occ_var = NULL,
  occ_dy = NULL,
  num_p_year = 100,
  count_multiple_events = FALSE,
  conf_level = 0.95,
  vs_ref_group = FALSE,
  .in_ref_col,
  cur_col_expr,
  ref_col_expr,
  inriskdiffcol = FALSE
)

a_eair_strat_j(
  df,
  labelstr = NULL,
  .var,
  .df_row,
  .spl_context,
  .alt_df_full = NULL,
  id = "USUBJID",
  strata,
  fup_var,
  occ_var = NULL,
  occ_dy = NULL,
  num_p_year = 100,
  count_multiple_events = FALSE,
  drop_levels = FALSE,
  riskdiff = FALSE,
  ref_path = NULL,
  conf_level = 0.95,
  .stats = "eair_strat",
  .formats = NULL,
  .labels = NULL,
  .indent_mods = NULL,
  na_str = rep("NA", 3),
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

- .alt_df_full:

  (`dataframe`)  
  alternative dataset for calculations.

- .var:

  (`string`)  
  variable name that is passed by `rtables`.

- strata:

  (`string`)  
  variable identifying strata (e.g., study identifier).

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

- `s_eair_strat_levii_j()` returns a list containing the following
  statistics:

&nbsp;

- `n_event_total`: total event count across strata (current arm)

- `person_years_total`: total person-years across strata (current arm)

- `eair_strat`: stratified pooled EAIR per `num_p_year` person-years

- `n_eair_strat`: composite of `n_event_total` and `eair_strat`

- `eair_strat_ci`: Wald CI for `eair_strat`

- `eair_strat_est_ci`: composite of `eair_strat` and `eair_strat_ci`

- `eair_strat_n`: composite of `eair_strat` and `n_event_total`

- `eair_strat_n_py`: composite of `eair_strat`, `n_event_total` and
  `person_years_total`

- `eair_strat_diff_est_ci`: stratified rate difference with Wald CI
  (only when `vs_ref_group = TRUE`, otherwise `NULL`)

- `eair_strat_diff_est`: estimate for the difference in stratified rate

- `eair_strat_diff_ci`: CI (Wald type) for the difference in stratified
  rate

The list of available statistics (core columns) can also be viewed by
running `junco_get_stats("a_eair_strat_j")`.

- `a_eair_strat_j()` returns a list of formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html)
  rows. Within a risk difference column, the statistics `eair_strat` and
  `n_eair_strat` are replaced by `eair_strat_diff_est_ci` (difference in
  EAIR between current group and reference group, together with it's
  confidence interval).

## Details

The weighting method uses raw event counts `n_ti` in both the rate and
variance. \$\$\hat r\_{ti} = \texttt{num\\p\\year} \times
\frac{n\_{ti}}{T\_{ti}}\$\$

The strata weight is `w_i = T_0i + T_1i` (total person-years across
**all** groups in strata `i`), and the pooled weighted rate is: \$\$\hat
r_t = \sum_i \frac{w_i}{W} \hat r\_{ti}, \quad W = \sum_i w_i\$\$

The Wald 95% CI for the pooled rate is: \$\$\hat r_t \pm z\_{1-\alpha/2}
\times \frac{\texttt{num\\p\\year}}{W} \sqrt{\sum_i w_i^2
\frac{n\_{ti}}{T\_{ti}^2}}\$\$.

The weighted rate difference and its Wald CI are: \$\$\hat\delta =
\sum_i \frac{w_i}{W}(\hat r\_{1i} - \hat r\_{0i})\$\$ \$\$\hat\delta \pm
z\_{1-\alpha/2} \times \frac{\texttt{num\\p\\year}}{W} \sqrt{\sum_i
w_i^2 \left(\frac{n\_{1i}+c}{T\_{1i}^2} +
\frac{n\_{0i}}{T\_{0i}^2}\right)}\$\$

## Functions

- `a_eair_strat_j()`: Formatted analysis function for stratified (eg
  study size-weighted) EAIR, used as `afun` in
  [`rtables::analyze()`](https://insightsengineering.github.io/rtables/latest-tag/reference/analyze.html).

## References

Scosyrev, E & Pethe, A (2022). Confidence intervals for
exposure-adjusted rate differences in randomized trials. Pharmaceutical
Statistics, 2022;21:103–121. doi: 10.1002/pst.2155

Crowe, B., Chuang-Stein, C., Lettis, S., & Brueckner, A. (2016).
Reporting adverse drug reactions in product labels. Therapeutic
Innovation & Regulatory Science, 50(4), 455-463. doi:
10.1177/2168479016628574

## See also

[`a_eair100_j()`](https://johnsonandjohnson.github.io/junco/reference/a_eair100_j.md)

## Examples

``` r
library(tern)
library(dplyr)

set.seed(42)
n <- 120
adsl <- data.frame(
  USUBJID = paste0("S", seq_len(n)),
  ARM = rep(c("Treatment", "Placebo"), each = n / 2),
  STUDY = rep(c("Study1", "Study2", "Study3"), times = n / 3),
  TRTDURY = runif(n, 0.5, 3),
  stringsAsFactors = FALSE
)
adae <- data.frame(
  USUBJID = sample(adsl$USUBJID, 40),
  AEDECOD = "Headache",
  ASTDY = sample(30:500, 40, replace = TRUE),
  AOCCPFL = "Y",
  stringsAsFactors = FALSE
)
aefup <- merge(adae, adsl, by = "USUBJID")

# Standard method (matches rdcomaeinci SAS macro)
lyt <- basic_table() |>
  split_cols_by("ARM") |>
  analyze(
    "AEDECOD",
    afun = a_eair_strat_j,
    extra_args = list(
      strata = "STUDY",
      fup_var = "TRTDURY",
      occ_var = "AOCCPFL",
      occ_dy = "ASTDY",
      .stats = c("eair_strat", "eair_strat_ci")
    )
  )
result <- build_table(lyt, aefup, alt_counts_df = adsl)
result
#>                              Treatment         Placebo    
#> ——————————————————————————————————————————————————————————
#> Headache Stratified EAIR        19.9             27.8     
#> Headache 95% CI (Wald)     (10.94, 28.88)   (15.85, 39.69)
```
