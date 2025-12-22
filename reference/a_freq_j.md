# Analysis/statistical function for count and percentage in core columns and (optional) relative risk columns

Analysis/statistical function for count and percentage in core columns
and (optional) relative risk columns

## Usage

``` r
s_freq_j(
  df,
  .var,
  .df_row,
  val = NULL,
  drop_levels = FALSE,
  excl_levels = NULL,
  alt_df,
  parent_df,
  id = "USUBJID",
  denom = c("n_df", "n_altdf", "N_col", "n_rowdf", "n_parentdf"),
  .N_col,
  countsource = c("df", "altdf")
)

a_freq_j(
  df,
  labelstr = NULL,
  .var = NA,
  val = NULL,
  drop_levels = FALSE,
  excl_levels = NULL,
  new_levels = NULL,
  new_levels_after = FALSE,
  addstr2levs = NULL,
  .df_row,
  .spl_context,
  .N_col,
  id = "USUBJID",
  denom = c("N_col", "n_df", "n_altdf", "N_colgroup", "n_rowdf", "n_parentdf"),
  riskdiff = TRUE,
  ref_path = NULL,
  variables = list(strata = NULL),
  conf_level = 0.95,
  method = c("wald", "waldcc", "cmh", "ha", "newcombe", "newcombecc", "strat_newcombe",
    "strat_newcombecc"),
  weights_method = "cmh",
  label = NULL,
  label_fstr = NULL,
  label_map = NULL,
  .alt_df_full = NULL,
  denom_by = NULL,
  .stats = c("count_unique_denom_fraction"),
  .formats = NULL,
  .indent_mods = NULL,
  na_str = rep("NA", 3),
  .labels_n = NULL,
  extrablankline = FALSE,
  extrablanklineafter = NULL,
  restr_columns = NULL,
  colgroup = NULL,
  countsource = c("df", "altdf")
)

a_freq_j_with_exclude(
  df,
  labelstr = NULL,
  exclude_levels,
  .var = NA,
  .spl_context,
  .df_row,
  .N_col,
  .alt_df_full = NULL,
  .stats = "count_unique_denom_fraction",
  .formats = NULL,
  .indent_mods = NULL,
  .labels_n = NULL,
  ...
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

- val:

  (`character` or NULL)  
  When NULL, all levels of the incoming variable (variable used in the
  `analyze` call) will be considered.  
  When a single `string`, only that current level/value of the incoming
  variable will be considered.  
  When multiple levels, only those levels/values of the incoming
  variable will be considered.  
  When no values are observed (eg zero row input df), a row with
  row-label `No data reported` will be included in the table.

- drop_levels:

  (`logical`)  
  If `TRUE` non-observed levels (based upon .df_row) will not be
  included.  
  Cannot be used together with `val`.

- excl_levels:

  (`character` or NULL)  
  When NULL, no levels of the incoming variable (variable used in the
  `analyze` call) will be excluded.  
  When multiple levels, those levels/values of the incoming variable
  will be excluded.  
  Cannot be used together with `val`.

- alt_df:

  (`dataframe`)  
  Will be derived based upon alt_df_full and denom_by within a_freq_j.

- parent_df:

  (`dataframe`)  
  Will be derived within a_freq_j based upon the input dataframe that
  goes into build_table (df) and denom_by.  
  It is a data frame in the higher row-space than the current input df
  (which underwent row-splitting by the rtables splitting machinery).

- id:

  (`string`)  
  subject variable name.

- denom:

  (`string`)  
  See Details.

- .N_col:

  (`integer`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- countsource:

  Either `df` or `alt_df`.  
  When `alt_df` the counts will be based upon the alternative dataframe
  `alt_df`.  
  This is useful for subgroup processing, to present counts of subjects
  in a subgroup from the alternative dataframe.

- labelstr:

  An argument to ensure this function can be used as a `cfun` in a
  `summarize_row_groups` call.  
  It is recommended not to utilize this argument for other purposes.  
  The label argument could be used instead (if `val` is a single
  string)  
  An another approach could be to utilize the `label_map` argument to
  control the row labels of the incoming analysis variable.

- new_levels:

  (list(2) or NULL)  
  List of length 2.  
  First element : names of the new levels  
  Second element: list with values of the new levels.  

- new_levels_after:

  (`logical`)  
  If `TRUE` new levels will be added after last level.

- addstr2levs:

  string, if not NULL will be appended to the rowlabel for that level,
  eg to add ",n (percent)" at the end of the rowlabels

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- riskdiff:

  (`logical`)  
  When `TRUE`, risk difference calculations will be performed and
  presented (if required risk difference column splits are included).  
  When `FALSE`, risk difference columns will remain blank (if required
  risk difference column splits are included).

- ref_path:

  (`string`)  
  Column path specifications for the control group for the relative risk
  derivation.

- variables:

  Will be passed onto the relative risk function (internal function
  s_rel_risk_val_j), which is based upon
  [`tern::s_proportion_diff()`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff.html).  
  See
  [`?tern::s_proportion_diff`](https://insightsengineering.github.io/tern/latest-tag/reference/prop_diff.html)
  for details.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  Will be passed onto the relative risk function (internal function
  s_rel_risk_val_j).  

- weights_method:

  Will be passed onto the relative risk function (internal function
  s_rel_risk_val_j).  

- label:

  (`string`)  
  When `val` has length 1, the row label to be shown on the output can
  be specified using this argument.  
  When `val` is a `character vector`, the `label_map` argument can be
  specified to control the row-labels.

- label_fstr:

  (`string`)  
  a sprintf style format string. It can contain up to one `"%s"`, which
  takes the current split value and generates the row/column label.  
  It will be combined with the `labelstr` argument, when utilizing this
  function as a `cfun` in a `summarize_row_groups` call.  
  It is recommended not to utilize this argument for other purposes. The
  label argument could be used instead (if `val` is a single string)  

- label_map:

  (`tibble`)  
  A mapping tibble to translate levels from the incoming variable into a
  different row label to be presented on the table.  

- .alt_df_full:

  (`dataframe`)  
  Denominator dataset for fraction and relative risk calculations.  
  this argument gets populated by the rtables split machinery (see
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html)).

- denom_by:

  (`character`)  
  Variables from row-split to be used in the denominator derivation.  
  This controls both `denom = "n_parentdf"` and `denom = "n_altdf"`.  
  When `denom = "n_altdf"`, the denominator is derived from
  `.alt_df_full` in combination with `denom_by` argument

- .stats:

  (`character`)  
  statistics to select for the table. See Value for list of available
  statistics.

- .formats:

  (named 'character' or 'list')  
  formats for the statistics.

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

- .labels_n:

  (named `character`)  
  String to control row labels for the 'n'-statistics.  
  Only useful when more than one 'n'-statistic is requested (rare
  situations only).

- extrablankline:

  (`logical`)  
  When `TRUE`, an extra blank line will be added after the last value.  
  Avoid using this in template scripts, use section_div = " " instead
  (once PR for rtables is available)  

- extrablanklineafter:

  (`string`)  
  When the row-label matches the string, an extra blank line will be
  added after that value.

- restr_columns:

  `character`  
  If not NULL, columns not defined in `restr_columns` will be blanked
  out.

- colgroup:

  The name of the column group variable that is used as source for
  denominator calculation.  
  Required to be specified when `denom = "N_colgroup"`.

- exclude_levels:

  (`list`)  
  A named list where names correspond to split variables and values are
  vectors of levels to exclude.

- ...:

  additional arguments for the lower level functions.

## Value

- `s_freq_j`: returns a list of following statistics  

  - n_df

  - n_rowdf

  - n_parentdf

  - n_altdf

  - denom

  - count

  - count_unique

  - count_unique_fraction

  - count_unique_denom_fraction

&nbsp;

- `a_freq_j`: returns a list of requested statistics with formatted
  [`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).  
  Within the relative risk difference columns, the following stats are
  blanked out:

  - any of the n-statistics (n_df, n_altdf, n_parentdf, n_rowdf, denom)

  - count

  - count_unique

  For the others (count_unique_fraction, count_unique_denom_fraction),
  the statistic is replaced by the relative risk difference + confidence
  interval.

## Details

`denom` controls the denominator used to calculate proportions/percents.
It must be one of  

- **N_col** Column count,  

- **n_df** Number of patients (based upon the main input dataframe
  `df`),  

- **n_altdf** Number of patients from the secondary dataframe
  (`.alt_df_full`),  
  Note that argument `denom_by` will perform a row-split on the
  `.alt_df_full` dataframe.  
  It is a requirement that variables specified in `denom_by` are part of
  the row split specifications.  

- **N_colgroup** Number of patients from the column group variable (note
  that this is based upon the input .alt_df_full dataframe).  
  Note that the argument `colgroup` (column variable) needs to be
  provided, as it cannot be retrieved directly from the column layout
  definition.

- **n_rowdf** Number of patients from the current row-level dataframe
  (`.row_df` from the rtables splitting machinery).  

- **n_parentdf** Number of patients from a higher row-level split than
  the current split.  
  This higher row-level split is specified in the argument `denom_by`.  

## Functions

- `a_freq_j_with_exclude()`: Wrapper for the `afun` which can exclude
  row split levels from producing the analysis. These have to be
  specified in the `exclude_levels` argument, see
  [`?do_exclude_split`](https://johnsonandjohnson.github.io/junco/reference/do_exclude_split.md)
  for details.

## Examples

``` r
library(dplyr)

adsl <- ex_adsl |> select("USUBJID", "SEX", "ARM")
adae <- ex_adae |> select("USUBJID", "AEBODSYS", "AEDECOD")
adae[["TRTEMFL"]] <- "Y"

trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
adsl$colspan_trt <- factor(ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)

adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

adae <- adae |> left_join(adsl)
#> Joining with `by = join_by(USUBJID)`

colspan_trt_map <- create_colspan_map(adsl,
  non_active_grp = "B: Placebo",
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

lyt <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
  split_cols_by(trtvar) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp))

lyt1 <- lyt |>
  analyze("TRTEMFL",
    show_labels = "hidden",
    afun = a_freq_j,
    extra_args = list(
      method = "wald",
      .stats = c("count_unique_denom_fraction"),
      ref_path = ref_path
    )
  )

result1 <- build_table(lyt1, adae, alt_counts_df = adsl)

result1
#>            Active Study Agent                                          Risk Difference (%) (95% CI)             
#>        A: Drug X      C: Combination      B: Placebo      A: Drug X vs B: Placebo   C: Combination vs B: Placebo
#>         (N=134)           (N=132)           (N=134)               (N=134)                     (N=132)           
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Y   122/134 (91.0%)   120/132 (90.9%)   123/134 (91.8%)      -0.7 (-7.5, 6.0)             -0.9 (-7.6, 5.9)      

x_drug_x <- list(length(unique(subset(adae, adae[[trtvar]] == "A: Drug X")[["USUBJID"]])))
N_x_drug_x <- length(unique(subset(adsl, adsl[[trtvar]] == "A: Drug X")[["USUBJID"]]))
y_placebo <- list(length(unique(subset(adae, adae[[trtvar]] == ctrl_grp)[["USUBJID"]])))
N_y_placebo <- length(unique(subset(adsl, adsl[[trtvar]] == ctrl_grp)[["USUBJID"]]))

tern::stat_propdiff_ci(
  x = x_drug_x,
  N_x = N_x_drug_x,
  y = y_placebo,
  N_y = N_y_placebo
)
#> [[1]]
#> [1] -0.7462687 -7.4525893  5.9600520
#> 

x_combo <- list(length(unique(subset(adae, adae[[trtvar]] == "C: Combination")[["USUBJID"]])))
N_x_combo <- length(unique(subset(adsl, adsl[[trtvar]] == "C: Combination")[["USUBJID"]]))

tern::stat_propdiff_ci(
  x = x_combo,
  N_x = N_x_combo,
  y = y_placebo,
  N_y = N_y_placebo
)
#> [[1]]
#> [1] -0.8819539 -7.6386167  5.8747089
#> 


extra_args_rr <- list(
  denom = "n_altdf",
  denom_by = "SEX",
  riskdiff = FALSE,
  .stats = c("count_unique")
)

extra_args_rr2 <- list(
  denom = "n_altdf",
  denom_by = "SEX",
  riskdiff = TRUE,
  ref_path = ref_path,
  method = "wald",
  .stats = c("count_unique_denom_fraction"),
  na_str = rep("NA", 3)
)

lyt2 <- basic_table(
  top_level_section_div = " ",
  colcount_format = "N=xx"
) |>
  split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
  split_cols_by(trtvar, show_colcounts = TRUE) |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(trtvar,
    labels_var = "rrisk_label", split_fun = remove_split_levels("B: Placebo"),
    show_colcounts = FALSE
  ) |>
  split_rows_by("SEX", split_fun = drop_split_levels) |>
  summarize_row_groups("SEX",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr, list(label_fstr = "Gender: %s"))
  ) |>
  split_rows_by("TRTEMFL",
    split_fun = keep_split_levels("Y"),
    indent_mod = -1L,
    section_div = c(" ")
  ) |>
  summarize_row_groups("TRTEMFL",
    cfun = a_freq_j,
    extra_args = append(extra_args_rr2, list(
      label =
        "Subjects with >=1 AE", extrablankline = TRUE
    ))
  ) |>
  split_rows_by("AEBODSYS",
    split_label = "System Organ Class",
    split_fun = trim_levels_in_group("AEDECOD"),
    label_pos = "topleft",
    section_div = c(" "),
    nested = TRUE
  ) |>
  summarize_row_groups("AEBODSYS",
    cfun = a_freq_j,
    extra_args = extra_args_rr2
  ) |>
  analyze("AEDECOD",
    afun = a_freq_j,
    extra_args = extra_args_rr2
  )

result2 <- build_table(lyt2, adae, alt_counts_df = adsl)
```
