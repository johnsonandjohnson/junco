# Analysis function count and percentage in column design controlled by combosdf

Analysis function count and percentage in column design controlled by
combosdf

## Usage

``` r
a_freq_combos_j(
  df,
  labelstr = NULL,
  .var = NA,
  val = NULL,
  combosdf = NULL,
  do_not_filter = NULL,
  filter_var = NULL,
  flag_var = NULL,
  .df_row,
  .spl_context,
  .N_col,
  id = "USUBJID",
  denom = c("N_col", "n_df", "n_altdf", "n_rowdf", "n_parentdf"),
  label = NULL,
  label_fstr = NULL,
  label_map = NULL,
  .alt_df_full = NULL,
  denom_by = NULL,
  .stats = "count_unique_denom_fraction",
  .formats = NULL,
  .labels_n = NULL,
  .indent_mods = NULL,
  na_str = rep("NA", 3)
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- labelstr:

  (`character`)  
  label of the level of the parent split currently being summarized
  (must be present as second argument in Content Row Functions). See
  [`rtables::summarize_row_groups()`](https://insightsengineering.github.io/rtables/latest-tag/reference/summarize_row_groups.html)
  for more information.

- .var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

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

- combosdf:

  The df which provides the mapping of column facets to produce
  cumulative counts for .N_col.  
  In the cell facet, these cumulative records must then be removed from
  the numerator, which can be done via the filter_var parameter to avoid
  unwanted counting of events.

- do_not_filter:

  A vector of facets (i.e., column headers), identifying headers for
  which no filtering of records should occur. That is, the numerator
  should contain cumulative counts. Generally, this will be used for a
  "Total" column, or something similar.

- filter_var:

  The variable which identifies the records to count in the numerator
  for any given column. Generally, this will contain text matching the
  column header for the column associated with a given record.

- flag_var:

  Variable which identifies the occurrence (or first occurrence) of an
  event. The flag variable is expected to have a value of "Y"
  identifying that the event should be counted, or NA otherwise.

- .df_row:

  (`data.frame`)  
  data frame across all of the columns for the given row split.

- .spl_context:

  (`data.frame`)  
  gives information about ancestor split states that is passed by
  `rtables`.

- .N_col:

  (`integer`)  
  column-wise N (column count) for the full column being analyzed that
  is typically passed by `rtables`.

- id:

  (`string`)  
  subject variable name.

- denom:

  (`string`)  
  One of  

  - **N_col** Column count,  

  - **n_df** Number of patients (based upon the main input dataframe
    `df`),  

  - **n_altdf** Number of patients from the secondary dataframe
    (`.alt_df_full`),  
    Note that argument `denom_by` will perform a row-split on the
    `.alt_df_full` dataframe.  
    It is a requirement that variables specified in `denom_by` are part
    of the row split specifications.  

  - **n_rowdf** Number of patients from the current row-level dataframe
    (`.row_df` from the rtables splitting machinery).  

  - **n_parentdf** Number of patients from a higher row-level split than
    the current split.  
    This higher row-level split is specified in the argument
    `denom_by`.  

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
  statistics to select for the table.

- .formats:

  (named 'character' or 'list')  
  formats for the statistics.

- .labels_n:

  (named `character`)  
  String to control row labels for the 'n'-statistics.  
  Only useful when more than one 'n'-statistic is requested (rare
  situations only).

- .indent_mods:

  (named `integer`)  
  indent modifiers for the labels. Defaults to 0, which corresponds to
  the unmodified default behavior. Can be negative.

- na_str:

  (`string`)  
  string used to replace all `NA` or empty values in the output.

## Value

list of requested statistics with formatted
[`rtables::CellValue()`](https://insightsengineering.github.io/rtables/latest-tag/reference/CellValue.html).  

## Examples

``` r
library(dplyr)
ADSL <- ex_adsl |> select(USUBJID, ARM, EOSSTT, EOSDT, EOSDY, TRTSDTM)

cutoffd <- as.Date("2023-09-24")

ADSL <- ADSL |>
  mutate(
    TRTDURY = case_when(
      !is.na(EOSDY) ~ EOSDY,
      TRUE ~ as.integer(cutoffd - as.Date(TRTSDTM) + 1)
    )
  ) |>
  mutate(ACAT1 = case_when(
    TRTDURY < 183 ~ "0-6 Months",
    TRTDURY < 366 ~ "6-12 Months",
    TRUE ~ "+12 Months"
  )) |>
  mutate(ACAT1 = factor(ACAT1, levels = c("0-6 Months", "6-12 Months", "+12 Months")))


ADAE <- ex_adae |> select(USUBJID, ARM, AEBODSYS, AEDECOD, ASTDY)

ADAE <- ADAE |>
  mutate(TRTEMFL = "Y") |>
  mutate(ACAT1 = case_when(
    ASTDY < 183 ~ "0-6 Months",
    ASTDY < 366 ~ "6-12 Months",
    TRUE ~ "+12 Months"
  )) |>
  mutate(ACAT1 = factor(ACAT1, levels = c("0-6 Months", "6-12 Months", "+12 Months")))

combodf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "Tot", "Total", c("0-6 Months", "6-12 Months", "+12 Months"), list(),
  "A_0-6 Months", "0-6 Months", c("0-6 Months", "6-12 Months", "+12 Months"), list(),
  "B_6-12 Months", "6-12 Months", c("6-12 Months", "+12 Months"), list(),
  "C_+12 Months", "+12 Months", c("+12 Months"), list()
)


lyt <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("ARM") |>
  split_cols_by("ACAT1",
    split_fun = add_combo_levels(combosdf = combodf, trim = FALSE, keep_levels = combodf$valname)
  ) |>
  analyze("TRTEMFL",
    show_labels = "hidden",
    afun = a_freq_combos_j,
    extra_args = list(
      val = "Y",
      label = "Subjects with >= 1 AE",
      combosdf = combodf,
      filter_var = "ACAT1",
      do_not_filter = "Tot"
    )
  )


result <- build_table(lyt, df = ADAE, alt_counts_df = ADSL)

result
#>                                                     A: Drug X                                                            B: Placebo                                                         C: Combination                          
#>                              Total          0-6 Months      6-12 Months       +12 Months          Total          0-6 Months      6-12 Months       +12 Months          Total          0-6 Months      6-12 Months       +12 Months  
#>                             (N=134)          (N=134)          (N=123)          (N=117)           (N=134)          (N=134)          (N=125)          (N=113)           (N=132)          (N=132)          (N=124)          (N=111)    
#> ————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> Subjects with >= 1 AE   122/134 (91.0%)   96/134 (71.6%)   75/123 (61.0%)   86/117 (73.5%)   123/134 (91.8%)   95/134 (70.9%)   85/125 (68.0%)   87/113 (77.0%)   120/132 (90.9%)   96/132 (72.7%)   77/124 (62.1%)   89/111 (80.2%)
```
