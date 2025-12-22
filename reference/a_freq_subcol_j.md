# Analysis function count and percentage with extra column-subsetting in selected columns (controlled by subcol\_\* arguments)

Analysis function count and percentage with extra column-subsetting in
selected columns (controlled by subcol\_\* arguments)

## Usage

``` r
a_freq_subcol_j(
  df,
  labelstr = NULL,
  .var = NA,
  val = NULL,
  subcol_split = NULL,
  subcol_var = NULL,
  subcol_val = NULL,
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
  .stats = c("count_unique_denom_fraction"),
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

- subcol_split:

  (`string`)  
  text to search colid to determine whether further subsetting should be
  performed.

- subcol_var:

  (`string`)  
  name of variable containing to be searched for the text identified in
  subcol_val argument.

- subcol_val:

  (`string`)  
  value to use to perform further data sub-setting.

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

ADSL <- ex_adsl |>
  select(USUBJID, ARM)

ADSL$COLSPAN_REL <- "AEs"

ADAE <- ex_adae |>
  select(USUBJID, ARM, AEDECOD, AREL)

ADAE <- ADAE |>
  mutate(
    AEREL = case_when(
      AREL == "Y" ~ "RELATED",
      AREL == "N" ~ "NOT RELATED"
    ),
    AEREL = factor(AEREL),
    COLSPAN_REL = "AEs"
  )

combodf <- tribble(
  ~valname,  ~label,        ~levelcombo, ~exargs,
  "RELATED", "Related AEs", c("AEs"),    list()
)

lyt <- basic_table(show_colcounts = TRUE) |>
  split_cols_by("COLSPAN_REL", split_fun = add_combo_levels(combodf, trim = TRUE)) |>
  split_cols_by("ARM") |>
  analyze("AEDECOD",
    afun = a_freq_subcol_j,
    extra_args = list(
      subcol_split = "RELATED",
      subcol_var = "AEREL",
      subcol_val = "RELATED"
    )
  )

result <- build_table(lyt, ADAE, alt_counts_df = ADSL)

result
#>                                       AEs                                            Related AEs                   
#>                   A: Drug X        B: Placebo     C: Combination     A: Drug X        B: Placebo     C: Combination
#>                    (N=134)          (N=134)          (N=132)          (N=134)          (N=134)          (N=132)    
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> dcd A.1.1.1.1   50/134 (37.3%)   45/134 (33.6%)   63/132 (47.7%)    0/134 (0.0%)     0/134 (0.0%)     0/132 (0.0%) 
#> dcd A.1.1.1.2   48/134 (35.8%)   48/134 (35.8%)   50/132 (37.9%)    0/134 (0.0%)     0/134 (0.0%)     0/132 (0.0%) 
#> dcd B.1.1.1.1   47/134 (35.1%)   49/134 (36.6%)   43/132 (32.6%)   47/134 (35.1%)   49/134 (36.6%)   43/132 (32.6%)
#> dcd B.2.1.2.1   49/134 (36.6%)   44/134 (32.8%)   52/132 (39.4%)    0/134 (0.0%)     0/134 (0.0%)     0/132 (0.0%) 
#> dcd B.2.2.3.1   48/134 (35.8%)   54/134 (40.3%)   51/132 (38.6%)    0/134 (0.0%)     0/134 (0.0%)     0/132 (0.0%) 
#> dcd C.1.1.1.3   43/134 (32.1%)   46/134 (34.3%)   43/132 (32.6%)   43/134 (32.1%)   46/134 (34.3%)   43/132 (32.6%)
#> dcd C.2.1.2.1   35/134 (26.1%)   48/134 (35.8%)   55/132 (41.7%)   35/134 (26.1%)   48/134 (35.8%)   55/132 (41.7%)
#> dcd D.1.1.1.1   50/134 (37.3%)   42/134 (31.3%)   51/132 (38.6%)    0/134 (0.0%)     0/134 (0.0%)     0/132 (0.0%) 
#> dcd D.1.1.4.2   48/134 (35.8%)   42/134 (31.3%)   50/132 (37.9%)    0/134 (0.0%)     0/134 (0.0%)     0/132 (0.0%) 
#> dcd D.2.1.5.3   47/134 (35.1%)   58/134 (43.3%)   57/132 (43.2%)   47/134 (35.1%)   58/134 (43.3%)   57/132 (43.2%)
```
